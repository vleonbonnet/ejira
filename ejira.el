;;; ejira.el --- Org-mode interface to JIRA  -*- lexical-binding: t -*-

;; Copyright (C) 2017 - 2022 Henrik Nyman

;; Author: Henrik Nyman
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira
;; Package-Requires: ((emacs "29.1") (org-sync-confirm "0.1"))
;; Package-Version: 1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO:
;; - Sprint handling
;; - Attachments

;;; Code:

(require 'org)
(require 'dash)
(require 'ejira-core)
(require 'ejira-confirm)
(require 'ejira-push)

(defvar ejira--sync-in-progress)
(defvar ejira--auto-sync-round)
(defvar ejira-auto-pull-interval)




(defvar ejira-update-jql-resolved-fn #'ejira-jql-all-resolved-project-tickets
  "Generates JQL used in `ejira-update-project' to find server-resolved items.
Must take a project-id as a string, a list of keys, and return JQL as a
string.")

(defvar ejira-update-jql-unresolved-multi-fn
  #'ejira-jql-all-unresolved-multi-project-tickets
  "Generates JQL to find unresolved items for a list of project IDs.
Used by both `ejira-update-my-projects' (full list) and `ejira-update-project'
(single-element list). Must take a list of project-id strings and return JQL.")


(defun ejira-delete-comment ()
  "Stage a comment deletion. Confirmed via ejira-confirm on next save."
  (interactive)
  (let* ((item (ejira-get-id-under-point "ejira-comment"))
         (id (nth 1 item)))
    (when (y-or-n-p (format "Stage comment %s for deletion? " (cdr id)))
      (org-with-point-at (nth 2 item)
        (org-set-property "PendingDelete" "t"))
      (message "ejira: comment deletion staged — save buffer to confirm."))))

(defun ejira-jql-all-unresolved-multi-project-tickets (project-ids)
  "Default multi-project JQL for `ejira-update-jql-unresolved-multi-fn'.
Returns the open tickets across all PROJECT-IDS: those not in a terminal
status (status category Done).  The resolution field is not used: a
workflow can end an issue without setting one."
  (format "project in (%s) and statusCategory != Done"
          (s-join ", " (mapcar (lambda (p) (format "'%s'" p)) project-ids))))

(defun ejira-jql-all-resolved-project-tickets (project-id keys)
  "Builds JQL for server-resolved project tickets in PROJECT-ID from local KEYS.
This is the function used in `ejira-update-project'. Override with
`ejira-update-jql-resolved-fn'."
  (format "project = '%s' and key in (%s) and statusCategory = Done"
          project-id (s-join ", " keys)))


(defun ejira--auth-header ()
  "Return the HTTP Authorization header cons cell for the active auth mode."
  (cond ((eq jiralib2-auth 'cookie)
         `("cookie" . ,jiralib2--session))
        ((eq jiralib2-auth 'bearer)
         `("Authorization" . ,(format "Bearer %s" jiralib2--session)))
        (t
         `("Authorization" . ,(format "Basic %s" jiralib2--session)))))

;;; Asynchronous requests

(defcustom ejira-request-timeout 60
  "Seconds before an asynchronous Jira request is abandoned as failed.
A request without a deadline that never answers would keep its sync
waiting forever."
  :group 'ejira
  :type 'integer)

(defcustom ejira-max-concurrent-requests 6
  "Maximum number of asynchronous Jira requests in flight at once.
Further requests wait in a queue.  Each request runs its own curl
process."
  :group 'ejira
  :type 'integer)

(defvar ejira--request-queue nil
  "Asynchronous requests waiting for a free slot, oldest first.
Each is the argument list of `ejira--request-send'.")

(defvar ejira--requests-active 0
  "Number of asynchronous Jira requests in flight.")

(defun ejira--request-async (path type data on-success on-error)
  "Send the Jira REST request PATH in the background.
TYPE is the HTTP method and DATA the JSON body, or nil.  ON-SUCCESS is
called with the parsed answer, ON-ERROR with an error object, (error
MESSAGE): when the request fails, times out (`ejira-request-timeout')
or answers with an HTTP error.  Exactly one of them is called, from
the request's process sentinel; an error they signal is reported and
does not stop the queue.  At most `ejira-max-concurrent-requests' run
at once."
  (unless jiralib2--session (jiralib2-session-login))
  (setq ejira--request-queue
        (nconc ejira--request-queue
               (list (list path type data on-success on-error))))
  (ejira--request-pump))

(defun ejira--request-pump ()
  "Send queued requests while fewer than the maximum are in flight."
  (while (and ejira--request-queue
              (< ejira--requests-active ejira-max-concurrent-requests))
    (let ((args (pop ejira--request-queue)))
      (cl-incf ejira--requests-active)
      (condition-case err
          (apply #'ejira--request-send args)
        (error
         (cl-decf ejira--requests-active)
         (funcall (nth 4 args)
                  (list 'error (format "Jira request %s failed: %s"
                                       (car args) (error-message-string err)))))))))

(defun ejira--request-send (path type data on-success on-error)
  "Send the request of `ejira--request-async' (PATH TYPE DATA ON-SUCCESS ON-ERROR)."
  (request (concat jiralib2-url path)
    :type type
    :headers `(("Content-Type" . "application/json")
               ,(ejira--auth-header))
    :data data
    :timeout ejira-request-timeout
    :parser (lambda () (let ((json-array-type 'list)) (json-read)))
    ;; One callback for every outcome: `request' calls `:complete' after
    ;; `:success' or `:error', but not when one of those signals, and
    ;; the slot must be released whatever happens.
    :complete
    (cl-function
     (lambda (&key data symbol-status error-thrown response &allow-other-keys)
       (cl-decf ejira--requests-active)
       (unwind-protect
           (with-demoted-errors "ejira: request callback failed: %S"
             (if (eq symbol-status 'success)
                 (funcall on-success data)
               (funcall on-error
                        (list 'error
                              (format "Jira request %s failed (%s%s): %s"
                                      path symbol-status
                                      (if-let* ((code (and response
                                                           (request-response-status-code
                                                            response))))
                                          (format ", HTTP %s" code)
                                        "")
                                      (pcase error-thrown
                                        (`(,_ . ,(and msg (pred stringp))) msg)
                                        (`(,_ . ,_) (error-message-string error-thrown))
                                        (_ error-thrown)))))))
         (ejira--request-pump))))))

(defun ejira--async-jql (jql fields success-fn &optional error-fn)
  "Run JQL asynchronously, fetching all pages; call SUCCESS-FN with full list.
FIELDS are the fields to fetch.  ERROR-FN, when given, is called with
the error of a failed page instead; see `ejira--request-async'."
  ;; Drop any nil-named fields (ejira-epic-field / ejira-sprint-field unset).
  (let ((fields (cl-remove-if (lambda (f) (or (null f) (equal f "nil"))) fields))
        (results '()))
    (cl-labels ((fetch (start)
                  (ejira--request-async
                   "/rest/api/2/search" "POST"
                   (json-encode `((jql . ,jql)
                                  (startAt . ,start)
                                  (maxResults . 1000)
                                  (fields . ,fields)))
                   (lambda (data)
                     (let ((page (alist-get 'issues data))
                           (total (alist-get 'total data)))
                       (setq results (append results page))
                       ;; An empty page ends the search even short of
                       ;; TOTAL: asking again would loop forever.
                       (if (and page total (< (length results) total))
                           (fetch (length results))
                         (funcall success-fn results))))
                   (lambda (err)
                     (if error-fn
                         (funcall error-fn err)
                       (message "ejira: JQL error: %s" (error-message-string err)))))))
      (fetch 0))))

(defvar ejira--trace-file nil
  "When set to a file path, `ejira--apply-sync' appends timestamped phase
traces there.  Debugging aid for locating where a sync stalls; nil disables
it.")

(defun ejira--trace (fmt &rest args)
  "Append a timestamped FMT/ARGS line to `ejira--trace-file' when set."
  (when ejira--trace-file
    (write-region (concat (format-time-string "%H:%M:%S.%3N ")
                          (apply #'format fmt args) "\n")
                  nil ejira--trace-file 'append 'silent)))

(defun ejira--apply-sync (projects unresolved-items resolved-items shallow)
  "Apply UNRESOLVED-ITEMS and RESOLVED-ITEMS to org files for PROJECTS.
Called from async callbacks once all network responses have arrived."
  (unwind-protect
      ;; One sync operation: a buffer whose file changed on disk under it
      ;; is reloaded first, and a save refused mid-sync is settled at the
      ;; end (see `ejira--with-transaction') instead of prompting,
      ;; overwriting the external change or leaving the buffer unsaved.
      (ejira--with-transaction
        (let ((ejira--syncing t)
              (ejira--heading-cache (make-hash-table :test 'equal))
              (ejira--shallow-only shallow)
              (ejira--deferred-keys nil)
              (save-silently t)
              (message-log-max nil))
          (ejira--trace "START unresolved=%d resolved=%d" (length unresolved-items) (length resolved-items))
          ;; org-id-locations is kept current by `org-id-track-globally' on every
          ;; save; a full `org-id-update-id-locations' rescan of every org-id file
          ;; (~90 files, each triggering org-indent-refresh-maybe →
          ;; org-element--parse-to per line) was the single biggest hotspot
          ;; (66% of profiler samples).  Skipped entirely — if a refiled issue
          ;; isn't found by ejira--find-heading, ejira--update-task-light falls
          ;; back to ejira--update-task which creates a new heading.
          ;; Save fold state (char positions, no markers — survives revert of unmodified buffers).
          (let ((vis-saves
                 (delq nil
                       (mapcar (lambda (id)
                                 (let ((path (expand-file-name (ejira--project-file-name id))))
                                   (when-let* ((buf (find-buffer-visiting path)))
                                     (with-current-buffer buf
                                       (cons buf (org-fold-core-get-regions))))))
                               projects))))
            ;; Revert unmodified buffers so content matches disk; expand all headings
            ;; so ejira--with-expand-all is a no-op inside the update loop.
            (dolist (id projects)
              (with-current-buffer (find-file-noselect
                                    (expand-file-name (ejira--project-file-name id)) t)
                (when (and (not (buffer-modified-p)) (file-exists-p buffer-file-name))
                  (revert-buffer t t t))
                (outline-show-all)))
            (ejira--trace "after revert+expand, starting loop")
            ;; Process all fetched items.  ejira--update-task-light and
            ;; ejira--normalize-end-spacing both guard against no-op writes
            ;; (comparing current values before calling org-set-property, skipping
            ;; delete+insert when spacing is already correct), so a sync where
            ;; nothing changed makes zero buffer modifications and triggers zero
            ;; after-change-functions.
            (let ((update-fn (if shallow
                                 (lambda (i)
                                   (ejira--update-task-light
                                    (ejira--alist-get i 'key)
                                    (ejira--alist-get i 'fields 'status 'name)
                                    (ejira--alist-get i 'fields 'assignee 'displayName)
                                    (ejira--alist-get i 'fields 'resolution 'name)))
                               #'ejira--update-task-or-hold)))
              (mapc update-fn unresolved-items)
              (mapc update-fn resolved-items))
            (ejira--trace "after loop")
            ;; Shallow syncs skip unknown keys rather than escalating; surface
            ;; them so an explicit full sync can pick them up.
            (when ejira--deferred-keys
              (ejira--trace "deferred %d key(s): %s" (length ejira--deferred-keys)
                            (s-join ", " ejira--deferred-keys))
              (message "ejira: %d issue(s) skipped, need a full sync: %s"
                       (length ejira--deferred-keys)
                       (s-join ", " (seq-take ejira--deferred-keys 5))))
            ;; Normalize: ensure exactly one blank line after every :END: closer.
            ;; No-ops when spacing is already correct — see ejira--normalize-end-spacing.
            (dolist (id projects)
              (when-let* ((buf (find-buffer-visiting
                                (expand-file-name (ejira--project-file-name id)))))
                (with-current-buffer buf
                  (ejira--normalize-end-spacing))))
            (ejira--trace "after normalize")
            ;; Save all buffers touched during sync, not just project files.
            ;; Headings refiled into other org files are found via
            ;; org-id-find-id-in-file and updated in-place; those buffers must
            ;; be saved too or the sync leaves them dirty.
            (let ((touched
                   (delq nil
                         (cl-remove-duplicates
                          (cl-loop for m being the hash-values of ejira--heading-cache
                                   when (and (markerp m) (marker-buffer m))
                                   collect (marker-buffer m))
                          :test #'eq))))
              (dolist (id projects)
                (when-let* ((buf (find-buffer-visiting
                                  (expand-file-name (ejira--project-file-name id)))))
                  (cl-pushnew buf touched :test #'eq)))
              (dolist (buf touched)
                (with-current-buffer buf
                  (ejira--save-buffer-safe))))
            (ejira--trace "after save")
            ;; Restore fold state for any buffer that was open before the sync.
            (dolist (entry vis-saves)
              (with-current-buffer (car entry)
                (org-fold-core-regions (cdr entry) :override t)))
            (ejira--trace "after fold-restore"))))
    (setq ejira--sync-in-progress nil)
    ;; Cycles that waited for the pull to end can run now.
    (ejira--auto-sync-kick)
    (message "ejira: sync finished")))

(defun ejira-pull-item-under-point ()
  "Replace the issue, project or comment under point with Jira's version.
Pending local edits of an issue are discarded: this is how a held issue
whose local copy is stale is resolved without pushing it."
  (interactive)
  (let* ((ejira--discard-local-edits t)
         (item (ejira-get-id-under-point))
         (id (nth 1 item))
         (type (nth 0 item)))
    (cond ((equal type "ejira-comment")
           (ejira--update-comment
            (car id) (ejira--parse-comment (jiralib2-get-comment (car id) (cdr id)))))
          ((equal type "ejira-project")
           (ejira--update-project id))
          (t
           (ejira--update-task id)))))


(defun ejira-push-item-under-point ()
  "Push the ejira item at point through ejira-confirm."
  (interactive)
  (ejira-push-at-point))

(defun ejira-browse-issue-under-point ()
  "Open the current issue in external browser."
  (interactive)
  (browse-url (concat (replace-regexp-in-string "/*$" "" jiralib2-url) "/browse/" (ejira-issue-id-under-point))))



(defun ejira-heading-to-task (_focus)
  "Mark the current heading as a pending Jira issue creation.
Ensure it has a TODO state; save the buffer to stage creation via
ejira-confirm.
FOCUS is accepted for compatibility but has no effect until confirmed."
  (interactive "P")
  (unless (org-get-todo-state)
    (org-todo (car org-todo-keywords-1)))
  (message "ejira: heading marked as pending issue — save buffer to stage."))

(defun ejira-heading-to-subtask (_focus)
  "Mark the current heading as a pending Jira subtask creation.
Save the buffer to stage creation via ejira-confirm."
  (interactive "P")
  (unless (org-get-todo-state)
    (org-todo (car org-todo-keywords-1)))
  (message "ejira: heading marked as pending subtask — save buffer to stage."))

(defun ejira--local-todo-keys (projects)
  "Return keys of local ejira TODO headings belonging to PROJECTS.
Uses `org-id-locations' to find which files contain ejira issue keys
for these projects, then scans those files for TODO headings.

Scanning uses `re-search-forward' for headline patterns — org-fold
only hides subtree *content*, never the headline lines themselves,
so the search reaches all headings regardless of fold state without
calling `outline-show-all'.  This avoids both fold disruption and
the cost of a full `org-element-parse-buffer' AST (which parses every
bold, italic, link, and timestamp object in the buffer when we only
need the :ID property and TODO keyword per headline)."
  (let* ((prefixes (mapcar (lambda (id) (concat id "-")) projects))
         (key-pred (lambda (id)
                     (and id
                          (cl-some (lambda (p) (string-prefix-p p id)) prefixes))))
         (files (cl-remove-duplicates
                 (delq nil
                       (cl-loop for id being the hash-keys of org-id-locations
                                when (funcall key-pred id)
                                collect (gethash id org-id-locations)))
                 :test #'equal))
         keys)
    (dolist (file files)
      (let ((buf (find-file-noselect file t)))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*\\{1,4\\} " nil t)
             (let ((id (org-entry-get (point) "ID"))
                   (todo (org-get-todo-state)))
               (when (and (funcall key-pred id)
                          todo
                          (not (member todo org-done-keywords)))
                 (push id keys))))))))
    (nreverse keys)))

(defun ejira-update-project (id &optional shallow)
  "Update all issues in project ID.
If DEEP set to t, update each issue with separate API call which pulls also
comments. With SHALLOW, only update todo status and assignee."
  (setq ejira--sync-in-progress t)
  (unwind-protect
      (progn
        (ejira--update-project id)

        ;; Expand the project buffer once so ejira--with-expand-all becomes a no-op
        ;; inside the sync loop instead of save/restoring outline visibility per op.
        (ejira--with-transaction
          (let* ((ejira--syncing t)
         (ejira--heading-cache (make-hash-table :test 'equal))
         (proj-buf (find-file-noselect
                            (expand-file-name (ejira--project-file-name id))))
         (vis-save (with-current-buffer proj-buf (org-fold-core-get-regions))))
            (with-current-buffer proj-buf (outline-show-all))

            ;; First, update all items that are marked as unresolved.
            ;;
            ;; Handles cases:
            ;; *local*    | *remote*
            ;; ===========+===========
            ;;            | unresolved
            ;; unresolved | unresolved
            ;; resolved   | unresolved
            ;;
            (mapc (lambda (i) (if shallow
                                  (ejira--update-task-light
                                   (ejira--alist-get i 'key)
                                   (ejira--alist-get i 'fields 'status 'name)
                                   (ejira--alist-get i 'fields 'assignee 'displayName)
                                   (ejira--alist-get i 'fields 'resolution 'name))
                (ejira--update-task-or-hold i)))
                  (apply #'jiralib2-jql-search
             (funcall ejira-update-jql-unresolved-multi-fn (list id))
             (ejira--get-fields-to-sync shallow)))

            ;; Then, sync any items that are still marked as unresolved in our local sync,
            ;; but are already resolved at the server. This should ensure that there are
            ;; no hanging todo items in our local sync.
            ;;
            ;; Scans files from `org-id-locations' (not just this project's canonical
            ;; sync file) so issues refiled elsewhere are still caught — see
            ;; `ejira--local-todo-keys'.
            ;;
            ;; Handles cases:
            ;; *local*    | *remote*
            ;; ===========+===========
            ;; unresolved | resolved
            ;;
            (let ((keys (ejira--local-todo-keys (list id))))
              (when keys
        (mapc (lambda (i) (if shallow
                                      (ejira--update-task-light
                                       (ejira--alist-get i 'key)
                                       (ejira--alist-get i 'fields 'status 'name)
                                       (ejira--alist-get i 'fields 'assignee 'displayName)
                                       (ejira--alist-get i 'fields 'resolution 'name))
                                    (ejira--update-task-or-hold i)))
                      (apply #'jiralib2-jql-search
                             (funcall ejira-update-jql-resolved-fn id keys)
                             (ejira--get-fields-to-sync shallow)))))

            ;; TODO: Handle issue being deleted from server:
            ;; *local*    | *remote*
            ;; ===========+===========
            ;; unresolved |
            ;; resolved   |

            ;; Normalize spacing on the project buffer.
            (when-let* ((buf (find-buffer-visiting
                             (expand-file-name (ejira--project-file-name id)))))
              (with-current-buffer buf
        (ejira--normalize-end-spacing)))
            ;; Save all buffers touched during sync, not just the project file.
            (let ((touched
                   (delq nil
             (cl-remove-duplicates
                          (cl-loop for m being the hash-values of ejira--heading-cache
                                   when (and (markerp m) (marker-buffer m))
                                   collect (marker-buffer m))
                          :test #'eq))))
              (when-let* ((buf (find-buffer-visiting
                               (expand-file-name (ejira--project-file-name id)))))
        (cl-pushnew buf touched :test #'eq))
              (dolist (buf touched)
        (with-current-buffer buf
                  (ejira--save-buffer-safe))))
            ;; Restore fold state on the project buffer.
            (when-let* ((buf (find-buffer-visiting
                             (expand-file-name (ejira--project-file-name id)))))
              (with-current-buffer buf
        (org-fold-core-regions vis-save :override t))))))
    (setq ejira--sync-in-progress nil)))

;;;###autoload
(defun ejira-update-my-projects (&optional shallow)
  "Synchronize data on projects listed in `ejira-projects'.
With prefix argument SHALLOW, update only the todo state and assignee.
Fires one combined unresolved JQL and one combined resolved JQL in
parallel, then applies all updates synchronously when both arrive."
  (interactive "P")
  (let* ((projects ejira-projects)
         (fields (ejira--get-fields-to-sync shallow))
         ;; Collect local TODO keys via `org-id-locations' (see
         ;; `ejira--local-todo-keys') before firing network requests.
         ;; This is a fast local scan, no network needed.
         (local-todo-keys
          (ejira--local-todo-keys projects))
         ;; Mutable state shared between the two async callbacks.
         (pending 0)
         (all-unresolved nil)
         (all-resolved nil))
    (if (not projects)
        (message "ejira: no projects configured — set `ejira-projects'")
      (setq ejira--sync-in-progress t)
      (cl-labels
          ((maybe-apply ()
             (when (= pending 0)
               ;; Defer onto a zero-delay timer so the (long, synchronous) apply
               ;; runs in the command loop where C-g works.  The request.el
               ;; success callback fires from the curl process sentinel, where
               ;; `inhibit-quit' is t — running apply-sync there makes it
               ;; uninterruptible, so any slowness becomes a hard freeze.
               (run-at-time 0 nil #'ejira--apply-sync
                            projects all-unresolved all-resolved shallow))))
        (message "ejira: fetching...")
        ;; Fire unresolved query for all projects in one round-trip.
        (cl-incf pending)
        (ejira--async-jql
         (funcall ejira-update-jql-unresolved-multi-fn projects) fields
         (lambda (items) (setq all-unresolved items) (cl-decf pending) (maybe-apply))
         (lambda (err) (message "ejira: unresolved fetch failed: %s" (error-message-string err)) (cl-decf pending) (maybe-apply)))
        ;; Fire resolved-check query in parallel if there are any local TODOs.
        ;; This finds tickets that are TODO in org but already closed on Jira.
        ;; Uses statusCategory = Done to catch all terminal-status issues,
        ;; including those with null resolution (e.g. Cancelled).
        (if local-todo-keys
            (progn
              (cl-incf pending)
              (ejira--async-jql
               (format "key in (%s) and statusCategory = Done"
                       (s-join ", " local-todo-keys))
               fields
               (lambda (items) (setq all-resolved items) (cl-decf pending) (maybe-apply))
               (lambda (_err) (cl-decf pending) (maybe-apply))))
          ;; No local TODOs, nothing to check.
          (maybe-apply))))))


;;; Auto-pull

(defcustom ejira-auto-pull-interval nil
  "Seconds between automatic pulls from Jira, or nil to disable.
When set, ejira periodically pulls changes on a timer and also
pulls when switching to an ejira buffer if the interval has
elapsed.  Auto-pull is read-only — it never triggers a push."
  :group 'ejira
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil)))

(defcustom ejira-auto-pull-shallow t
  "When non-nil, auto-pull uses shallow updates (status + assignee only).
When nil, auto-pull performs full updates including comments and
descriptions.  Shallow updates are faster and sufficient for
tracking status changes."
  :group 'ejira
  :type 'boolean)

(defvar ejira--auto-pull-timer nil
  "Timer for periodic auto-pull, or nil when not running.")

(defvar ejira--last-pull-time nil
  "Time of the last pull (from `current-time').
Used by auto-pull to avoid pulling more often than `ejira-auto-pull-interval'.")

(defvar ejira--sync-in-progress nil
  "Non-nil while a sync cycle is in progress.
Set at the start of `ejira-update-my-projects' or `ejira-update-project'
and cleared when `ejira--apply-sync' (or the synchronous update) finishes.
Guards auto-pull against re-entrancy.")

(defun ejira--auto-pull-due-p ()
  "Return non-nil if enough time has elapsed since the last pull."
  (or (null ejira--last-pull-time)
      (> (float-time (time-subtract (current-time) ejira--last-pull-time))
         ejira-auto-pull-interval)))

(defun ejira--any-ejira-buffer-p ()
  "Return non-nil if any live buffer is visiting an ejira org file."
  (let ((ejira-dir (file-truename (expand-file-name ejira-org-directory))))
    (cl-some (lambda (b)
               (and (buffer-live-p b)
                    (with-current-buffer b
                      (and (derived-mode-p 'org-mode)
                           buffer-file-name
                           (string-prefix-p ejira-dir
                                            (file-truename buffer-file-name))))))
             (buffer-list))))

(defun ejira--auto-pull ()
  "Pull from Jira if not already syncing or pushing.
Calls `ejira-update-my-projects' with `ejira-auto-pull-shallow'."
  (when (and ejira-auto-pull-interval
             (not ejira--sync-in-progress)
             ;; A pull racing a push applies a fetched response over
             ;; newer pushed state; serialize the paths.
             (not (and (boundp 'ejira--pushing) ejira--pushing))
             (not (and (boundp 'ejira--syncing) ejira--syncing))
             ;; Nor an automatic reconciliation waiting on Jira: its
             ;; cycles would restart on the pull's edits.
             (not ejira--auto-sync-round)
             ejira-projects)
    (setq ejira--last-pull-time (current-time))
    (message "ejira: auto-pull started")
    (ejira-update-my-projects ejira-auto-pull-shallow)))

(defun ejira--auto-pull-timer-fn ()
  "Timer callback for periodic auto-pull."
  (when (and ejira-auto-pull-interval
             (ejira--any-ejira-buffer-p))
    (ejira--auto-pull)))

(defun ejira--on-window-buffer-change (frame)
  "Auto-pull when switching to an ejira buffer.
Added to `window-buffer-change-functions' by `ejira--start-auto-pull'."
  (when ejira-auto-pull-interval
    (let ((buf (window-buffer (frame-selected-window frame))))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (derived-mode-p 'org-mode)
                        buffer-file-name
                        (string-prefix-p (file-truename
                                          (expand-file-name ejira-org-directory))
                                         (file-truename buffer-file-name))))
                 (not ejira--sync-in-progress)
                 (ejira--auto-pull-due-p))
        (ejira--auto-pull)))))

(defun ejira--start-auto-pull ()
  "Set up auto-pull timer and buffer-switch hook."
  (when (and ejira-auto-pull-interval (not ejira--auto-pull-timer))
    (setq ejira--auto-pull-timer
          (run-with-timer ejira-auto-pull-interval
                          ejira-auto-pull-interval
                          #'ejira--auto-pull-timer-fn))
    (add-hook 'window-buffer-change-functions #'ejira--on-window-buffer-change)
    (message "ejira: auto-pull enabled (every %ds)" ejira-auto-pull-interval)))

(defun ejira--stop-auto-pull ()
  "Tear down auto-pull timer and buffer-switch hook."
  (when ejira--auto-pull-timer
    (cancel-timer ejira--auto-pull-timer)
    (setq ejira--auto-pull-timer nil))
  (remove-hook 'window-buffer-change-functions #'ejira--on-window-buffer-change))

;;; Automatic reconciliation (auto-sync)

(defcustom ejira-auto-sync-files nil
  "Extra Org files that reconcile automatically when saved or changed.
With `ejira-auto-sync-tracked' (the default) every tracked file already
reconciles; list here only files ejira would not otherwise track.
A save of (or external change to) one of these files schedules a full
reconciliation cycle: remote-only changes are pulled and applied, and
every local change is held for the review buffer -- nothing is ever
sent to Jira without confirmation.  Issues changed on both sides since
their last acknowledgment are held as conflicts and reported in the
`*ejira sync log*' buffer.  Files not listed keep the save-time
push-review behavior.  List canonical paths only: git worktrees must
not independently publish their own copy of a managed file."
  :group 'ejira
  :type '(repeat file))

(defvar ejira-auto-sync-create nil
  "Obsolete: the automatic sync never pushes anything by itself.
Creations, like every other local change, wait for the review buffer.")
(make-obsolete-variable
 'ejira-auto-sync-create
 "every local change, creations included, waits for the review buffer."
 "2026-10-02")

(defcustom ejira-auto-sync-interval 3
  "Idle seconds between automatic reconciliation queue processing."
  :group 'ejira
  :type 'integer)

(defvar ejira--auto-sync-queue nil
  "Files awaiting an automatic reconciliation cycle, newest last.")
(defvar ejira--auto-sync-timer nil)
(defvar ejira--auto-sync-mtimes (make-hash-table :test 'equal)
  "Last seen modification times of `ejira-auto-sync-files'.")
(defvar ejira--auto-sync-attempts (make-hash-table :test 'equal)
  "Consecutive failed reconcile attempts per file, for retry capping.")
(defcustom ejira-auto-sync-retry-limit 3
  "Consecutive failed attempts per file before auto-sync gives up and
reports, instead of retrying on every idle cycle.  The next successful
save or external change resets the counter."
  :type 'integer
  :group 'ejira)

(defvar ejira--auto-sync-review-queue nil
  "Auto-sync files whose next cycle must open the review buffer.
Filled by a save in Emacs and by `ejira-auto-sync-review'; a cycle
caused by an external change (another editor, git, an agent) only
counts its held items in the mode line.")

(defvar ejira--auto-sync-review-plans nil
  "Plans the running cycle held for review, bound by the cycle.")

(defcustom ejira-auto-sync-full-fetch-interval 3600
  "Seconds between the cycles of a file that fetch every issue from Jira.
In between, a cycle first asks Jira only for the update time of each
issue, and fetches the issues whose time moved since their remote
baseline (`Modified', see `ejira-remote-hash-property'), the issues
with local changes and those without a baseline.  The time is stored to
the second, so two Jira changes within the same second as the stored
one wait for the next full fetch.  A full fetch also catches
a change Jira made without moving the update time.  The first cycle of
each file in a session is full, and so is `ejira-auto-sync-review'
with a prefix argument.  nil means only those."
  :group 'ejira
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Only at first and on request" nil)))

(defconst ejira--auto-sync-batch-size 50
  "Number of issues fetched by one Jira search of an automatic cycle.")

(defvar ejira--auto-sync-full-fetch-times (make-hash-table :test 'equal)
  "Map an auto-sync file to the `float-time' of its last full fetch.")

(defvar ejira--auto-sync-full-queue nil
  "Auto-sync files whose next cycle must fetch every issue.")

(defvar ejira--auto-sync-report-pending nil
  "Non-nil when the next round reports its outcome when it ends.
Set by `ejira-auto-sync-review'.")

(cl-defstruct (ejira--cycle (:constructor ejira--cycle-make) (:copier nil))
  "One reconciliation cycle of an auto-sync file."
  (file nil :documentation "The file's true name.")
  (review nil :documentation "Non-nil: what the cycle holds is offered for review.")
  (full nil :documentation "Non-nil: fetch every issue, not only those Jira changed.")
  (audit nil :documentation "Non-nil: a read-only audit; nothing is applied.")
  (round nil :documentation "The `ejira--round' running the cycle, or nil.")
  (mtime nil :documentation "The file's modification time when the cycle started.")
  (restarts 0 :documentation "Times the buffer changed during the fetch.")
  (plans nil :documentation "Plans the apply step held for review.")
  (ended nil :documentation "Non-nil once the cycle has ended."))

(cl-defstruct (ejira--round (:constructor ejira--round-make) (:copier nil))
  "The cycles of several auto-sync files, started together."
  (pending nil :documentation "Files whose cycle has not ended.")
  (plans nil :documentation "Review plans of the ended cycles, in order.")
  (notes nil :documentation "Why files were left unsynced, for the report.")
  (report nil :documentation "Non-nil: tell the outcome when the round ends.")
  (started nil :documentation "`float-time' when the round started."))

(defvar ejira--auto-sync-round nil
  "The running `ejira--round', or nil.
A round starts only when the previous one has ended.")

(defun ejira--auto-sync-full-due-p (file)
  "Return non-nil when the next cycle of FILE must fetch every issue."
  (let ((last (gethash file ejira--auto-sync-full-fetch-times)))
    (or (null last)
        (member file ejira--auto-sync-full-queue)
        (and ejira-auto-sync-full-fetch-interval
             (> (- (float-time) last) ejira-auto-sync-full-fetch-interval)))))

(defun ejira--auto-sync-enqueue (file &optional review)
  "Schedule FILE for an automatic reconciliation cycle.
With REVIEW, the cycle opens the review buffer for what it holds.
Callers decide that FILE auto-syncs (`ejira--auto-sync-file-p')."
  (progn
    (cl-pushnew (file-truename file) ejira--auto-sync-queue
                :test #'equal)
    (when review
      (cl-pushnew (file-truename file) ejira--auto-sync-review-queue
                  :test #'equal))))

(defun ejira--buffer-issue-keys ()
  "Return the issue keys of all ejira-managed headings in the buffer.
Unlike `ejira--local-todo-keys' this covers every outline depth and
also terminal issues (DONE/CANCELED), which can still carry local
pushes; project headings and comments are excluded.  So are issues
of projects outside `ejira-projects': ejira does not sync them."
  (let (keys)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-heading-regexp nil t)
       (let ((id (org-entry-get nil "ID")))
         (when (and id
                    (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" id)
                    (ejira--synced-key-p id)
                    (not (equal (org-entry-get nil "TYPE") "ejira-comment")))
           (push id keys)))))
    (nreverse (delete-dups keys))))

(defun ejira--buffer-issue-baselines ()
  "Return the remote baselines of the issue headings in the buffer.
One (KEY . PLIST) per key of `ejira--buffer-issue-keys', in buffer
order, for the first heading carrying KEY.  PLIST holds :hash, the
`RemoteHash'; :updated, the `Modified' dating it; :dirty, non-nil when
the heading has local changes to push; and :comments-dirty, non-nil
when one of its comments has."
  (let (res comment-keys)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-heading-regexp nil t)
       (let ((id (org-entry-get nil "ID"))
             (type (org-entry-get nil "TYPE")))
         (cond
          ;; An edited comment is pushed against its issue's comment page.
          ((equal type "ejira-comment")
           (when (and (org-entry-get nil "CommId")
                      (ejira--locally-modified-p))
             (push (org-entry-get nil "ID" t) comment-keys)))
          ((and id
                (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" id)
                (ejira--synced-key-p id)
                (not (assoc id res)))
           (push (cons id (list :hash (org-entry-get nil ejira-remote-hash-property)
                                :updated (org-entry-get nil "Modified")
                                :dirty (and (ejira--locally-modified-p) t)))
                 res))))))
    (dolist (entry res)
      (when (member (car entry) comment-keys)
        (setcdr entry (plist-put (cdr entry) :comments-dirty t))))
    (nreverse res)))

;;; Held-issue indicator

(defcustom ejira-auto-sync-mode-line t
  "When non-nil, show the number of held auto-sync items in the mode line.
Held items are local changes awaiting review, and issues that cannot
sync (changed on both sides, without a remote baseline, blocked, or
with unconvertible remote markup).  They only appear in the
`*ejira sync log*' buffer otherwise, which is easy never to look at."
  :group 'ejira
  :type 'boolean)

(defvar ejira--auto-sync-held (make-hash-table :test 'equal)
  "Map an auto-sync file to the held items of its last cycle.
Each item is (KIND . LINE).  KIND is `review' for a change the review
buffer can push, and `blocked' for an item no push can release: only a
fix by hand does (a creation of unknown outcome, a duplicate identity,
unconvertible remote markup).")

(defcustom ejira-auto-sync-indicator-hook nil
  "Normal hook run when the auto-sync mode-line indicator changed.
The indicator is computed when the mode line is redisplayed.  A mode
line that renders `global-mode-string' ahead of redisplay and caches
the result needs to know when to render it again: add its refresh
function here."
  :group 'ejira
  :type 'hook)

(defvar ejira--auto-sync-indicator-state nil
  "The indicator inputs last announced by `ejira-auto-sync-indicator-hook'.")

(defun ejira--auto-sync-indicator-update ()
  "Redisplay the auto-sync indicator when its inputs changed.
Its inputs are the held counts, the buffers conflicting with their
file, and whether a round waits on Jira.  Run
`ejira-auto-sync-indicator-hook' on a change."
  (let ((state (list (ejira-auto-sync-held-count 'review)
                     (ejira-auto-sync-held-count 'blocked)
                     (mapcar #'buffer-name (ejira--disk-conflict-buffers))
                     (and ejira--auto-sync-round t))))
    (unless (equal state ejira--auto-sync-indicator-state)
      (setq ejira--auto-sync-indicator-state state)
      (force-mode-line-update t)
      (run-hooks 'ejira-auto-sync-indicator-hook))))

(defun ejira--auto-sync-record-held (file items)
  "Record ITEMS, a list of (KIND . LINE), as FILE's held items.
See `ejira--auto-sync-held' for the kinds."
  (puthash file items ejira--auto-sync-held)
  (ejira--auto-sync-indicator-update))

(defun ejira-auto-sync-held-count (&optional kind)
  "Return the number of items held by the latest auto-sync cycles.
With KIND, `review' or `blocked', count only the items of that kind."
  (let ((n 0))
    (maphash (lambda (_file items)
               (setq n (+ n (if kind
                                (cl-count kind items :key #'car)
                              (length items)))))
             ejira--auto-sync-held)
    n))

(defun ejira-auto-sync-show-log ()
  "Display the `*ejira sync log*' buffer at its latest entries."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ejira sync log*"))
  (goto-char (point-max)))

(defun ejira--auto-sync-show-review (plans)
  "Open the review buffer for PLANS held by auto-sync cycles.
After the confirmed pushes run, the files are queued for a fresh
cycle so the held count reflects what is left."
  (when plans
    (ejira-confirm-show plans)))

(defun ejira-auto-sync-review (&optional full)
  "Reconcile the auto-sync files now and review what they hold.
Opens one review buffer for every local change awaiting confirmation
and for issues changed on both sides, in all the files; see
`ejira--auto-sync-apply'.  When only blocked items are held, shows the
`*ejira sync log*' that explains them instead.  With prefix argument
FULL, fetch every issue from Jira instead of only those Jira changed
\(see `ejira-auto-sync-full-fetch-interval').

The command returns at once; the review opens when Jira has answered."
  (interactive "P")
  (dolist (file (ejira--auto-sync-files))
    (ejira--auto-sync-enqueue file t)
    (when full
      (cl-pushnew file ejira--auto-sync-full-queue :test #'equal)))
  (setq ejira--auto-sync-report-pending t)
  (cond
   ;; The running round ends by starting the next one, which reports.
   (ejira--auto-sync-round
    (message "ejira auto-sync: a sync is running; the review opens when it ends"))
   ((not (ejira--auto-sync-ready-p))
    (message "ejira auto-sync: a Jira sync is running; the review follows it")
    (ejira--auto-sync-kick))
   ((ejira--auto-sync-due-files)
    (message "ejira auto-sync: asking Jira%s..." (if full " for every issue" ""))
    (ejira--auto-sync-start-round (ejira--auto-sync-due-files)))
   (t (setq ejira--auto-sync-report-pending nil)
      (message "ejira auto-sync: no file to sync"))))

(defun ejira--auto-sync-report (round)
  "Tell the outcome of ROUND, a round `ejira-auto-sync-review' asked for.
The review buffer opens for what a push can resolve.  Blocked items
have nothing to review: say so, and show where they are explained,
instead of leaving the command without an answer."
  (let* ((review (ejira-auto-sync-held-count 'review))
         (blocked (ejira-auto-sync-held-count 'blocked))
         (notes (ejira--round-notes round))
         (outcome
          (cond
           ((ejira--round-plans round) nil)
           ((> review 0) nil)
           ((> blocked 0)
            (ejira-auto-sync-show-log)
            (format "nothing to review; %d item(s) blocked until fixed by hand"
                    blocked))
           ;; Unsynced files may hold changes: not "nothing to review".
           (notes nil)
           (t "nothing to review")))
         (parts (delq nil (cons outcome notes))))
    (when parts
      (message "ejira auto-sync: %s" (string-join parts "; ")))))

(defvar ejira--auto-sync-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ejira-auto-sync-review)
    (define-key map [mode-line mouse-3] #'ejira-auto-sync-show-log)
    map)
  "Keymap for the auto-sync mode-line indicator.")

(defvar ejira--auto-sync-blocked-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ejira-auto-sync-show-log)
    map)
  "Keymap for the mode-line indicator of blocked auto-sync items.")

(defvar ejira--disk-conflicts-cache nil
  "(TIME . BUFFERS) of the last `ejira--disk-conflict-buffers' scan.")

(defun ejira--disk-conflict-buffers ()
  "Return the buffers of auto-sync files with unsaved edits over a changed file.
Such a buffer stops its file from syncing: a cycle waits for unsaved
edits to be saved, and saving would overwrite the change on disk.  Only
the user can choose which version to keep.  The result is cached for a
second, because the mode line asks on every redisplay."
  (let ((now (float-time)))
    (if (and ejira--disk-conflicts-cache
             (< (- now (car ejira--disk-conflicts-cache)) 1))
        (cl-remove-if-not #'buffer-live-p (cdr ejira--disk-conflicts-cache))
      (let ((bufs (cl-remove-if-not
                   (lambda (b) (and (buffer-modified-p b) (ejira--file-changed-p b)))
                   (ejira--tracked-buffers))))
        (setq ejira--disk-conflicts-cache (cons now bufs))
        bufs))))

(defun ejira-show-disk-conflict ()
  "Show how the first conflicted auto-sync buffer differs from its file.
Then keep one version: save the buffer to overwrite the file, or revert
it to take the file's version."
  (interactive)
  (if-let* ((buf (car (ejira--disk-conflict-buffers))))
      (diff-buffer-with-file buf)
    (message "ejira: no auto-sync buffer conflicts with its file")))

(defvar ejira--disk-conflict-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ejira-show-disk-conflict)
    map)
  "Keymap for the mode-line indicator of a buffer conflicting with its file.")

(defun ejira--auto-sync-mode-line ()
  "Return the mode-line indicators of auto-sync, or nil.
The number of changes awaiting review, the number of items blocked
until fixed by hand, the files that cannot sync because their buffer
holds unsaved edits while the file changed on disk, and whether a
round waits on Jira."
  (when ejira-auto-sync-mode-line
    (let* ((n (ejira-auto-sync-held-count 'review))
           (blocked (ejira-auto-sync-held-count 'blocked))
           (conflicts (ejira--disk-conflict-buffers))
           (indicator
            (concat
             (when ejira--auto-sync-round
               (propertize " Jira:syncing"
                           'face 'shadow
                           'help-echo "ejira asks Jira about the auto-sync files; mouse-3: show *ejira sync log*"
                           'mouse-face 'mode-line-highlight
                           'local-map ejira--auto-sync-mode-line-map))
             (when conflicts
               (propertize (format " Jira:%s changed on disk"
                                   (mapconcat #'buffer-name conflicts ","))
                           'face 'error
                           'help-echo "Unsaved edits over a file changed on disk: ejira cannot sync it until one version is kept.  mouse-1: diff the buffer with the file"
                           'mouse-face 'mode-line-highlight
                           'local-map ejira--disk-conflict-mode-line-map))
             (when (> n 0)
               (propertize (format " Jira:%d to review" n)
                           'face 'warning
                           'help-echo "ejira changes awaiting review; mouse-1: review, mouse-3: show *ejira sync log*"
                           'mouse-face 'mode-line-highlight
                           'local-map ejira--auto-sync-mode-line-map))
             (when (> blocked 0)
               (propertize (format " Jira:%d blocked" blocked)
                           'face 'error
                           'help-echo "ejira items no push can release: each needs a fix by hand, explained in *ejira sync log*.  mouse-1: show the log"
                           'mouse-face 'mode-line-highlight
                           'local-map ejira--auto-sync-blocked-mode-line-map)))))
      (unless (string-empty-p indicator) indicator))))

(defconst ejira--auto-sync-mode-line-construct
  '(:eval (ejira--auto-sync-mode-line))
  "Mode-line construct added to `global-mode-string' by auto-sync.")

;;; Discovery of Jira-side children

(defcustom ejira-auto-sync-discover nil
  "Whether the automatic sync imports Jira issues missing from the file.
A Jira issue whose epic link or parent is an issue of an auto-sync file,
but which the file does not contain yet (created in Jira, or by someone
else), is otherwise never pulled: the cycle only fetches the keys the
file already holds.
  nil         never import;
  `unresolved' import open children: those not in a terminal status
               (status category Done), whatever their resolution field;
  `all'        import children whatever their status."
  :group 'ejira
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Unresolved children" unresolved)
                 (const :tag "All children" all)))

(defun ejira--buffer-issue-types ()
  "Return an alist (KEY . TYPE) of the ejira issue headings in the buffer.
Issues of projects outside `ejira-projects' are left out."
  (let (res)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-heading-regexp nil t)
       (let ((id (org-entry-get nil "ID"))
             (type (org-entry-get nil "TYPE")))
         (when (and id type
                    (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" id)
                    (ejira--synced-key-p id)
                    (member type '("ejira-issue" "ejira-story"
                                   "ejira-subtask" "ejira-epic")))
           (push (cons id type) res)))))
    (nreverse res)))

(defun ejira--item-parent-key (item)
  "Return the key of REST item ITEM's parent issue or epic, or nil."
  (or (ejira--alist-get item 'fields 'parent 'key)
      (and ejira-epic-field
           (let ((v (ejira--alist-get item 'fields ejira-epic-field)))
             (and (stringp v) v)))))

(defun ejira--auto-sync-discover-query (buf)
  "Return (JQL . PRESENT) to discover the Jira children missing from BUF.
JQL finds the children of the issues of BUF: by epic link for epic
headings, by parent for the other issue headings; see
`ejira-auto-sync-discover'.  PRESENT are the keys BUF already holds.
Return nil when BUF has no issue that can have children."
  (with-current-buffer buf
    (let* ((types (ejira--buffer-issue-types))
           (present (mapcar #'car types))
           (epics (mapcar #'car (cl-remove-if-not
                                 (lambda (e) (equal (cdr e) "ejira-epic")) types)))
           (parents (mapcar #'car (cl-remove-if
                                   (lambda (e) (member (cdr e) '("ejira-epic" "ejira-subtask")))
                                   types)))
           (clauses
            (delq nil
                  (list
                   (when (and epics ejira-epic-field)
                     (format "cf[%s] in (%s)"
                             (replace-regexp-in-string
                              "\\`customfield_" "" (symbol-name ejira-epic-field))
                             (s-join ", " epics)))
                   (when parents
                     (format "parent in (%s)" (s-join ", " parents)))))))
      (when clauses
        (cons (concat "(" (s-join " OR " clauses) ")"
                      ;; Only projects ejira syncs: a child in another
                      ;; project would make ejira create a project file
                      ;; for it.
                      (when ejira-projects
                        (format " AND project in (%s)"
                                (s-join ", " ejira-projects)))
                      ;; Open means not in a terminal status.  A workflow
                      ;; can end an issue without a resolution (RNDSEC's
                      ;; Cancelled sets none), and such an issue is
                      ;; finished: re-importing it from the archive that
                      ;; holds it would pull it again on every cycle.
                      (when (eq ejira-auto-sync-discover 'unresolved)
                        " AND statusCategory != Done"))
              present)))))

(defun ejira--auto-sync-discover-missing (items present)
  "Return the REST ITEMS whose key is not among PRESENT."
  (cl-remove-if (lambda (item) (member (ejira--alist-get item 'key) present))
                items))

(defun ejira--auto-sync-import (item buf &optional on-hold)
  "Import REST ITEM under its parent heading in BUF; return non-nil on success.
The heading is created directly under the parent, then pulled, which
records its baselines.  An
issue already present in another file is pulled, which refiles it
here.  An issue created by a creation whose response was lost is
pulled onto the journaled heading (see `ejira--adopt-creation-journal').
A conversion failure is reported through ON-HOLD (see
`ejira--update-task-or-hold') and leaves no heading behind."
  (let* ((key (ejira--alist-get item 'key))
         (parent (ejira--item-parent-key item))
         (parent-m (and parent (ejira--find-heading parent))))
    (when (and parent-m (eq (marker-buffer parent-m) buf))
      (let ((created (unless (or (ejira--find-heading key)
                                 ;; The outcome of a creation journal: the
                                 ;; pull gives the journaled heading the key,
                                 ;; and a placeholder would duplicate it.
                                 (ignore-errors
                                   (ejira--creation-journal-for (ejira--parse-item item))))
                       (ejira--new-heading buf parent key))))
        (if (ejira--update-task-or-hold item on-hold)
            (and (ejira--find-heading key) t)
          ;; Held: remove the empty placeholder so the next cycle retries
          ;; discovery instead of seeing a heading with no content.
          (when created
            (org-with-point-at created
              (delete-region (point) (ejira--true-subtree-end)))
            (when ejira--heading-cache (remhash key ejira--heading-cache)))
          nil)))))

;;; Read-only audit

(defun ejira--audit-state-match-p (local-state status resolution)
  "Return non-nil when Org LOCAL-STATE corresponds to Jira STATUS/RESOLUTION.
Several Org keywords can map to one Jira status (and one status to
several keywords), so compare through `ejira-todo-states-alist' both
ways rather than by index alone."
  (let* ((remote-index (funcall ejira-todo-state-fn status resolution))
         (local-index (1+ (or (cl-position local-state org-todo-keywords-1
                                           :test #'equal)
                              -2))))
    (or (= remote-index local-index)
        (and (not (equal status "Closed"))
             (member status
                     (mapcar #'car
                             (cl-remove-if-not
                              (lambda (e) (= (cdr e) local-index))
                              ejira-todo-states-alist)))))))

(defun ejira--audit-normalize (s)
  "Normalize Org text S for the audit's local/remote comparison.
Drops what the Jira export cannot carry or renders equivalently, so the
audit reports content differences only: drawers (a LOGBOOK under a
description subheading is never exported), blank lines (the exporter
separates every element with one), the brackets of a bare link
\\=(a plain URL exports as [url] and imports as [[url]]), and whether a
timestamp is active (JIRA dates import as inactive timestamps)."
  (let ((s (ejira--push-normalize (ejira-parser-inactivate-timestamps (or s "")))))
    (setq s (replace-regexp-in-string
             "^[ \t]*:[A-Za-z_]+:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*$" "" s))
    (setq s (replace-regexp-in-string "\\[\\[\\(https?://[^]\n]+\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string "\n\\(?:[ \t]*\n\\)+" "\n" s))
    (string-trim s)))

(defun ejira--audit-convert (markup level)
  "Convert JIRA MARKUP at LEVEL, or return `:unconvertible'."
  (condition-case nil
      (let ((ejira-parser-signal-failures t))
        (ejira--parse-body markup level))
    (ejira-parser-error :unconvertible)))

(defun ejira--audit-comments (key item)
  "Return a list of comment differences for issue KEY against REST ITEM."
  (let* ((remote (ejira--alist-get item 'fields 'comment 'comments))
         (remote-ids (mapcar (lambda (c) (format "%s" (ejira--alist-get c 'id)))
                             remote))
         (diffs nil)
         (local nil))
    (ejira--with-point-on key
      (when-let* ((cm (ejira--find-child-heading ejira-comments-heading-name)))
        (dolist (m (ejira--comment-heading-markers cm))
          (org-with-point-at m
            (push (list (org-entry-get (point) "CommId") m (ejira--locally-modified-p))
                  local)))))
    (dolist (c remote)
      (let* ((id (format "%s" (ejira--alist-get c 'id)))
             (l (assoc id local)))
        (if (not l)
            (push (format "comment %s missing locally" id) diffs)
          (let ((expected (ejira--audit-convert (ejira--alist-get c 'body)
                                                (org-with-point-at (nth 1 l)
                                                  (org-current-level)))))
            (cond
             ((eq expected :unconvertible)
              (push (format "comment %s unconvertible" id) diffs))
             ((not (equal (ejira--audit-normalize expected)
                          (ejira--audit-normalize
                           (ejira--get-heading-body (nth 1 l)))))
              (push (format "comment %s differs%s" id
                            (if (nth 2 l) " (edited locally)" ""))
                    diffs)))))))
    (dolist (l local)
      (unless (member (car l) remote-ids)
        (push (format "comment %s missing in Jira" (car l)) diffs)))
    (nreverse diffs)))

(defun ejira--sync-audit-report (buf items missing)
  "Compare the ejira headings of BUF with REST ITEMS, Jira's version of them.
MISSING are the REST items of the unresolved Jira children absent from
BUF.  Return a plist:
  :rows      one (KEY :class CLASS :diffs DIFFS :dirty D :baseline B)
             per issue heading, CLASS being `identical', `local-newer'
             (differs, edited locally, remote unchanged since the
             baseline), `unknown-baseline' (differs, edited locally, no
             remote baseline), `remote-newer' (differs, no local edit),
             `both-changed', `unconvertible' or `missing-in-jira';
  :missing   keys of MISSING;
  :local-only titles of TODO headings with no Jira identity;
  :duplicates (KEY . FILES) for keys carried by more than one heading;
  :summary   counts per class."
  (let ((ejira--heading-cache (make-hash-table :test 'equal))
        rows local-only duplicates)
    (with-current-buffer buf
      (org-with-wide-buffer
       (let* ((keys (ejira--buffer-issue-keys))
              (by-key (mapcar (lambda (i) (cons (ejira--alist-get i 'key) i)) items)))
         (dolist (key keys)
           (let ((item (cdr (assoc key by-key)))
                 (m (ejira--find-heading key)))
             (if (not item)
                 (push (list key :class 'missing-in-jira :diffs nil) rows)
               (org-with-point-at m
                 (let* ((dirty (ejira--locally-modified-p))
                        (stored (org-entry-get nil ejira-remote-hash-property))
                        (baseline (cond ((not stored) 'none)
                                        ((equal stored (md5 (ejira--remote-fields-identity item)))
                                         'match)
                                        (t 'moved)))
                        (rsum (ejira--audit-convert
                               (ejira--alist-get item 'fields 'summary) nil))
                        (rdesc (condition-case nil
                                   (let ((ejira-parser-signal-failures t))
                                     (ejira--expected-jira-description
                                      m (ejira--alist-get item 'fields 'description)))
                                 (ejira-parser-error :unconvertible)))
                        (status (ejira--alist-get item 'fields 'status 'name))
                        (resolution (ejira--alist-get item 'fields 'resolution 'name))
                        (rprio (ejira--priority-id-string
                                (ejira--alist-get item 'fields 'priority 'id)))
                        (lprio (org-entry-get nil ejira-priority-id-property))
                        (diffs
                         (append
                          (delq nil
                                (list
                                 (cond ((eq rsum :unconvertible) "summary unconvertible")
                                       ((not (equal (ejira--push-normalize rsum)
                                                    (ejira--push-normalize (ejira--jira-summary))))
                                        "summary"))
                                 (cond ((eq rdesc :unconvertible) "description unconvertible")
                                       ((not (equal (ejira--audit-normalize rdesc)
                                                    (ejira--audit-normalize
                                                     (ejira--jira-description))))
                                        "description"))
                                 (unless (ejira--audit-state-match-p
                                          (substring-no-properties (or (org-get-todo-state) ""))
                                          status resolution)
                                   (format "state %s vs %s" (org-get-todo-state) status))
                                 (when (and lprio rprio (not (equal lprio rprio)))
                                   (format "priority %s vs %s" lprio rprio))))
                          (ejira--audit-comments key item)))
                        (class
                         (cond
                          ((cl-some (lambda (d) (string-match-p "unconvertible" d)) diffs)
                           'unconvertible)
                          ((null diffs) 'identical)
                          ((and dirty (eq baseline 'moved)) 'both-changed)
                          ((and dirty (eq baseline 'none)) 'unknown-baseline)
                          (dirty 'local-newer)
                          (t 'remote-newer))))
                   (push (list key :class class :diffs diffs
                               :dirty (and dirty t) :baseline baseline)
                         rows))))))
         (let ((locations (ejira--issue-heading-locations)))
           (dolist (key keys)
             (when (> (length (gethash key locations)) 1)
               (push (cons key (gethash key locations)) duplicates))))
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (when (and (org-get-todo-state)
                      (not (org-entry-get nil "TYPE"))
                      (not (org-in-commented-heading-p)))
             (push (org-get-heading t t t t) local-only))))))
    (setq rows (nreverse rows))
    (let (summary)
      (dolist (r rows)
        (let ((c (plist-get (cdr r) :class)))
          (setf (alist-get c summary) (1+ (alist-get c summary 0)))))
      (list :rows rows
            :missing (mapcar (lambda (i) (ejira--alist-get i 'key)) missing)
            :local-only (nreverse local-only)
            :duplicates duplicates :summary summary))))

(defun ejira-sync-audit-file (file done)
  "Compare every ejira heading of FILE with Jira, without changing anything.
Jira is asked in the background, every issue of FILE and its unresolved
children at once; DONE is then called with the audit plist (see
`ejira--sync-audit-report'), or with (:error MESSAGE) when a request
failed."
  (let* ((buf (find-file-noselect file t))
         (cycle (ejira--cycle-make :file file :full t :audit t))
         (snap (list :buf buf
                     :keys (with-current-buffer buf (ejira--buffer-issue-keys))
                     :discover (let ((ejira-auto-sync-discover 'unresolved))
                                 (ejira--auto-sync-discover-query buf)))))
    (ejira--auto-sync-gather
     cycle snap
     (lambda (remote)
       (run-at-time 0 nil
                    (lambda ()
                      (funcall done (ejira--sync-audit-report
                                     buf (plist-get remote :items)
                                     (plist-get remote :discovered))))))
     (lambda (err)
       (run-at-time 0 nil done (list :error (error-message-string err)))))))

(defun ejira--sync-audit-show (audits)
  "Show AUDITS, a list of (FILE . AUDIT), in the `*ejira sync audit*' buffer."
  (with-current-buffer (get-buffer-create "*ejira sync audit*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (a audits)
        (let ((r (cdr a)))
          (insert (format "* %s\n" (car a)))
          (if-let* ((err (plist-get r :error)))
              (insert (format "  failed: %s\n" err))
            (insert (format "  summary: %S\n" (plist-get r :summary)))
            (dolist (row (plist-get r :rows))
              (unless (eq (plist-get (cdr row) :class) 'identical)
                (insert (format "  %-12s %-16s %s\n" (car row)
                                (plist-get (cdr row) :class)
                                (s-join "; " (plist-get (cdr row) :diffs))))))
            (dolist (k (plist-get r :missing))
              (insert (format "  %-12s %-16s\n" k 'missing-locally)))
            (dolist (d (plist-get r :duplicates))
              (insert (format "  %-12s %-16s %s\n" (car d) 'duplicate
                              (s-join ", " (cdr d)))))
            (dolist (h (plist-get r :local-only))
              (insert (format "  %-12s %-16s %s\n" "-" 'local-only h)))))))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*ejira sync audit*"))

(defun ejira-sync-audit (&optional file)
  "Audit FILE (default: each auto-sync file) against Jira.
Read-only: nothing is pulled, pushed or baselined.  Jira is asked in
the background; the report opens in the `*ejira sync audit*' buffer
when every file has its answers."
  (interactive)
  (let* ((files (if file (list file) (ejira--auto-sync-files)))
         (audits nil)
         (pending (length files)))
    (message "ejira: auditing %d file(s) against Jira..." pending)
    (dolist (f files)
      (ejira-sync-audit-file
       f (lambda (audit)
           (push (cons f audit) audits)
           (when (= (cl-decf pending) 0)
             (ejira--sync-audit-show
              (mapcar (lambda (f) (assoc f audits)) files))))))))

(defun ejira--auto-sync-hold-line (plan)
  "Return the held-item line describing push PLAN."
  (let ((title (plist-get plan :title))
        (fields (mapcar #'car (plist-get plan :changes))))
    (cond
     ((plist-get plan :remote-changed)
      (format "%s: changed remotely since last sync; held for review" title))
     ((eq (plist-get plan :op) 'create)
      (format "%s: creation awaiting review" title))
     ((eq (plist-get plan :op) 'delete)
      (format "%s: deletion awaiting review" title))
     (fields
      (format "%s: %s awaiting review" title (string-join fields ", ")))
     (t (format "%s: awaiting review" title)))))

(defun ejira--auto-sync-hold (file plans)
  "Hold every push PLAN of FILE for the review buffer; send nothing.
Invariant: an automatic cycle never writes to Jira.  Every plan is
collected in `ejira--auto-sync-review-plans' for the review buffer,
where the user confirms what is sent (see `ejira--jira-write').
Return the held items, (KIND . LINE) for the caller to log and count
with the cycle's other held items: KIND is `review', or `pending' for
plans built from stale content, which the next cycle rebuilds."
  (let ((buf (get-file-buffer file))
        held)
    (if (and buf (buffer-live-p buf) (not (verify-visited-file-modtime buf)))
        ;; The file changed on disk mid-cycle (another editor, git): the
        ;; plans were built from stale in-memory content and must not be
        ;; offered for review; the next cycle rebuilds them.
        (dolist (plan plans)
          (push (cons 'pending
                      (format "%s: file changed on disk mid-cycle; held for the next cycle"
                              (plist-get plan :title)))
                held))
      (dolist (plan plans)
        (push plan ejira--auto-sync-review-plans)
        (push (cons 'review (ejira--auto-sync-hold-line plan)) held)))
    (nreverse held)))

(defun ejira--auto-sync-held-message (review blocked pending reviewing)
  "Return the message reporting a cycle's held items.
REVIEW, BLOCKED and PENDING count them by kind; REVIEWING is non-nil
when the cycle opens the review buffer itself."
  (string-join
   (delq nil
         (list (when (> review 0)
                 (format "%d change(s) awaiting review%s" review
                         (if reviewing "" "; click Jira in the mode line to review")))
               (when (> blocked 0)
                 (format "%d item(s) blocked until fixed by hand (see *ejira sync log*)"
                         blocked))
               (when (> pending 0)
                 (format "%d change(s) held for the next cycle" pending))))
   "; "))

;;; Reconciliation cycles
;;
;; A cycle reconciles one file in three steps.  The snapshot reads the
;; file: the issue keys it holds and their baselines.  The fetch asks
;; Jira about them and changes nothing.  The apply step checks that the
;; buffer is still the one the snapshot read, then pulls, imports and
;; holds against the answers.  The fetch runs in the background, its
;; requests in parallel, so Emacs never waits on Jira for a cycle.  A
;; round runs the cycles of several files together and opens a single
;; review for all of them.

(defconst ejira--auto-sync-restart-limit 2
  "Times a round restarts a cycle whose buffer changed during the fetch.
The file is then queued for a later round.")

(defconst ejira--auto-sync-round-timeout 600
  "Seconds after which a round that never ended is abandoned.
Every request has a deadline (`ejira-request-timeout'), so a round
always ends; this guards against a lost callback, which would
otherwise stop automatic reconciliation for the session.")

(defun ejira--auto-sync-snapshot (cycle)
  "Read the file of CYCLE as its fetch needs it.
Return the snapshot, a plist:
  :buf       the file's buffer;
  :tick      its `buffer-chars-modified-tick' when read;
  :keys      the issue keys to fetch, duplicate identities left out;
  :baselines see `ejira--buffer-issue-baselines';
  :discover  see `ejira--auto-sync-discover-query', or nil.
Return `deferred' instead when the buffer holds unsaved edits: a
moving target, reconciled after its next save, which queues the file
again.  Return `gone' when the file no longer exists.  A buffer whose
file changed on disk is reloaded first."
  (let ((file (ejira--cycle-file cycle)))
    (if (not (file-exists-p file))
        (progn (remhash file ejira--auto-sync-mtimes) 'gone)
      (let ((buf (find-file-noselect file t)))
        (catch 'defer
          (ejira--with-transaction
            (with-current-buffer buf
              (when (buffer-modified-p)
                (ejira--auto-sync-enqueue file)
                (throw 'defer 'deferred))
              (when (ejira--file-changed-p)
                (ejira--reload-buffer buf))))
          (let* ((locations (ejira--issue-heading-locations))
                 ;; A pre-scan: comparing each heading with its baseline
                 ;; must not show and refold the whole buffer per heading.
                 (baselines (ejira--with-pre-scan buf
                              (ejira--buffer-issue-baselines))))
            (list :buf buf
                  :tick (buffer-chars-modified-tick buf)
                  ;; A duplicate identity is held: no need to ask about it.
                  :keys (cl-remove-if (lambda (k) (> (length (gethash k locations)) 1))
                                      (mapcar #'car baselines))
                  :baselines baselines
                  :discover (when ejira-auto-sync-discover
                              (ejira--auto-sync-discover-query buf)))))))))

(defun ejira--auto-sync-in-key-order (items keys)
  "Return REST ITEMS sorted as their keys appear in KEYS, unknown keys last."
  (let ((pos (make-hash-table :test 'equal))
        (n 0))
    (dolist (k keys)
      (puthash k (cl-incf n) pos))
    (sort (copy-sequence items)
          (lambda (a b)
            (< (gethash (ejira--alist-get a 'key) pos most-positive-fixnum)
               (gethash (ejira--alist-get b 'key) pos most-positive-fixnum))))))

(defun ejira--auto-sync-fetch-issues (keys k fail)
  "Fetch issues KEYS with the synced fields; call K with the items.
One search per batch of `ejira--auto-sync-batch-size' keys, all sent
at once; FAIL is called with the error of the first that fails."
  (let* ((batches (seq-partition keys ejira--auto-sync-batch-size))
         (pending (length batches))
         (results (make-vector pending nil))
         (failed nil))
    (if (= pending 0)
        (funcall k nil)
      (cl-loop
       for batch in batches
       for i from 0
       do (let ((i i))
            (ejira--async-jql
             (format "key in (%s)" (s-join ", " batch))
             (ejira--get-fields-to-sync nil)
             (lambda (items)
               (unless failed
                 (aset results i items)
                 (when (= (cl-decf pending) 0)
                   (funcall k (apply #'append (append results nil))))))
             (lambda (err)
               (unless failed
                 (setq failed t)
                 (funcall fail err)))))))))

(defun ejira--auto-sync-fetch-updated (keys k fail)
  "Fetch the update time of issues KEYS; call K with the items, or FAIL."
  (ejira--async-jql (format "key in (%s)" (s-join ", " keys)) '("updated") k fail))

(defun ejira--auto-sync-fetch-discovered (snap k fail)
  "Fetch the Jira children missing from the file of SNAP; call K with them.
See `ejira-auto-sync-discover'.  FAIL is called with the error of a
failed search."
  (let ((query (plist-get snap :discover)))
    (ejira--async-jql (car query) (ejira--get-fields-to-sync nil)
                      (lambda (items)
                        (funcall k (ejira--auto-sync-discover-missing
                                    items (cdr query))))
                      fail)))

(defun ejira--auto-sync-scheme-keys (items discovered baselines)
  "Return the keys whose priority scheme an apply step may ask Jira for.
A push plan asks for the scheme of every dirty issue among the REST
ITEMS.  A pull or an import asks for it when the item carries a
priority: the ITEMS differing from their `RemoteHash' in BASELINES,
and the DISCOVERED items."
  (delete-dups
   (delq nil
         (append
          (mapcar (lambda (item)
                    (let* ((key (ejira--alist-get item 'key))
                           (b (cdr (assoc key baselines))))
                      (and (or (plist-get b :dirty)
                               (and (ejira--alist-get item 'fields 'priority 'id)
                                    (not (equal (plist-get b :hash)
                                                (md5 (ejira--remote-fields-identity
                                                      item))))))
                           key)))
                  items)
          (mapcar (lambda (item)
                    (and (ejira--alist-get item 'fields 'priority 'id)
                         (ejira--alist-get item 'key)))
                  discovered)))))

(defun ejira--auto-sync-prefetch-schemes (keys k)
  "Cache the priority schemes of issues KEYS in the background, then call K.
Only the schemes not cached yet are asked for, in parallel.  A scheme
Jira does not give stays uncached: `ejira--get-priority-scheme' asks
again, and reports the failure, when the scheme is needed."
  (unless (hash-table-p ejira--priority-scheme-cache)
    (setq ejira--priority-scheme-cache (make-hash-table :test #'equal)))
  (let* ((todo (cl-remove-if
                (lambda (key)
                  (gethash (ejira--priority-scheme-cache-key key)
                           ejira--priority-scheme-cache))
                keys))
         (pending (length todo)))
    (if (= pending 0)
        (funcall k)
      (dolist (key todo)
        (let ((key key))
          (ejira--request-async
           (format "/rest/api/2/issue/%s/editmeta" key) "GET" nil
           (lambda (data)
             (unwind-protect
                 (when-let* ((scheme (ejira--parse-priority-scheme data)))
                   (puthash (ejira--priority-scheme-cache-key key) scheme
                            ejira--priority-scheme-cache))
               (when (= (cl-decf pending) 0)
                 (funcall k))))
           (lambda (_err)
             (when (= (cl-decf pending) 0)
               (funcall k)))))))))

(defun ejira--auto-sync-gather (cycle snap done fail)
  "Ask Jira, in the background, what CYCLE needs to know about SNAP.
Call DONE with a plist: :items, the REST items of the fetched issues
in file order, and :discovered, the Jira children missing from the
file (see `ejira-auto-sync-discover').  Or call FAIL with the error of
a failed request: never both, and each at most once.

Unless the cycle is full, an issue whose `Modified' still equals Jira's
update time is not fetched.  Jira moves that time on every change of
the issue, comments included, so its fields still hash to the stored
`RemoteHash', and the apply step would do nothing with it.
One light search asks for the update times of all such issues.  An
issue with local changes (its own or its comments'), or lacking either
property, is always fetched: the apply step classifies it, and push
plans compare against it.

Every search is sent at once, and DONE and FAIL run from the requests'
callbacks.  Unless CYCLE is an audit, the priority schemes the apply
step will need are cached first, so that it never waits on Jira."
  (let* ((keys (plist-get snap :keys))
         (baselines (plist-get snap :baselines))
         (candidates
          (unless (ejira--cycle-full cycle)
            (cl-remove-if-not
             (lambda (k)
               (let ((b (cdr (assoc k baselines))))
                 (and (plist-get b :hash)
                      (plist-get b :updated)
                      (not (plist-get b :dirty))
                      (not (plist-get b :comments-dirty)))))
             keys)))
         (direct (cl-remove-if (lambda (k) (member k candidates)) keys))
         ;; One count per awaited answer, plus one held until every
         ;; request is sent: an answer given at once (a request that
         ;; failed to start) must not end the gathering early.
         (pending 1)
         (failed nil)
         (schemes-cached (ejira--cycle-audit cycle))
         (fetched nil)
         (discovered nil))
    (cl-labels
        ((abort (err)
           (unless failed
             (setq failed t)
             (funcall fail err)))
         (answer (fn)
           ;; The handler of an answer: count it in, and turn an error
           ;; into the failure of the whole gathering.
           (lambda (&rest args)
             (unless failed
               (when (condition-case err
                         (progn (apply fn args)
                                (cl-decf pending)
                                t)
                       (error (abort err) nil))
                 (settle)))))
         (fetch (ks)
           (when ks
             (cl-incf pending)
             (ejira--auto-sync-fetch-issues
              ks
              (answer (lambda (items) (setq fetched (append fetched items))))
              #'abort)))
         (settle ()
           (when (and (= pending 0) (not failed))
             (if schemes-cached
                 (funcall done
                          (list :items (ejira--auto-sync-in-key-order fetched keys)
                                :discovered discovered))
               (setq schemes-cached t)
               (cl-incf pending)
               (ejira--auto-sync-prefetch-schemes
                (ejira--auto-sync-scheme-keys fetched discovered baselines)
                (answer #'ignore))))))
      (when (plist-get snap :discover)
        (cl-incf pending)
        (ejira--auto-sync-fetch-discovered
         snap
         (answer (lambda (items) (setq discovered items)))
         #'abort))
      (when candidates
        (cl-incf pending)
        (ejira--auto-sync-fetch-updated
         candidates
         (answer
          (lambda (items)
            ;; Fetch those whose time moved, and those Jira left out.
            (fetch (cl-remove-if
                    (lambda (k)
                      (let ((item (cl-find k items
                                           :key (lambda (i) (ejira--alist-get i 'key))
                                           :test #'equal)))
                        (when-let* ((updated (and item (ejira--alist-get
                                                        item 'fields 'updated))))
                          (equal (ejira--remote-time updated)
                                 (plist-get (cdr (assoc k baselines)) :updated)))))
                    candidates))))
         #'abort))
      (fetch direct)
      ;; Every request is sent.
      (cl-decf pending)
      (settle))))

(defun ejira--auto-sync-apply (cycle snap remote)
  "Reconcile the file of CYCLE against REMOTE, Jira's answers about SNAP.
REMOTE is what `ejira--auto-sync-gather' delivered.  Return `done', or
`restart' when the buffer is no longer the one SNAP read (an edit, a
save, a reload from disk): the answers may describe another version of
the file, and the cycle must start over.

Remote-only changes are pulled and applied.  Every local change is
built into a push plan and held: the cycle itself never writes to
Jira (see `ejira--jira-write').  With review (a save in Emacs, or
`ejira-auto-sync-review'), the plans of everything held that a push
can resolve -- every local change, and issues changed on both sides
or without a remote baseline -- go to `ejira--cycle-plans' for the
review buffer.  Without it (an external change) the held changes are
only counted in the mode line.  What no push can release is held as
blocked: a creation of unknown outcome, a duplicate identity,
unconvertible markup, and a conflicted issue with nothing to push.  It
is counted apart in the mode line and explained in the log; only a
fix by hand releases it.

The step is one sync operation (see `ejira--with-transaction'), so it
leaves no unsaved edit behind: if a save is refused because the file
changed under the buffer, the buffer is reloaded and the file queued
again."
  (let* ((file (ejira--cycle-file cycle))
         (review (ejira--cycle-review cycle))
         (buf (plist-get snap :buf))
         (ejira--syncing t)
         (ejira--pushing t)
         (ejira--heading-cache (make-hash-table :test 'equal))
         (pulls nil)
         ;; Held items, (KIND KEY LINE).  KIND `issue' is a conflicted
         ;; issue: reviewable when it has a plan, blocked otherwise.
         (conflicts nil)
         (notes nil)
         (held-keys nil)
         ;; Conversion holds: keep the issue's pushes out of the cycle
         ;; and report it with the other held issues.
         (unreviewable-keys nil)
         (ejira--auto-sync-review-plans nil)
         (on-hold (lambda (key line)
                    (push key held-keys)
                    (push key unreviewable-keys)
                    (push (list 'blocked key line) conflicts))))
    (setf (ejira--cycle-plans cycle) nil)
    (catch 'restart
      (unless (buffer-live-p buf)
        (throw 'restart 'restart))
      (ejira--with-transaction
        ;; The answers describe the buffer the snapshot read.  A buffer
        ;; changed since (an edit, a save, a reload of a file another
        ;; writer changed) is reconciled from scratch.
        (with-current-buffer buf
          (when (or (buffer-modified-p)
                    (ejira--file-changed-p)
                    (/= (plist-get snap :tick) (buffer-chars-modified-tick)))
            (when (and (not (buffer-modified-p)) (ejira--file-changed-p))
              (ejira--reload-buffer buf))
            (throw 'restart 'restart)))
        ;; ── duplicate identities: hold, never pick one ──
        (let ((locations (ejira--issue-heading-locations)))
          (dolist (key (with-current-buffer buf (ejira--buffer-issue-keys)))
            (let ((files (gethash key locations)))
              (when (> (length files) 1)
                (push key held-keys)
                (push key unreviewable-keys)
                (push (list 'blocked key
                            (format "%s: %d headings carry this key (%s); held until one is removed"
                                    key (length files)
                                    (s-join ", " (mapcar #'file-name-nondirectory
                                                         (delete-dups (copy-sequence files))))))
                      conflicts)))))
        (let ((items (cl-remove-if
                      (lambda (i) (member (ejira--alist-get i 'key) held-keys))
                      (plist-get remote :items)))
              (discovered (plist-get remote :discovered)))
          (with-current-buffer buf
            (org-with-wide-buffer
             ;; ── classify against the remote baselines ──
             (dolist (item items)
               (let* ((key (ejira--alist-get item 'key))
                      (m (ejira--find-heading key))
                      (stored (and m (org-with-point-at m
                                       (org-entry-get nil
                                                      ejira-remote-hash-property))))
                      (remote-changed-p
                       (and stored
                            (not (equal stored
                                        (md5 (ejira--remote-fields-identity
                                              item))))))
                      (remote-unknown-p (and m (not stored)))
                      (dirty (and m (org-with-point-at m
                                      (ejira--locally-modified-p)))))
                 (cond
                  ;; Push candidate only when a remote baseline exists
                  ;; and has not moved: with no baseline, "not changed
                  ;; remotely" is UNKNOWN, and an automatic push could
                  ;; overwrite unseen remote work.  Fail closed.  The
                  ;; remote baseline still holds: date it.
                  ((and dirty (not remote-unknown-p) (not remote-changed-p))
                   (ejira--date-remote-baseline m item))
                  ;; Remote-only change, or no baseline yet: a pull
                  ;; applies the remote state and records both
                  ;; baselines (a no-op fetch for unchanged issues).
                  ((and (not dirty)
                        (or remote-changed-p remote-unknown-p))
                   (push item pulls))
                  ((and remote-changed-p dirty)
                   (push key held-keys)
                   (push (list 'issue key
                               (format "%s: changed locally and remotely" key))
                         conflicts))
                  ;; Dirty with no baseline: the remote side is
                  ;; unknown; never push blind.
                  ((and dirty remote-unknown-p)
                   (push key held-keys)
                   (push (list 'issue key
                               (format "%s: changed locally with no remote baseline" key))
                         conflicts))
                  ;; Clean and unchanged: the baseline still holds.
                  ;; Dated, the next cycles need not fetch it.
                  ((and m stored)
                   (ejira--date-remote-baseline m item)))))
             ;; Imports and pulls rewrite whole subtrees: they run
             ;; on the shown buffer.  Reading, dating baselines and
             ;; building plans need no unfolding, which costs a large
             ;; file a fifth of a second each way.
             (when (or discovered pulls)
               (let ((vis (org-fold-core-get-regions)))
                 (outline-show-all)
                 (unwind-protect
                     (progn
                       ;; ── import Jira-side children missing locally ──
                       (dolist (item discovered)
                         (let ((key (ejira--alist-get item 'key)))
                           (when (ejira--auto-sync-import item buf on-hold)
                             (push (format "%s: imported from Jira under %s"
                                           key (ejira--item-parent-key item))
                                   notes))))
                       ;; ── pull remote-only changes ──
                       (dolist (item (nreverse pulls))
                         (let ((key (ejira--alist-get item 'key)))
                           (if (ejira--issue-comments-dirty-p key)
                               (progn (push key held-keys)
                                      (push (list 'issue key
                                                  (format "%s: locally edited comments; pull deferred"
                                                          key))
                                            conflicts))
                             ;; The pull records both baselines.
                             (let ((ejira--force-full-update t))
                               (ejira--update-task-or-hold item on-hold))))))
                   (org-fold-core-regions vis :override t))))
             ;; Pulls and imports reach the file before the plans
             ;; are built.
             (ejira--save-buffer-safe)
             ;; ── hold local changes for review; never push ──
             (let* ((ops (ejira--with-pre-scan buf
                           (ejira--push-scan-buffer buf)))
                    (blocked (cl-remove-if-not
                              (lambda (op) (eq (plist-get op :op) 'blocked))
                              ops))
                    (actions (cl-remove-if
                              (lambda (op) (eq (plist-get op :op) 'blocked))
                              ops))
                    ;; The fetch answered for every dirty issue a
                    ;; moment ago: the plans need not ask again.
                    (plans (when actions
                             (ejira--push-build-plans
                              actions (plist-get remote :items)))))
               (dolist (b blocked)
                 ;; A TODO with no Jira ancestor is a local task, not
                 ;; a sync item: tracked files mix both freely.
                 (unless (plist-get b :local-only)
                   (push (list 'blocked nil
                               (format "%s: %s"
                                       (plist-get b :title)
                                       (plist-get b :reason)))
                         conflicts)))
               ;; Issue-wide holds: the plans of an issue classified
               ;; as conflicted (staged transitions, type changes,
               ;; cascade creations) are already reported by its
               ;; conflict line; offer them for review unless the
               ;; issue cannot be pushed at all (duplicate identity,
               ;; unconvertible markup).
               (dolist (plan plans)
                 (let ((issue (plist-get plan :parent-issue)))
                   (when (and (member issue held-keys)
                              (not (member issue unreviewable-keys)))
                     (push plan ejira--auto-sync-review-plans))))
               (setq plans
                     (cl-remove-if
                      (lambda (plan)
                        (member (plist-get plan :parent-issue) held-keys))
                      plans))
               ;; Everything else waits for review too: an automatic
               ;; cycle never sends anything to Jira.
               (dolist (h (ejira--auto-sync-hold file plans))
                 (push (list (car h) nil (cdr h)) conflicts)))
             (when notes
               (ejira--auto-sync-log file (nreverse notes)))
             ;; A conflicted issue is reviewable when the review
             ;; buffer offers a push for it; otherwise nothing but
             ;; a fix by hand releases it.
             (let* ((review-keys (mapcar (lambda (p) (plist-get p :parent-issue))
                                         ejira--auto-sync-review-plans))
                    (held (mapcar (lambda (c)
                                    (if (eq (car c) 'issue)
                                        (cons (if (member (nth 1 c) review-keys)
                                                  'review
                                                'blocked)
                                              (nth 2 c))
                                      (cons (car c) (nth 2 c))))
                                  (nreverse conflicts)))
                    (count (lambda (kind) (cl-count kind held :key #'car))))
               (when held
                 (message "ejira auto-sync: %s"
                          (ejira--auto-sync-held-message
                           (funcall count 'review) (funcall count 'blocked)
                           (funcall count 'pending) review))
                 (ejira--auto-sync-log
                  file (mapcar (lambda (h)
                                 (if (eq (car h) 'blocked)
                                     (concat "blocked: " (cdr h))
                                   (cdr h)))
                               held)))
               ;; Pending items are rebuilt by the next cycle: they
               ;; are logged, not counted.
               (ejira--auto-sync-record-held
                file (cl-remove 'pending held :key #'car)))
             (ejira--save-buffer-safe)
             ;; Shown by the caller, after the cycle's own writes;
             ;; confirming runs the plans, and the next cycle
             ;; refreshes the count.
             (when review
               (setf (ejira--cycle-plans cycle)
                     (nreverse ejira--auto-sync-review-plans)))
             ;; A pull can refile or create headings in other files
             ;; (the project files); save those too, or the change
             ;; lives only in an unsaved buffer.
             (dolist (other (delete-dups
                             (cl-loop for m being the hash-values of ejira--heading-cache
                                      when (and (markerp m) (marker-buffer m)
                                                (not (eq (marker-buffer m) buf)))
                                      collect (marker-buffer m))))
               (with-current-buffer other
                 (when (buffer-modified-p)
                   (ejira--save-buffer-safe))))))))
      'done)))

;;; Rounds

(defun ejira--auto-sync-ready-p ()
  "Return non-nil when no other sync operation runs, so a round may start."
  (and (not ejira--sync-in-progress)
       (not (and (boundp 'ejira--pushing) ejira--pushing))
       (not (and (boundp 'ejira--syncing) ejira--syncing))))

(defun ejira--auto-sync-due-files ()
  "Return the auto-sync files due for a cycle, queued ones first.
Queued files, then those whose modification time changed since their
last cycle (another editor, git, an agent)."
  (let ((queued (reverse ejira--auto-sync-queue)))
    (cl-remove-if-not
     (lambda (f)
       (or (member f queued)
           (let ((mtime (ignore-errors
                          (file-attribute-modification-time (file-attributes f)))))
             (and mtime
                  (not (equal mtime (gethash f ejira--auto-sync-mtimes)))))))
     (delete-dups (append queued (ejira--auto-sync-files))))))

(defun ejira--auto-sync-start-round (files)
  "Start a round reconciling FILES; see `ejira--round'.
Each file leaves the queues: its cycle reviews when the file was queued
for review, and fetches every issue when that is due."
  (let ((round (ejira--round-make
                :pending (copy-sequence files)
                :report (prog1 ejira--auto-sync-report-pending
                          (setq ejira--auto-sync-report-pending nil))
                :started (float-time))))
    (setq ejira--auto-sync-round round)
    (ejira--auto-sync-indicator-update)
    (dolist (file files)
      (let ((cycle (ejira--cycle-make
                    :file file
                    :review (and (member file ejira--auto-sync-review-queue) t)
                    :full (and (ejira--auto-sync-full-due-p file) t)
                    :round round
                    :mtime (ignore-errors
                             (file-attribute-modification-time (file-attributes file))))))
        (setq ejira--auto-sync-queue (delete file ejira--auto-sync-queue)
              ejira--auto-sync-review-queue (delete file ejira--auto-sync-review-queue)
              ejira--auto-sync-full-queue (delete file ejira--auto-sync-full-queue))
        (ejira--auto-sync-start-cycle cycle)))))

(defun ejira--auto-sync-continue (fn &rest args)
  "Call FN with ARGS from a timer, once Jira answered.
Jira's answers arrive in a process sentinel, where quitting is
inhibited and errors are lost."
  (apply #'run-at-time 0 nil fn args))

(defun ejira--auto-sync-start-cycle (cycle)
  "Take the snapshot of CYCLE and send its fetch.
The apply step follows when every answer is in; see
`ejira--auto-sync-finish-cycle'."
  (condition-case err
      (let ((snap (ejira--auto-sync-snapshot cycle)))
        (if (not (consp snap))
            (ejira--auto-sync-end-cycle cycle snap)
          (ejira--auto-sync-gather
           cycle snap
           (lambda (remote)
             (ejira--auto-sync-continue #'ejira--auto-sync-finish-cycle
                                        cycle snap remote))
           (lambda (err)
             (ejira--auto-sync-continue #'ejira--auto-sync-end-cycle
                                        cycle 'error err)))))
    (error (ejira--auto-sync-end-cycle cycle 'error err))))

(defun ejira--auto-sync-finish-cycle (cycle snap remote)
  "Apply REMOTE, Jira's answers about SNAP, then end CYCLE.
A cycle whose buffer changed during the fetch starts over, up to
`ejira--auto-sync-restart-limit' times; then its file is queued for a
later round.  A cycle of an abandoned round is dropped."
  (when (and (not (ejira--cycle-ended cycle))
             (eq (ejira--cycle-round cycle) ejira--auto-sync-round))
    (condition-case err
        (if (not (eq (ejira--auto-sync-apply cycle snap remote) 'restart))
            (ejira--auto-sync-end-cycle cycle 'done)
          (let ((file (ejira--cycle-file cycle)))
            ;; A save meanwhile asked for a review: the new start gives it.
            (when (member file ejira--auto-sync-review-queue)
              (setf (ejira--cycle-review cycle) t))
            (setq ejira--auto-sync-queue (delete file ejira--auto-sync-queue)
                  ejira--auto-sync-review-queue (delete file ejira--auto-sync-review-queue))
            (if (< (ejira--cycle-restarts cycle) ejira--auto-sync-restart-limit)
                (progn
                  (cl-incf (ejira--cycle-restarts cycle))
                  (ejira--auto-sync-start-cycle cycle))
              (ejira--auto-sync-enqueue file (ejira--cycle-review cycle))
              (ejira--auto-sync-end-cycle cycle 'restarted))))
      (error (ejira--auto-sync-end-cycle cycle 'error err)))))

(defun ejira--auto-sync-end-cycle (cycle outcome &optional err)
  "End CYCLE with OUTCOME, and its round when it was the last cycle.
OUTCOME is `done', `deferred' (unsaved edits), `gone' (no file),
`restarted' (the buffer kept changing) or `error', with ERR.  Record
what the next rounds need: the file's modification time after a
successful cycle, so it is not reconciled again until it changes; the
time of a full fetch; and failed attempts, capped by
`ejira-auto-sync-retry-limit'."
  (unless (ejira--cycle-ended cycle)
    (setf (ejira--cycle-ended cycle) t)
    (let* ((file (ejira--cycle-file cycle))
           (round (ejira--cycle-round cycle))
           (name (file-name-nondirectory file))
           (note nil))
      (pcase outcome
        ('done
         ;; The cycle's own save changed the file: record that time, or
         ;; the next round would reconcile it again.
         (puthash file
                  (or (ignore-errors
                        (file-attribute-modification-time (file-attributes file)))
                      (ejira--cycle-mtime cycle))
                  ejira--auto-sync-mtimes)
         (remhash file ejira--auto-sync-attempts)
         (when (ejira--cycle-full cycle)
           (puthash file (float-time) ejira--auto-sync-full-fetch-times)))
        ('deferred
         (setq note (format "%s has unsaved edits; save it to sync" name)))
        ('restarted
         (setq note (format "%s kept changing during the sync; queued again" name)))
        ('error
         ;; The time is recorded only after a successful cycle: a
         ;; transient failure leaves it stale, so the next round retries
         ;; the file instead of silently dropping it.
         (let ((n (1+ (gethash file ejira--auto-sync-attempts 0)))
               (msg (error-message-string err)))
           (puthash file n ejira--auto-sync-attempts)
           (if (>= n ejira-auto-sync-retry-limit)
               (progn
                 ;; Stop retrying: pin the time and report once.
                 (puthash file (ejira--cycle-mtime cycle) ejira--auto-sync-mtimes)
                 (remhash file ejira--auto-sync-attempts)
                 (message "ejira auto-sync: giving up on %s after %d attempts: %s"
                          file n msg))
             (message "ejira auto-sync: %s failed (attempt %d/%d): %s"
                      file n ejira-auto-sync-retry-limit msg))
           (setq note (format "%s failed: %s" name msg)))))
      (when (and round (eq round ejira--auto-sync-round))
        (when (eq outcome 'done)
          (setf (ejira--round-plans round)
                (append (ejira--round-plans round) (ejira--cycle-plans cycle))))
        (when note
          (setf (ejira--round-notes round)
                (append (ejira--round-notes round) (list note))))
        (setf (ejira--round-pending round)
              (delete file (ejira--round-pending round)))
        (unless (ejira--round-pending round)
          (ejira--auto-sync-end-round round))))))

(defun ejira--auto-sync-end-round (round)
  "End ROUND: open one review for what its cycles hold, and report.
Then have the worker run again if files became due meanwhile."
  (when (eq round ejira--auto-sync-round)
    (setq ejira--auto-sync-round nil))
  (ejira--auto-sync-indicator-update)
  (when-let* ((plans (ejira--round-plans round)))
    (run-at-time 0 nil #'ejira--auto-sync-show-review plans))
  (when (ejira--round-report round)
    (ejira--auto-sync-report round))
  (ejira--auto-sync-kick))

(defun ejira--auto-sync-kick ()
  "Have the worker run at the next idle moment when a file can sync now.
Files saved or changed during a round wait for it to end, and the
repeating idle timer only runs again after the next input.  A file
whose buffer holds unsaved edits waits for its save instead."
  (when (and ejira--auto-sync-timer
             (cl-some (lambda (f)
                        (let ((b (get-file-buffer f)))
                          (not (and b (buffer-modified-p b)))))
                      (ejira--auto-sync-due-files)))
    (run-with-idle-timer ejira-auto-sync-interval nil #'ejira--auto-sync-worker)))

(defun ejira--auto-sync-expire-round ()
  "Abandon a round that has not ended within `ejira--auto-sync-round-timeout'.
Its unfinished files are queued again; answers still arriving for it
are dropped."
  (when-let* ((round ejira--auto-sync-round))
    (when (> (- (float-time) (ejira--round-started round))
             ejira--auto-sync-round-timeout)
      (setq ejira--auto-sync-round nil)
      (dolist (file (ejira--round-pending round))
        (ejira--auto-sync-enqueue file (ejira--round-report round)))
      (message "ejira auto-sync: abandoned a sync that never finished: %s"
               (mapconcat #'file-name-nondirectory (ejira--round-pending round) ", "))
      (ejira--auto-sync-indicator-update))))

(defun ejira--auto-sync-worker ()
  "Start a round for every auto-sync file due for a cycle.
Each call first refreshes the indicator: a buffer may have come to
conflict with its file since the last call."
  (ejira--auto-sync-indicator-update)
  (ejira--auto-sync-expire-round)
  (when (and (ejira--auto-sync-files)
             (not ejira--auto-sync-round)
             (ejira--auto-sync-ready-p))
    (when-let* ((files (ejira--auto-sync-due-files)))
      (ejira--auto-sync-start-round files))))

(defun ejira--start-auto-sync ()
  "Start the automatic reconciliation timer for the auto-sync files.
See `ejira-auto-sync-tracked' and `ejira-auto-sync-files'."
  (when (and (ejira--auto-sync-files) (not ejira--auto-sync-timer))
    (setq ejira--auto-sync-timer
          (run-with-idle-timer ejira-auto-sync-interval
                               ejira-auto-sync-interval
                               #'ejira--auto-sync-worker))
    (unless (member ejira--auto-sync-mode-line-construct global-mode-string)
      (setq global-mode-string
            (append (or global-mode-string '(""))
                    (list ejira--auto-sync-mode-line-construct))))
    ;; The indicator just joined the mode line: announce it.
    (setq ejira--auto-sync-indicator-state nil)
    (ejira--auto-sync-indicator-update)
    (message "ejira: auto-sync enabled for %d file(s)"
             (length (ejira--auto-sync-files)))))

(defun ejira--stop-auto-sync ()
  "Tear down the automatic reconciliation timer."
  (when ejira--auto-sync-timer
    (cancel-timer ejira--auto-sync-timer)
    (setq ejira--auto-sync-timer nil))
  (setq global-mode-string
        (delete ejira--auto-sync-mode-line-construct global-mode-string))
  ;; The indicator left the mode line: announce it.
  (setq ejira--auto-sync-indicator-state nil)
  (force-mode-line-update t)
  (run-hooks 'ejira-auto-sync-indicator-hook))


;;;###autoload
(defun ejira-set-deadline (arg &optional time)
  "Set deadline of issue under point. Save buffer to stage push."
  (interactive "P")
  (ejira--with-point-on (ejira-issue-id-under-point)
    (org-deadline arg time)))

;;;###autoload
(defun ejira-set-priority ()
  "Set priority of the issue under point using its Jira priority scheme.
Save the buffer to stage the exact Jira priority ID for push."
  (interactive)
  (let* ((key (ejira-issue-id-under-point))
         (project (ejira--get-project key))
         (scheme (ejira--get-priority-scheme key))
         (selectable (ejira--selectable-priority-scheme project scheme)))
    (unless scheme
      (user-error "No editable Jira priority scheme available for %s" key))
    (unless selectable
      (user-error "No selectable Jira priorities available for %s" key))
    (let* ((choices
            (mapcar (lambda (entry)
                      (cons (format "%s [%s]"
                                    (plist-get entry :name)
                                    (plist-get entry :id))
                            entry))
                    selectable))
           (selected (completing-read "Priority: " choices nil t))
           (entry (cdr (assoc selected choices)))
           (rank (ejira--priority-rank scheme (plist-get entry :id) project)))
      (ejira--with-point-on key
        (ejira--ensure-org-priority-range rank)
        (org-priority (ejira--org-priority-for-rank rank))
        (org-set-property ejira-priority-id-property (plist-get entry :id))
        (org-set-property ejira-priority-name-property (plist-get entry :name))
        (org-set-property ejira-priority-rank-property (number-to-string rank))))))

;;;###autoload
(defun ejira-assign-issue (&optional to-me)
  "Set the assignee of the issue under point.
With prefix-argument TO-ME assign to me."
  (interactive "P")
  (ejira--assign-issue (ejira-issue-id-under-point) to-me))

;;;###autoload
(defun ejira-progress-issue ()
  "Stage a Jira status transition. The actual transition is confirmed via
ejira-confirm on save."
  (interactive)
  (let* ((key (ejira-issue-id-under-point))
         (actions (jiralib2-get-actions key))
         (selected (rassoc
                    (completing-read "Action: " (mapcar #'cdr actions))
                    actions)))
    (when selected
      (ejira--with-point-on key
        (org-set-property "PendingTransition" (cdr selected)))
      (message "ejira: transition '%s' staged — save buffer to confirm."
               (cdr selected)))))

;;;###autoload
(defun ejira-set-issuetype ()
  "Stage an issuetype change for the issue under point."
  (interactive)
  (let* ((id (ejira-get-id-under-point nil t))
         (key (nth 1 id))
         (type (ejira--select-issuetype)))
    (when type
      (ejira--with-point-on key
        (org-set-property "PendingIssuetype" type))
      (message "ejira: issuetype '%s' staged — save buffer to confirm." type))))

;;;###autoload
(defun ejira-set-epic ()
  "Stage an epic change for the issue under point."
  (interactive)
  (let* ((id (ejira-issue-id-under-point))
         (epic (ejira--select-id-or-nil
                "Select epic: "
                (ejira--get-headings-in-agenda-files :type "ejira-epic"))))
    (ejira--with-point-on id
      (org-set-property "PendingEpic" (or epic "")))
    (message "ejira: epic change staged — save buffer to confirm.")))

;;;###autoload
(defun ejira-focus-on-issue (key)
  "Open an indirect buffer narrowed to issue KEY."
  (interactive)
  (let* ((m (or (ejira--find-heading key)
                (error (concat "no issue: " key))))
         (m-buffer (marker-buffer m))
         (buffer-name (concat "*" key "*"))
         (b (or (get-buffer buffer-name)
                (make-indirect-buffer m-buffer (concat "*" key "*") t))))
    (switch-to-buffer b)
    (widen)
    (outline-show-all)
    (goto-char m)
    (org-narrow-to-subtree)
    (outline-show-subtree)
    (ejira-mode 1)))

;;;###autoload
(defun ejira-focus-on-clocked-issue ()
  "Goto current or last clocked item, and narrow to it, and expand it."
  (interactive)
  (ejira-focus-on-issue (ejira--get-clocked-issue)))


(defun ejira-close-buffer ()
  "Close the current buffer viewing issue details."
  (interactive)
  (kill-buffer (current-buffer))

  ;; Because we are using indirect buffers, killing current buffer will not go
  ;; back to the previous buffer, but instead to the corresponding direct
  ;; buffer. Switching to previous buffer here does the trick.
  ;; (switch-to-prev-buffer)
  )

(defun ejira-insert-link-to-clocked-issue ()
  "Insert link to currently clocked issue into buffer."
  (interactive)
  (insert (format "%s/browse/%s" jiralib2-url (ejira--get-clocked-issue))))

;;;###autoload
(defun ejira-focus-item-under-point ()
  "And narrow to item under point, and expand it."
  (interactive)
  (ejira-focus-on-issue (ejira-issue-id-under-point)))

;;;###autoload
(defun ejira-focus-up-level ()
  "Try to focus the parent item of the item under point."
  (interactive)
  (ejira-focus-on-issue
   (ejira--with-point-on (ejira-issue-id-under-point)
     (org-up-element)
     (ejira-issue-id-under-point))))

(define-minor-mode ejira-mode
  "Minor mode for managing JIRA tickets in a narrowed Org buffer."
  :lighter " Ejira"
  :init-value nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q") #'ejira-close-buffer)
            (define-key map (kbd "C-c C-d") #'ejira-set-deadline)
            (define-key map (kbd "C-c ,") #'ejira-set-priority)
            ;; (define-key map (kbd "C-c C-t") #'ejira-progress-issue)
            map))

(defun ejira--get-first-id-matching-jql (jql)
  "Helper function for `ejira-guess-epic-sprint-fields'.
Return the first item matching JQL."
  (nth 0
       (alist-get 'issues
                  (jiralib2-session-call "/rest/api/2/search"
                                         :type "POST"
                                         :data (json-encode
                                                `((jql . ,jql)
                                                  (startAt . 0)
                                                  (maxResults . 1)
                                                  (fields . ("key"))))))))

(defun ejira-refile (key)
  "Refile heading under point under item KEY."
  (let ((target (or (ejira--find-heading key) (error "Item not found"))))
    (org-refile nil nil
                `(nil ,(buffer-file-name (marker-buffer target)) nil
                      ,(marker-position target)))))

(defun ejira-guess-epic-sprint-fields ()
  "Try to guess the custom field names for epic and sprint."
  (interactive)
  (message "Attempting to auto-configure Ejira custom fields...")
  (let* ((epic-key (alist-get 'key (ejira--get-first-id-matching-jql
                                    (format "type = %s" ejira-epic-type-name))))
         (issue-key (alist-get 'key (ejira--get-first-id-matching-jql
                                     (format "type != %s" ejira-epic-type-name))))
         (epic-meta (jiralib2-session-call
                     (format "/rest/api/2/issue/%s/editmeta" epic-key)))
         (issue-meta (jiralib2-session-call
                      (format "/rest/api/2/issue/%s/editmeta" issue-key)))

         (epic-field (caar (-filter (lambda (field)
                                      (equal (alist-get 'name field) "Epic Link"))
                                    (alist-get 'fields epic-meta))))
         (sprint-field (caar (-filter (lambda (field)
                                        (equal (alist-get 'name field) "Sprint"))
                                      (alist-get 'fields issue-meta))))
         (epic-summary-field (caar (-filter (lambda (field)
                                              (equal (alist-get 'name field) "Epic Name"))
                                            (alist-get 'fields epic-meta)))))
    (setq ejira-epic-field epic-field
          ejira-epic-summary-field epic-summary-field
          ejira-sprint-field sprint-field)
    (message "Successfully configured custom fields")))

(provide 'ejira)
;;; ejira.el ends here
