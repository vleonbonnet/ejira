;;; ejira.el --- Org-mode interface to JIRA  -*- lexical-binding: t -*-

;; Copyright (C) 2017 - 2022 Henrik Nyman

;; Author: Henrik Nyman <h@nyymanni.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira
;; Package-Requires: ((emacs "26.1"))
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




(defvar ejira-update-jql-resolved-fn #'ejira-jql-all-resolved-project-tickets
  "Generates JQL used in `ejira-update-project' to find server-resolved items.
Must take a project-id as a string, a list of keys, and return JQL as a string.")

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
Returns unresolved tickets across all PROJECT-IDS."
  (format "project in (%s) and resolution = unresolved"
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

(defun ejira--async-jql (jql fields success-fn &optional error-fn)
  "Run JQL asynchronously, fetching all pages; call SUCCESS-FN with full list."
  (unless jiralib2--session (jiralib2-session-login))
  ;; Drop any nil-named fields (ejira-epic-field / ejira-sprint-field unset).
  (let ((fields (cl-remove-if (lambda (f) (or (null f) (equal f "nil"))) fields))
        (results '()))
    (cl-labels ((fetch (start)
                  (request (concat jiralib2-url "/rest/api/2/search")
                           :type "POST"
                           :headers `(("Content-Type" . "application/json")
                                      ,(ejira--auth-header))
                           :data (json-encode `((jql . ,jql)
                                                (startAt . ,start)
                                                (maxResults . 1000)
                                                (fields . ,fields)))
                           :parser (lambda ()
                                     (let ((json-array-type 'list)) (json-read)))
                           :success (cl-function
                                     (lambda (&key data &allow-other-keys)
                                       (let ((page (alist-get 'issues data))
                                             (total (alist-get 'total data)))
                                         (setq results (append results page))
                                         (if (< (length results) total)
                                             (fetch (length results))
                                           (funcall success-fn results)))))
                           :error (cl-function
                                   (lambda (&key error-thrown &allow-other-keys)
                                     (if error-fn
                                         (funcall error-fn error-thrown)
                                       (message "ejira: JQL error: %s"
                                                error-thrown)))))))
      (fetch 0))))

(defvar ejira--trace-file nil
  "When set to a file path, `ejira--apply-sync' appends timestamped phase
traces there.  Debugging aid for locating where a sync stalls; nil disables it.")

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
      ;; Suppress the \"changed since visited or saved\" prompt for the entire sync.
  ;; ejira--new-heading calls basic-save-buffer mid-sync to register new IDs,
  ;; which advances the on-disk modtime while we continue editing the buffer.
  ;; Overriding verify-visited-file-modtime to always return t prevents
  ;; basic-save-buffer and save-buffer from asking the user to confirm.
  (cl-letf (((symbol-function 'verify-visited-file-modtime) (lambda (&optional _) t)))
    (let ((ejira--syncing t)
          (ejira--heading-cache (make-hash-table :test 'equal))
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
                               (when-let ((buf (find-buffer-visiting path)))
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
                           (lambda (i) (ejira--update-task (ejira--parse-item i))))))
          (mapc update-fn unresolved-items)
          (mapc update-fn resolved-items))
        (ejira--trace "after loop")
        ;; Normalize: ensure exactly one blank line after every :END: closer.
        ;; No-ops when spacing is already correct — see ejira--normalize-end-spacing.
        (dolist (id projects)
          (when-let ((buf (find-buffer-visiting
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
            (when-let ((buf (find-buffer-visiting
                             (expand-file-name (ejira--project-file-name id)))))
              (cl-pushnew buf touched :test #'eq)))
          (dolist (buf touched)
            (with-current-buffer buf
              (when (buffer-modified-p) (save-buffer)))))
        (ejira--trace "after save")
        ;; Restore fold state for any buffer that was open before the sync.
        (dolist (entry vis-saves)
          (with-current-buffer (car entry)
            (org-fold-core-regions (cdr entry) :override t)))
        (ejira--trace "after fold-restore"))))
    (setq ejira--sync-in-progress nil)
    (message "ejira: sync finished")))

(defun ejira-pull-item-under-point ()
  "Update the issue, project or comment under point."
  (interactive)
  (let* ((item (ejira-get-id-under-point))
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



(defun ejira-heading-to-task (focus)
  "Mark the current heading as a pending Jira issue creation.
Ensure it has a TODO state; save the buffer to stage creation via ejira-confirm.
FOCUS is accepted for compatibility but has no effect until confirmed."
  (interactive "P")
  (unless (org-get-todo-state)
    (org-todo (car org-todo-keywords-1)))
  (message "ejira: heading marked as pending issue — save buffer to stage."))

(defun ejira-heading-to-subtask (focus)
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
                             (ejira--update-task (ejira--parse-item i))))
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
                                 (ejira--update-task (ejira--parse-item i))))
                    (apply #'jiralib2-jql-search
                           (funcall ejira-update-jql-resolved-fn id keys)
                           (ejira--get-fields-to-sync shallow)))))

          ;; TODO: Handle issue being deleted from server:
          ;; *local*    | *remote*
          ;; ===========+===========
          ;; unresolved |
          ;; resolved   |

          ;; Normalize spacing on the project buffer.
          (when-let ((buf (find-buffer-visiting
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
            (when-let ((buf (find-buffer-visiting
                             (expand-file-name (ejira--project-file-name id)))))
              (cl-pushnew buf touched :test #'eq))
            (dolist (buf touched)
              (with-current-buffer buf
                (when (buffer-modified-p) (save-buffer)))))
          ;; Restore fold state on the project buffer.
          (when-let ((buf (find-buffer-visiting
                           (expand-file-name (ejira--project-file-name id)))))
            (with-current-buffer buf
              (org-fold-core-regions vis-save :override t)))))
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
         (lambda (err) (message "ejira: unresolved fetch failed: %s" err) (cl-decf pending) (maybe-apply)))
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
  "Pull from Jira if not already syncing.
Calls `ejira-update-my-projects' with `ejira-auto-pull-shallow'."
  (when (and ejira-auto-pull-interval
             (not ejira--sync-in-progress)
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


;;;###autoload
(defun ejira-set-deadline (arg &optional time)
  "Set deadline of issue under point. Save buffer to stage push."
  (interactive "P")
  (ejira--with-point-on (ejira-issue-id-under-point)
    (org-deadline arg time)))

;;;###autoload
(defun ejira-set-priority ()
  "Set priority of the issue under point. Save buffer to stage push."
  (interactive)
  (ejira--with-point-on (ejira-issue-id-under-point)
    (let ((p (completing-read "Priority: "
                              (mapcar #'car ejira-priorities-alist))))
      (org-priority (alist-get p ejira-priorities-alist nil nil #'equal)))))

;;;###autoload
(defun ejira-assign-issue (&optional to-me)
  "Set the assignee of the issue under point.
With prefix-argument TO-ME assign to me."
  (interactive "P")
  (ejira--assign-issue (ejira-issue-id-under-point) to-me))

;;;###autoload
(defun ejira-progress-issue ()
  "Stage a Jira status transition. The actual transition is confirmed via ejira-confirm on save."
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
  "Ejira Mode"
  "Minor mode for managing JIRA ticket in a narrowed org buffer."
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
