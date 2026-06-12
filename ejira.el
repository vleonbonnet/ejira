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



(defvar ejira-push-deadline-changes t
  "Sync deadlines to server when updated with `ejira-set-deadline'.")

(defvar ejira-push-confirm t
  "When non-nil, `ejira-push-item-under-point' shows a review buffer
of the pending changes before sending any request to the server.")

(defvar ejira-update-jql-resolved-fn #'ejira-jql-all-resolved-project-tickets
  "Generates JQL used in `ejira-update-project' to find server-resolved items.
Must take a project-id as a string, a list of keys, and return JQL as a string.")

(defvar ejira-update-jql-unresolved-multi-fn
  #'ejira-jql-all-unresolved-multi-project-tickets
  "Generates JQL to find unresolved items for a list of project IDs.
Used by both `ejira-update-my-projects' (full list) and `ejira-update-project'
(single-element list). Must take a list of project-id strings and return JQL.")

(defun ejira-add-comment (to-clocked)
  "Capture new comment to issue under point.
With prefix-argument TO-CLOCKED add comment to currently clocked issue."
  (interactive "P")
  (ejira--capture-comment (if to-clocked
                              (ejira--get-clocked-issue)
                            (ejira-issue-id-under-point))))

(defun ejira-delete-comment ()
  "Delete comment under point."
  (interactive)
  (let* ((item (ejira-get-id-under-point "ejira-comment"))
         (id (nth 1 item)))
    (when (y-or-n-p (format "Delete comment %s? " (cdr id)))
      (ejira--delete-comment (car id) (cdr id)))))

(defun ejira-jql-all-unresolved-multi-project-tickets (project-ids)
  "Default multi-project JQL for `ejira-update-jql-unresolved-multi-fn'.
Returns unresolved tickets across all PROJECT-IDS."
  (format "project in (%s) and resolution = unresolved"
          (s-join ", " (mapcar (lambda (p) (format "'%s'" p)) project-ids))))

(defun ejira-jql-all-resolved-project-tickets (project-id keys)
  "Builds JQL for server-resolved project tickets in PROJECT-ID from local KEYS.
This is the function used in `ejira-update-project'. Override with
`ejira-update-jql-resolved-fn'."
  (format "project = '%s' and key in (%s) and resolution = done"
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

(defun ejira--apply-sync (projects unresolved-items resolved-items shallow)
  "Apply UNRESOLVED-ITEMS and RESOLVED-ITEMS to org files for PROJECTS.
Called from async callbacks once all network responses have arrived."
  ;; Suppress the \"changed since visited or saved\" prompt for the entire sync.
  ;; ejira--new-heading calls basic-save-buffer mid-sync to register new IDs,
  ;; which advances the on-disk modtime while we continue editing the buffer.
  ;; Overriding verify-visited-file-modtime to always return t prevents
  ;; basic-save-buffer and save-buffer from asking the user to confirm.
  (cl-letf (((symbol-function 'verify-visited-file-modtime) (lambda (&optional _) t)))
    (let ((ejira--syncing t)
          (ejira--heading-cache (make-hash-table :test 'equal))
          (save-silently t)
          ;; Suppress all per-operation noise (org-todo, org-priority, org-refile,
          ;; basic-save-buffer "Wrote X", etc.) for the duration of the sync.
          ;; message-log-max is checked by the C message_dolog function, so it
          ;; suppresses *Messages* logging regardless of how message is called.
          ;; "ejira: sync finished" is printed after this let, stays visible.
          (message-log-max nil))
      ;; Refresh org-id locations so manually-refiled issues are found.
      (org-id-update-id-locations nil t)
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
        ;; Process all fetched items.
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
        ;; Normalize: ensure exactly one blank line after every :END: closer.
        ;; ejira--set-heading-body handles body content spacing, but does not
        ;; insert blanks between a heading's property drawer and its child headings.
        (dolist (id projects)
          (when-let ((buf (find-buffer-visiting
                           (expand-file-name (ejira--project-file-name id)))))
            (with-current-buffer buf
              (ejira--normalize-end-spacing))))
        ;; Save all modified project buffers, then restore fold state.
        (dolist (id projects)
          (when-let ((buf (find-buffer-visiting
                           (expand-file-name (ejira--project-file-name id)))))
            (with-current-buffer buf (save-buffer))))
        ;; Restore fold state for any buffer that was open before the sync.
        (dolist (entry vis-saves)
          (with-current-buffer (car entry)
            (org-fold-core-regions (cdr entry) :override t))))
      ;; Persist any new IDs created during this sync.
      (org-id-update-id-locations nil t)))
  (message "ejira: sync finished"))

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
  "Upload content of issue, project or comment under point to server.
For a project, this includes the summary, for a task the summary and
description, and for the comment the body.
When `ejira-push-confirm' is non-nil, the current server state is
fetched first and the pending changes are shown in a review buffer;
nothing is sent until the user confirms with \\<ejira-confirm-mode-map>\\[ejira-confirm-execute]."
  (interactive)
  (let* ((item (ejira-get-id-under-point))
         (id (nth 1 item))
         (type (nth 0 item)))
    (cond ((equal type "ejira-comment")
           (let* ((heading (nth 2 item))
                  (local-org (ejira--get-heading-body heading))
                  (send (lambda ()
                          (jiralib2-edit-comment
                           (car id) (cdr id) (ejira-parser-org-to-jira local-org))
                          (message "ejira: pushed comment %s of %s" (cdr id) (car id)))))
             (if (not ejira-push-confirm)
                 (funcall send)
               ;; Compare in org-space: the body stored on the last sync equals
               ;; ejira--expected-org-body of the current remote, so an unchanged
               ;; comment diffs to nothing and the push is a no-op.
               (let* ((remote-jira (ejira--alist-get
                                    (jiralib2-get-comment (car id) (cdr id)) 'body))
                      (remote-org (ejira--expected-org-body heading remote-jira))
                      (changes (ejira-confirm-field-changes
                                `(("body" ,remote-org ,local-org)))))
                 (if (null changes)
                     (message "ejira: no changes to push")
                   (ejira-confirm-show
                    (format "%s comment %s" (car id) (cdr id)) changes send))))))
          ((equal type "ejira-project")
           (message "TODO"))
          (t
           (let* ((desc-heading (ejira--find-task-subheading
                                 id ejira-description-heading-name))
                  (local-summary (ejira--with-point-on id
                                   (ejira--strip-properties (org-get-heading t t t t))))
                  (local-org-desc (ejira--get-heading-body desc-heading))
                  (send (lambda ()
                          (jiralib2-update-summary-description
                           id local-summary (ejira-parser-org-to-jira local-org-desc))
                          (message "ejira: pushed %s" id))))
             (if (not ejira-push-confirm)
                 (funcall send)
               ;; Diff in org-space (see comment above): summary is plain text;
               ;; the description compares the local org body against what the
               ;; current remote markup would parse to, making a freshly-synced
               ;; issue idempotent (no spurious heading-level diffs).
               (let* ((remote (jiralib2-get-issue id))
                      (remote-summary (ejira--alist-get remote 'fields 'summary))
                      (remote-org-desc (ejira--expected-org-body
                                        desc-heading
                                        (ejira--alist-get remote 'fields 'description)))
                      (changes (ejira-confirm-field-changes
                                `(("summary"     ,remote-summary  ,local-summary)
                                  ("description" ,remote-org-desc ,local-org-desc)))))
                 (if (null changes)
                     (message "ejira: no changes to push")
                   (ejira-confirm-show id changes send)))))))))

(defun ejira-browse-issue-under-point ()
  "Open the current issue in external browser."
  (interactive)
  (browse-url (concat (replace-regexp-in-string "/*$" "" jiralib2-url) "/browse/" (ejira-issue-id-under-point))))


(defun ejira--heading-to-item (heading project-id type &rest args)
  "Create an item from HEADING of TYPE into PROJECT-ID with parameters ARGS."
  (let* ((summary (ejira--strip-properties (org-get-heading t t t t)))
         (description (ejira-parser-org-to-jira (ejira--get-heading-body heading)))
         (item (ejira--parse-item
                (apply #'jiralib2-create-issue project-id
                       type summary description args))))

    (ejira--update-task (ejira-task-key item))
    (ejira-task-key item)))

(defun ejira-heading-to-task (focus)
  "Make the current org-heading into a JIRA task.
With prefix argument FOCUS, focus the issue after creating."
  (interactive "P")
  (let* ((heading (save-excursion
                    (if (outline-on-heading-p t)
                        (beginning-of-line)
                      (outline-back-to-heading))
                    (point-marker)))
         (project-id (ejira--select-project))
         (key (when project-id (ejira--heading-to-item heading project-id "Task"))))

    (when (and key focus)
      (ejira-focus-on-issue key))))

(defun ejira-heading-to-subtask (focus)
  "Make the current org-heading into a JIRA subtask.
With prefix argument FOCUS, focus the issue after creating."
  (interactive "P")
  (let* ((heading (save-excursion
                    (if (outline-on-heading-p t)
                        (beginning-of-line)
                      (outline-back-to-heading))
                    (point-marker)))
         (story (ejira--select-story))
         (project-id (ejira--get-project story))
         (key (when project-id(ejira--heading-to-item heading project-id
                                                      ejira-subtask-type-name
                                                      `(parent . ((key . ,story)))))))
    (when (and key focus)
      (ejira-focus-on-issue key))))

(defun ejira-update-project (id &optional shallow)
  "Update all issues in project ID.
If DEEP set to t, update each issue with separate API call which pulls also
comments. With SHALLOW, only update todo status and assignee."
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
  ;; Handles cases:
  ;; *local*    | *remote*
  ;; ===========+===========
  ;; unresolved | resolved
  ;;
  (let ((keys (mapcar #'car (ejira--get-headings-in-file
                             (ejira--project-file-name id)
                             '(:todo "todo")))))
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

  ;; Normalize spacing, save, then restore fold state.
  (when-let ((buf (find-buffer-visiting
                   (expand-file-name (ejira--project-file-name id)))))
    (with-current-buffer buf
      (ejira--normalize-end-spacing)
      (save-buffer)
      (org-fold-core-regions vis-save :override t)))))

;;;###autoload
(defun ejira-update-my-projects (&optional shallow)
  "Synchronize data on projects listed in `ejira-projects'.
With prefix argument SHALLOW, update only the todo state and assignee.
Fires one combined unresolved JQL and one combined resolved JQL in
parallel, then applies all updates synchronously when both arrive."
  (interactive "P")
  (let* ((projects ejira-projects)
         (fields (ejira--get-fields-to-sync shallow))
         ;; Collect local TODO keys across all project files before firing
         ;; network requests — this is a fast local scan, no network needed.
         (local-todo-keys
          (mapcan (lambda (id)
                    (mapcar #'car
                            (ejira--get-headings-in-file
                             (ejira--project-file-name id) '(:todo "todo"))))
                  projects))
         ;; Mutable state shared between the two async callbacks.
         (pending 0)
         (all-unresolved nil)
         (all-resolved nil))
    (cl-labels
        ((maybe-apply ()
           (when (= pending 0)
             (ejira--apply-sync projects all-unresolved all-resolved shallow))))
      (message "ejira: fetching...")
      ;; Fire unresolved query for all projects in one round-trip.
      (cl-incf pending)
      (ejira--async-jql
       (funcall ejira-update-jql-unresolved-multi-fn projects) fields
       (lambda (items) (setq all-unresolved items) (cl-decf pending) (maybe-apply))
       (lambda (err) (message "ejira: unresolved fetch failed: %s" err) (cl-decf pending) (maybe-apply)))
      ;; Fire resolved-check query in parallel if there are any local TODOs.
      ;; This finds tickets that are TODO in org but already closed on Jira.
      (if local-todo-keys
          (progn
            (cl-incf pending)
            (ejira--async-jql
             (format "key in (%s) and resolution = done"
                     (s-join ", " local-todo-keys))
             fields
             (lambda (items) (setq all-resolved items) (cl-decf pending) (maybe-apply))
             (lambda (_err) (cl-decf pending) (maybe-apply))))
        ;; No local TODOs, nothing to check.
        (maybe-apply)))))

;;;###autoload
(defun ejira-set-deadline (arg &optional time)
  "Wrapper around `org-deadline' which pushes the changed deadline to server.
ARG and TIME get passed on to `org-deadline'."
  (interactive "P")
  (ejira--with-point-on (ejira-issue-id-under-point)
    (org-deadline arg time)
    (when ejira-push-deadline-changes
      (let ((deadline (org-get-deadline-time (point-marker))))
        (jiralib2-update-issue (ejira-issue-id-under-point)
                               `(duedate . ,(when deadline
                                              (format-time-string "%Y-%m-%d"
                                                                  deadline))))))))

;;;###autoload
(defun ejira-set-priority ()
  "Set priority of the issue under point."
  (interactive)
  (ejira--with-point-on (ejira-issue-id-under-point)
    (let ((p (completing-read "Priority: "
                              (mapcar #'car ejira-priorities-alist))))
      (jiralib2-update-issue (ejira-issue-id-under-point)
                             `(priority . ((name . ,p))))
      (org-priority (alist-get p ejira-priorities-alist nil nil #'equal)))))

;;;###autoload
(defun ejira-assign-issue (&optional to-me)
  "Set the assignee of the issue under point.
With prefix-argument TO-ME assign to me."
  (interactive "P")
  (ejira--assign-issue (ejira-issue-id-under-point) to-me))

;;;###autoload
(defun ejira-progress-issue ()
  "Progress the issue under point with a selected action."
  (interactive)
  (ejira--progress-item (ejira-issue-id-under-point)))

;;;###autoload
(defun ejira-set-issuetype ()
  "Select a new issuetype for the issue under point."
  (interactive)
  (let* ((id (ejira-get-id-under-point nil t))
         (ejira-type (nth 0 id))
         (key (if (equal ejira-type "ejira-comment")
                  (user-error "Cannot set type of comment")
                (nth 1 id)))
         (type (ejira--select-issuetype)))
    (jiralib2-set-issue-type key  type)
    (ejira--update-task key)))

;;;###autoload
(defun ejira-set-epic ()
  "Select a new epic for issue under point."
  (interactive)
  (ejira--set-epic (ejira-issue-id-under-point)
                   (ejira--select-id-or-nil
                    "Select epic: "
                    (ejira--get-headings-in-agenda-files :type "ejira-epic"))))

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
