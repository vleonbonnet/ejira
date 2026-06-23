;;; ejira-push.el --- Push pipeline for ejira -*- lexical-binding: t -*-
;;; Commentary:
;; Owns all Jira write operations for ejira.  Scans org buffers for pending
;; operations, batch-fetches remote state, builds plan plists, and shows
;; ejira-confirm.  The only place jiralib2 write functions are called is
;; inside :send thunks.
;;; Code:
(require 'ejira-core)
(require 'ejira-confirm)

(defvar ejira-push-on-save t
  "When non-nil, saving any org buffer that contains ejira-managed headings
offers to push locally-edited issues and comments through a review buffer.")

(defvar ejira--pushing nil
  "Bound to t while a push writes the :Pushhash: baseline back.")

(defun ejira--push-finalize (marker)
  "Refresh MARKER's push baseline after a successful push and save its buffer."
  (org-with-point-at marker (ejira--update-push-baseline))
  (let ((ejira--pushing t))
    (with-current-buffer (marker-buffer marker)
      (when (buffer-modified-p) (save-buffer)))))

(defun ejira--buffer-has-pushable-p ()
  "Return non-nil if the current buffer contains any ejira-managed heading."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^[ \t]*:Pushhash:" nil t))))

(defun ejira--push-scan-buffer (buf)
  "Return list of pending-op plists for BUF."
  (with-current-buffer buf
    (org-with-wide-buffer
     (save-excursion
       (let ((ops nil))
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (let* ((type (org-entry-get nil "TYPE"))
                  (id (org-entry-get nil "ID"))
                  (pending-delete (org-entry-get nil "PendingDelete"))
                  (pending-transition (org-entry-get nil "PendingTransition"))
                  (pending-issuetype (org-entry-get nil "PendingIssuetype"))
                  (pending-epic (org-entry-get nil "PendingEpic"))
                  (todo-state (org-get-todo-state))
                  (heading-title (org-get-heading t t t t))
                  (marker (point-marker)))
             ;; Rule H: PendingDelete (check first)
             (if (and (equal pending-delete "t") (equal type "ejira-comment"))
                 (push (list :op 'delete
                             :object 'comment
                             :key (org-entry-get nil "CommId")
                             :project nil
                             :marker marker
                             :data (list :issue-key (org-entry-get nil "ID" t)
                                         :commid (org-entry-get nil "CommId")
                                         :body (ejira--get-heading-body marker)))
                       ops)
               ;; Rules A-G (only when NOT PendingDelete)
               ;; Rule A: Dirty Pushhash — issues
               (when (and (member type ejira-pushable-types)
                          id
                          (not (equal type "ejira-comment"))
                          (ejira--locally-modified-p)
                          (not pending-delete))
                 (let ((proj (condition-case nil
                                 (ejira--get-project id)
                               (error nil))))
                   (push (list :op 'update
                               :object 'issue
                               :key id
                               :project proj
                               :marker marker
                               :data nil)
                         ops)))
               ;; Rule A2: Dirty Pushhash — existing comments (have CommId)
               (when (and (equal type "ejira-comment")
                          (org-entry-get nil "CommId")
                          (ejira--locally-modified-p)
                          (not pending-delete))
                 (let* ((commid (org-entry-get nil "CommId"))
                        (issue-key (org-entry-get nil "ID" t))
                        (proj (condition-case nil
                                  (ejira--get-project issue-key)
                                (error nil))))
                   (push (list :op 'update
                               :object 'comment
                               :key commid
                               :project proj
                               :marker marker
                               :data (list :issue-key issue-key
                                           :commid commid))
                         ops)))
               ;; Rule B: PendingTransition
               (when (and pending-transition (member type ejira-pushable-types))
                 (push (list :op 'update
                             :object 'status
                             :key id
                             :project (condition-case nil (ejira--get-project id) (error nil))
                             :marker marker
                             :data (list :action-name pending-transition))
                       ops))
               ;; Rule C: PendingIssuetype
               (when pending-issuetype
                 (push (list :op 'update
                             :object 'issuetype
                             :key id
                             :project (condition-case nil (ejira--get-project id) (error nil))
                             :marker marker
                             :data (list :new-type pending-issuetype
                                         :old-type (org-entry-get nil "Issuetype")))
                       ops))
               ;; Rule D: PendingEpic
               (when pending-epic
                 (push (list :op 'update
                             :object 'epic
                             :key id
                             :project (condition-case nil (ejira--get-project id) (error nil))
                             :marker marker
                             :data (list :new-epic pending-epic))
                       ops))
               ;; Rule E/F: New heading without TYPE
               (when (and todo-state
                          (not type)
                          (not (equal heading-title ejira-description-heading-name))
                          (not (equal heading-title ejira-comments-heading-name)))
                 (let* ((parent-info
                         (save-excursion
                           (when (org-up-heading-safe)
                             (list (org-entry-get nil "TYPE")
                                   (org-entry-get nil "ID")))))
                        (parent-type (nth 0 parent-info))
                        (parent-id (nth 1 parent-info)))
                   (cond
                    ((member parent-type '("ejira-issue" "ejira-story"
                                          "ejira-epic" "ejira-subtask"))
                     (let ((project-key (condition-case nil
                                            (ejira--get-project parent-id)
                                          (error nil))))
                       (push (list :op 'create
                                   :object 'subtask
                                   :key nil
                                   :project project-key
                                   :marker marker
                                   :data (list :parent-key parent-id
                                               :project-key project-key))
                             ops)))
                    ((equal parent-type "ejira-project")
                     (push (list :op 'create
                                 :object 'issue
                                 :key nil
                                 :project parent-id
                                 :marker marker
                                 :data (list :project-key parent-id))
                           ops)))))
               ;; Rule G: New comment draft — heading directly under Comments,
               ;; no CommId yet.  Catches manually-added plain headings and
               ;; org-capture stubs (TYPE=ejira-comment, no CommId).
               (when (and (not (org-entry-get nil "CommId"))
                          (or (not type) (equal type "ejira-comment"))
                          (not todo-state))
                 (let* ((parent-title
                         (save-excursion
                           (when (org-up-heading-safe)
                             (org-get-heading t t t t))))
                        (issue-key
                         (when (equal parent-title ejira-comments-heading-name)
                           (save-excursion
                             (org-up-heading-safe)
                             (when (org-up-heading-safe)
                               (org-entry-get nil "ID")))))
                        (proj (when issue-key
                                (condition-case nil
                                    (ejira--get-project issue-key)
                                  (error nil)))))
                   (when issue-key
                     (push (list :op 'create
                                 :object 'comment
                                 :key nil
                                 :project proj
                                 :marker marker
                                 :data (list :issue-key issue-key))
                           ops)))))
           (goto-char (line-end-position))))
         (nreverse ops))))))

(defun ejira--push-scan-all ()
  "Scan all ejira-managed org files for pending operations."
  (let ((ejira-dir (file-truename (expand-file-name ejira-org-directory))))
    (cl-mapcan
     (lambda (f)
       (when (and (file-exists-p f)
                  (string-prefix-p ejira-dir (file-truename f)))
         (ejira--push-scan-buffer (find-file-noselect f t))))
     (org-agenda-files))))

(defun ejira--push-build-plans (pending-ops)
  "Build plan plists from PENDING-OPS."
  (let ((plans nil)
        (issue-update-ops nil)
        (other-ops nil))
    (dolist (op pending-ops)
      (if (and (eq (plist-get op :op) 'update)
               (eq (plist-get op :object) 'issue))
          (push op issue-update-ops)
        (push op other-ops)))
    (when issue-update-ops
      (let* ((keys (mapcar (lambda (op) (plist-get op :key)) issue-update-ops))
             (remote-items
              (condition-case err
                  (apply #'jiralib2-jql-search
                         (format "key in (%s)" (s-join ", " keys))
                         (ejira--get-fields-to-sync))
                (error
                 (message "ejira: failed to fetch remote state: %s"
                          (error-message-string err))
                 nil))))
        (dolist (op issue-update-ops)
          (let* ((key (plist-get op :key))
                 (marker (plist-get op :marker))
                 (project (plist-get op :project))
                 (item (cl-find-if (lambda (i)
                                     (equal (ejira--alist-get i 'key) key))
                                   remote-items))
                 (desc-marker (condition-case nil
                                  (ejira--find-task-subheading key ejira-description-heading-name)
                                (error nil)))
                 (local-summary (org-with-point-at marker
                                  (ejira--strip-properties (org-get-heading t t t t))))
                 (local-desc-org (if desc-marker (ejira--get-heading-body desc-marker) ""))
                 (local-assignee (or (org-entry-get marker "Assignee") ""))
                 (local-priority-char (org-entry-get marker "PRIORITY"))
                 (local-priority-name (when local-priority-char
                                        (car (rassoc (string-to-char local-priority-char)
                                                     ejira-priorities-alist))))
                 (local-deadline (when-let ((d (org-get-deadline-time marker)))
                                   (format-time-string "%Y-%m-%d" d)))
                 (remote-summary (when item (ejira--alist-get item 'fields 'summary)))
                 (remote-desc-org (when item
                                    (ejira--expected-org-body
                                     (or desc-marker marker)
                                     (ejira--alist-get item 'fields 'description))))
                 (remote-assignee (or (when item
                                        (ejira--alist-get item 'fields 'assignee 'displayName))
                                      ""))
                 (remote-priority-name (when item
                                         (ejira--alist-get item 'fields 'priority 'name)))
                 (remote-deadline (when item (ejira--alist-get item 'fields 'duedate)))
                 (changes (ejira-confirm-field-changes
                           `(("summary"     ,remote-summary  ,local-summary)
                             ("description" ,(or remote-desc-org "") ,local-desc-org)
                             ("assignee"    ,remote-assignee ,local-assignee)
                             ("priority"    ,(or remote-priority-name "") ,(or local-priority-name ""))
                             ("deadline"    ,(or remote-deadline "") ,(or local-deadline "")))))
                 (summary-changed (assoc "summary" changes))
                 (desc-changed (assoc "description" changes))
                 (assignee-changed (assoc "assignee" changes))
                 (priority-changed (assoc "priority" changes))
                 (deadline-changed (assoc "deadline" changes)))
            (when changes
              (push (list :op 'update
                          :object 'issue
                          :project project
                          :title key
                          :changes changes
                          :send (let ((key key) (marker marker)
                                      (local-summary local-summary)
                                      (local-desc-org local-desc-org)
                                      (local-assignee local-assignee)
                                      (local-priority-name local-priority-name)
                                      (local-deadline local-deadline)
                                      (summary-changed summary-changed)
                                      (desc-changed desc-changed)
                                      (assignee-changed assignee-changed)
                                      (priority-changed priority-changed)
                                      (deadline-changed deadline-changed))
                                  (lambda ()
                                    (when (or summary-changed desc-changed)
                                      (jiralib2-update-summary-description
                                       key local-summary
                                       (ejira-parser-org-to-jira local-desc-org)))
                                    (when assignee-changed
                                      (let* ((users (ejira--get-assignable-users key))
                                             (username (car (rassoc local-assignee users))))
                                        (jiralib2-assign-issue key username)))
                                    (when (and priority-changed local-priority-name)
                                      (jiralib2-update-issue
                                       key `(priority . ((name . ,local-priority-name)))))
                                    (when deadline-changed
                                      (jiralib2-update-issue
                                       key `(duedate . ,(or local-deadline ""))))
                                    (ejira--push-finalize marker))))
                    plans))))))
    (dolist (op (nreverse other-ops))
      (let* ((op-type (plist-get op :op))
             (object (plist-get op :object))
             (key (plist-get op :key))
             (marker (plist-get op :marker))
             (project (plist-get op :project))
             (data (plist-get op :data))
             (plan nil))
        (cond
         ((and (eq op-type 'update) (eq object 'comment))
          (let* ((issue-key (plist-get data :issue-key))
                 (commid (plist-get data :commid))
                 (comment-data (condition-case err
                                   (jiralib2-get-comment issue-key commid)
                                 (error (message "ejira: failed to fetch comment: %s"
                                                 (error-message-string err))
                                        nil)))
                 (remote-body (when comment-data (ejira--alist-get comment-data 'body)))
                 (remote-org (when comment-data
                               (ejira--expected-org-body marker remote-body)))
                 (local-body (ejira--get-heading-body marker))
                 (changes (ejira-confirm-field-changes
                           `(("body" ,(or remote-org "") ,local-body)))))
            (when changes
              (setq plan (list :op 'update
                               :object 'comment
                               :project project
                               :title (format "%s comment %s" issue-key commid)
                               :changes changes
                               :send (let ((issue-key issue-key) (commid commid) (marker marker))
                                       (lambda ()
                                         (let ((local-body (ejira--get-heading-body marker)))
                                           (jiralib2-edit-comment
                                            issue-key commid
                                            (ejira-parser-org-to-jira local-body))
                                           (ejira--push-finalize marker)))))))))
         ((and (eq op-type 'update) (eq object 'status))
          (let ((action-name (plist-get data :action-name)))
            (setq plan (list :op 'update
                             :object 'status
                             :project project
                             :title key
                             :changes `(("transition" "" ,action-name))
                             :send (let ((key key) (marker marker) (action-name action-name))
                                     (lambda ()
                                       (let* ((actions (jiralib2-get-actions key))
                                              (action (cl-find-if
                                                       (lambda (a) (equal (cdr a) action-name))
                                                       actions)))
                                         (if action
                                             (progn
                                               (jiralib2-do-action key (car action))
                                               (ejira--update-task key)
                                               (org-with-point-at marker
                                                 (org-delete-property "PendingTransition")))
                                           (error "ejira: transition '%s' not available for %s"
                                                  action-name key)))))))))
         ((and (eq op-type 'update) (eq object 'issuetype))
          (let ((new-type (plist-get data :new-type))
                (old-type (plist-get data :old-type)))
            (setq plan (list :op 'update
                             :object 'issuetype
                             :project project
                             :title key
                             :changes `(("issuetype" ,(or old-type "") ,new-type))
                             :send (let ((key key) (marker marker) (new-type new-type))
                                     (lambda ()
                                       (jiralib2-set-issue-type key new-type)
                                       (ejira--update-task key)
                                       (org-with-point-at marker
                                         (org-delete-property "PendingIssuetype"))))))))
         ((and (eq op-type 'update) (eq object 'epic))
          (let ((new-epic (plist-get data :new-epic)))
            (setq plan (list :op 'update
                             :object 'epic
                             :project project
                             :title key
                             :changes `(("epic" "" ,new-epic))
                             :send (let ((key key) (marker marker) (new-epic new-epic))
                                     (lambda ()
                                       (jiralib2-update-issue key `(,ejira-epic-field . ,new-epic))
                                       (ejira--update-task key)
                                       (org-with-point-at marker
                                         (org-delete-property "PendingEpic"))))))))
         ((and (eq op-type 'create) (eq object 'subtask))
          (let* ((parent-key (plist-get data :parent-key))
                 (project-key (plist-get data :project-key))
                 (heading-title (org-with-point-at marker
                                  (ejira--strip-properties (org-get-heading t t t t))))
                 (preview (let ((body (ejira--get-heading-body marker)))
                            (if (and body (> (length body) 0))
                                (substring body 0 (min 80 (length body)))
                              "(no body)"))))
            (setq plan (list :op 'create
                             :object 'subtask
                             :project project-key
                             :title (format "new subtask: %s"
                                            (substring heading-title
                                                       0 (min 60 (length heading-title))))
                             :preview preview
                             :send (let ((marker marker) (project-key project-key)
                                         (parent-key parent-key))
                                     (lambda ()
                                       (let* ((summary (org-with-point-at marker
                                                          (ejira--strip-properties
                                                           (org-get-heading t t t t))))
                                              (desc-heading (org-with-point-at marker
                                                              (ejira--find-child-heading
                                                               ejira-description-heading-name)))
                                              (desc (if desc-heading
                                                        (ejira-parser-org-to-jira
                                                         (ejira--get-heading-body desc-heading))
                                                      ""))
                                              (result (jiralib2-create-issue
                                                       project-key ejira-subtask-type-name
                                                       summary desc
                                                       `(parent . ((key . ,parent-key)))))
                                              (new-key (ejira--alist-get result 'key)))
                                         (org-with-point-at marker
                                           (org-set-property "ID" new-key))
                                         (puthash new-key
                                                  (abbreviate-file-name
                                                   (buffer-file-name (marker-buffer marker)))
                                                  org-id-locations)
                                         (ejira--update-task new-key))))))))
         ((and (eq op-type 'create) (eq object 'issue))
          (let* ((project-key (plist-get data :project-key))
                 (heading-title (org-with-point-at marker
                                  (ejira--strip-properties (org-get-heading t t t t))))
                 (preview (let ((body (ejira--get-heading-body marker)))
                            (if (and body (> (length body) 0))
                                (substring body 0 (min 80 (length body)))
                              "(no body)"))))
            (setq plan (list :op 'create
                             :object 'issue
                             :project project-key
                             :title (format "new issue: %s" heading-title)
                             :preview preview
                             :send (let ((marker marker) (project-key project-key))
                                     (lambda ()
                                       (let* ((summary (org-with-point-at marker
                                                          (ejira--strip-properties
                                                           (org-get-heading t t t t))))
                                              (desc-heading (org-with-point-at marker
                                                              (ejira--find-child-heading
                                                               ejira-description-heading-name)))
                                              (desc (if desc-heading
                                                        (ejira-parser-org-to-jira
                                                         (ejira--get-heading-body desc-heading))
                                                      ""))
                                              (result (jiralib2-create-issue
                                                       project-key
                                                       (or ejira-story-type-name "Task")
                                                       summary desc))
                                              (new-key (ejira--alist-get result 'key)))
                                         (org-with-point-at marker
                                           (org-set-property "ID" new-key))
                                         (puthash new-key
                                                  (abbreviate-file-name
                                                   (buffer-file-name (marker-buffer marker)))
                                                  org-id-locations)
                                         (ejira--update-task new-key))))))))
         ((and (eq op-type 'create) (eq object 'comment))
          (let* ((issue-key (plist-get data :issue-key))
                 (preview (let ((body (ejira--get-heading-body marker)))
                            (if (and body (> (length body) 0))
                                (substring body 0 (min 80 (length body)))
                              "(no body)"))))
            (setq plan (list :op 'create
                             :object 'comment
                             :project project
                             :title (format "new comment on %s" issue-key)
                             :preview preview
                             :send (let ((marker marker) (issue-key issue-key))
                                     (lambda ()
                                       (let* ((body (ejira--get-heading-body marker))
                                              (comment (ejira--parse-comment
                                                        (jiralib2-add-comment
                                                         issue-key
                                                         (ejira-parser-org-to-jira body)))))
                                         (org-with-point-at marker
                                           (org-set-property "CommId" (ejira-comment-id comment))
                                           (org-set-property "TYPE" "ejira-comment"))
                                         (ejira--update-comment issue-key comment))))))))
         ((and (eq op-type 'delete) (eq object 'comment))
          (let* ((issue-key (plist-get data :issue-key))
                 (commid (plist-get data :commid))
                 (body (plist-get data :body)))
            (setq plan (list :op 'delete
                             :object 'comment
                             :project project
                             :title (format "delete comment on %s" issue-key)
                             :preview (or body "(empty)")
                             :send (let ((issue-key issue-key) (commid commid) (marker marker))
                                     (lambda ()
                                       (jiralib2-delete-comment issue-key commid)
                                       (org-with-point-at marker
                                         (ejira--with-expand-all
                                           (org-cut-subtree))))))))))
        (when plan (push plan plans))))
    (nreverse plans)))

(defun ejira-push-at-point ()
  "Scan the ejira heading at point and show ejira-confirm for it."
  (interactive)
  (let* ((ops (ejira--push-scan-buffer (current-buffer)))
         (point-ops (cl-remove-if-not
                     (lambda (op)
                       (let ((m (plist-get op :marker)))
                         (and m (equal (marker-buffer m) (current-buffer))
                              (= (save-excursion (goto-char m)
                                                 (line-beginning-position))
                                 (line-beginning-position)))))
                     ops))
         (plans (when point-ops (ejira--push-build-plans point-ops))))
    (if plans
        (ejira-confirm-show plans)
      (message "ejira: nothing to push at point"))))

(defun ejira--push-on-save ()
  "Offer to push locally-edited ejira items after saving a managed buffer."
  (when (and ejira-push-on-save
             (not ejira--pushing)
             (not ejira--syncing)
             (derived-mode-p 'org-mode)
             (ejira--buffer-has-pushable-p))
    (let* ((ops (ejira--push-scan-buffer (current-buffer)))
           (plans (when ops (ejira--push-build-plans ops))))
      (when plans
        (ejira-confirm-show plans)))))

(add-hook 'after-save-hook #'ejira--push-on-save)

(provide 'ejira-push)
;;; ejira-push.el ends here
