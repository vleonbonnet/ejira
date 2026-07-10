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

(defvar ejira--assign-new-issues t
  "Default value for the assign-self cell on new issue plans.
Individual cells are mutable `(list t)' stored on each plan's
:assign-self property, toggled interactively in the confirm buffer.")

(defvar ejira--pushing nil
  "Bound to t while a push batch is executing (inhibits re-scan on save).")

(defvar ejira-state-resolution-alist
  '((5 . "Done")
    (6 . "Won't Fix"))
  "Alist mapping org-todo-keyword index to the Jira resolution name used when
a transition requires a resolution field (e.g. the 'Closed' workflow step).
Index values correspond to positions in org-todo-keywords-1 (1-based).")

(defun ejira--transition-to-org-state (key org-state todo-keywords)
  "Transition Jira issue KEY to the state mapped from ORG-STATE.
TODO-KEYWORDS is the buffer's org-todo-keywords-1 list used for index lookup.
Signals an error if no matching transition is available or the API call fails."
  (unless (and (stringp org-state) (> (length org-state) 0)
               todo-keywords (boundp 'ejira-todo-states-alist))
    (error "ejira: cannot transition %s — missing state or todo-states-alist" key))
  (let* ((pos (cl-position org-state todo-keywords :test #'string=))
         (idx (when pos (1+ pos))))
    (unless idx
      (error "ejira: org state %S not found in todo-keywords for %s" org-state key))
    (let* ((target-states
            (delq nil (mapcar (lambda (e)
                                (when (= (cdr e) idx) (car e)))
                              ejira-todo-states-alist)))
           ;; Fetch transitions with field metadata to detect required fields.
           (all-transitions
            (cdadr
             (jiralib2-session-call
              (format "/rest/api/2/issue/%s/transitions?expand=transitions.fields" key))))
           (actions (mapcar (lambda (trans)
                              (cons (cdr (assoc 'id trans))
                                    (cdr (assoc 'name trans))))
                            all-transitions))
           (action (cl-find-if (lambda (a) (member (cdr a) target-states))
                               actions)))
      (unless action
        (error "ejira: no Jira transition to %S available for %s (tried: %s)"
               org-state key (mapconcat #'identity target-states ", ")))
      (let* ((action-id (car action))
             (trans-detail (cl-find-if (lambda (tr)
                                         (equal (cdr (assoc 'id tr)) action-id))
                                       all-transitions))
             (fields (cdr (assoc 'fields trans-detail)))
             (res-field (cdr (assoc 'resolution fields)))
             (resolution-required (eq (cdr (assoc 'required res-field)) t))
             (resolution-name (when resolution-required
                                (cdr (assq idx ejira-state-resolution-alist)))))
        (if resolution-name
            (jiralib2-session-call
             (format "/rest/api/2/issue/%s/transitions" key)
             :type "POST"
             :data (json-encode `((transition . ((id . ,action-id)))
                                  (fields . ((resolution . ((name . ,resolution-name))))))))
          (jiralib2-do-action key action-id))))))

(defun ejira--finalize-new-issue (new-key marker orig-state todo-keywords)
  "Post-create housekeeping for a newly-created Jira issue NEW-KEY.
Sets the org ID property, updates org-id-locations, tries to transition the Jira
issue to ORIG-STATE before refreshing so the Pushhash is stamped with the final
state.  If the transition is unavailable, force-sets the org state locally and
leaves the heading dirty so the next save retries the state push."
  (org-with-point-at marker (org-set-property "ID" new-key))
  (puthash new-key
           (abbreviate-file-name (buffer-file-name (marker-buffer marker)))
           org-id-locations)
  (condition-case err
      (ejira--transition-to-org-state new-key orig-state todo-keywords)
    (error (display-warning 'ejira (format "transition skipped for new issue %s: %s"
                                         new-key (error-message-string err))
                            :warning)))
  (ejira--update-task new-key)
  (when (and (stringp orig-state) (> (length orig-state) 0))
    (let* ((m (ejira--find-heading new-key))
           (cur (when m (org-with-point-at m
                          (substring-no-properties (or (org-get-todo-state) ""))))))
      (when (and m (not (equal cur orig-state)))
        ;; Force local state but do NOT re-baseline: the Pushhash from
        ;; ejira--update-task reflects the Jira state, so the heading stays
        ;; dirty and the next save will push the state transition.
        (org-with-point-at m (org-todo orig-state))))))

(defun ejira--push-scan-issue-children (parent-marker project-key)
  "Return a list of child plists for the new issue heading at PARENT-MARKER.
Each child plist has keys :marker :title :state :body.
Only scans direct children (depth 1) — Jira subtasks cannot have subtasks."
  (ignore project-key)
  (let (children)
    (org-with-wide-buffer
     (save-excursion
       (goto-char parent-marker)
       (let* ((parent-level (org-current-level))
              (end (save-excursion (org-end-of-subtree t) (point))))
         (while (and (outline-next-heading) (< (point) end))
           (when (= (org-current-level) (1+ parent-level))
             (let* ((type       (org-entry-get nil "TYPE"))
                    (todo-state (org-get-todo-state))
                    (heading    (org-get-heading t t t t)))
               (when (and todo-state
                          (not type)
                          (not (equal heading ejira-description-heading-name))
                          (not (equal heading ejira-comments-heading-name)))
                 (push (list :marker (point-marker)
                             :title  (ejira--strip-properties heading)
                             :state  (substring-no-properties (or todo-state ""))
                             :body   (ejira--get-heading-body (point-marker)))
                       children))))))))
    (nreverse children)))

(defun ejira--push-create-cascaded-subtask (parent-key project-key child todo-keywords assign-self)
  "Create a Jira subtask for CHILD under PARENT-KEY in PROJECT-KEY.
CHILD is a plist with :marker :title :state :body.
TODO-KEYWORDS is the org-todo-keywords-1 list for state-transition lookup.
ASSIGN-SELF is the value (t/nil) of the parent's assign-self cell."
  (let* ((child-marker (plist-get child :marker))
         (orig-state   (plist-get child :state))
         (summary      (plist-get child :title))
         (desc         (ejira-parser-org-to-jira (or (plist-get child :body) "")))
         (result (jiralib2-create-issue
                  project-key ejira-subtask-type-name
                  summary desc
                  `(parent . ((key . ,parent-key)))))
         (new-key (ejira--alist-get result 'key)))
    (when assign-self
      (let ((my-name (cdr (assoc 'name (jiralib2-get-user-info)))))
        (when my-name
          (jiralib2-assign-issue new-key my-name))))
    (ejira--finalize-new-issue new-key child-marker orig-state todo-keywords)))

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
             ;; Skip headings marked with the org COMMENT keyword (and any
             ;; of their descendants, since COMMENT is inherited) — these
             ;; are explicitly excluded from export/agenda by the user and
             ;; must never be detected as new issues or pushed to Jira.
             (if (org-in-commented-heading-p)
                 nil
             ;; Rule H: PendingDelete (check first)
             (if (and (equal pending-delete "t") (equal type "ejira-comment"))
                 (push (list :op 'delete
                             :object 'comment
                             :key (org-entry-get nil "CommId")
                             :project (when-let ((ik (org-entry-get nil "ID" t))) (car (split-string ik "-")))
                             :parent-issue (org-entry-get nil "ID" t)
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
                 (let ((proj (car (split-string id "-"))))
                   (push (list :op 'update
                               :object 'issue
                               :key id
                               :project proj
                               :parent-issue id
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
                        (proj (car (split-string issue-key "-"))))
                   (push (list :op 'update
                               :object 'comment
                               :key commid
                               :project proj
                               :parent-issue issue-key
                               :marker marker
                               :data (list :issue-key issue-key
                                           :commid commid))
                         ops)))
               ;; Rule B: PendingTransition
               (when (and pending-transition (member type ejira-pushable-types))
                 (push (list :op 'update
                             :object 'status
                             :key id
                             :project (car (split-string id "-"))
                             :parent-issue id
                             :marker marker
                             :data (list :action-name pending-transition))
                       ops))
               ;; Rule C: PendingIssuetype
               (when pending-issuetype
                 (push (list :op 'update
                             :object 'issuetype
                             :key id
                             :project (car (split-string id "-"))
                             :parent-issue id
                             :marker marker
                             :data (list :new-type pending-issuetype
                                         :old-type (org-entry-get nil "Issuetype")))
                       ops))
               ;; Rule D: PendingEpic
               (when pending-epic
                 (push (list :op 'update
                             :object 'epic
                             :key id
                             :project (car (split-string id "-"))
                             :parent-issue id
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
                                    (org-entry-get nil "ID")
                                    (org-entry-get nil "Issuetype")))))
                         (parent-type       (nth 0 parent-info))
                         (parent-id         (nth 1 parent-info))
                         (parent-issuetype  (nth 2 parent-info))
                         (project-key       (when parent-id
                                              (car (split-string parent-id "-")))))
                    (cond
                     ;; Under Initiative (or other epic-parent type) → create Epic
                     ((and (equal parent-type "ejira-issue")
                           (member parent-issuetype ejira-epic-parent-issuetypes))
                      (let ((children (ejira--push-scan-issue-children marker parent-id)))
                        (push (list :op 'create
                                    :object 'issue
                                    :key nil
                                    :project project-key
                                    :parent-issue parent-id
                                    :marker marker
                                    :data (list :project-key project-key
                                                :issue-type ejira-epic-type-name
                                                :parent-initiative parent-id
                                                :children children))
                              ops)))
                     ;; Under Epic → create Task (or Story) with Epic Link
                     ((equal parent-type "ejira-epic")
                      (let ((children (ejira--push-scan-issue-children marker parent-id)))
                        (push (list :op 'create
                                    :object 'issue
                                    :key nil
                                    :project project-key
                                    :parent-issue parent-id
                                    :marker marker
                                    :data (list :project-key project-key
                                                :issue-type ejira-epic-child-type-name
                                                :parent-epic parent-id
                                                :children children))
                              ops)))
                     ;; Under Issue/Story → create Sub-task (Jira parent link)
                     ((member parent-type '("ejira-issue" "ejira-story"))
                      (push (list :op 'create
                                  :object 'subtask
                                  :key nil
                                  :project project-key
                                  :parent-issue parent-id
                                  :marker marker
                                  :data (list :parent-key parent-id
                                              :project-key project-key))
                            ops))
                     ;; Under Project → create issue (top-level)
                     ((equal parent-type "ejira-project")
                      (let ((children (ejira--push-scan-issue-children marker parent-id)))
                        (push (list :op 'create
                                    :object 'issue
                                    :key nil
                                    :project parent-id
                                    :parent-issue nil
                                    :marker marker
                                    :data (list :project-key parent-id
                                                :children children))
                              ops))))))
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
                        (proj (when issue-key (car (split-string issue-key "-")))))
                   (when issue-key
                     (push (list :op 'create
                                 :object 'comment
                                 :key nil
                                 :project proj
                                 :parent-issue issue-key
                                 :marker marker
                                 :data (list :issue-key issue-key))
                           ops))))))
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
              (apply #'jiralib2-jql-search
                     (format "key in (%s)" (s-join ", " keys))
                     '("summary" "description" "assignee" "priority" "duedate" "status"))))
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
                  (local-priority-char (org-with-point-at marker
                                         (org-back-to-heading t)
                                         (when (looking-at org-priority-regexp)
                                           (match-string 2))))
                  (local-priority-name (when local-priority-char
                                         (car (rassoc (org-priority-to-value local-priority-char)
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
                 (remote-status-name (when item (ejira--alist-get item 'fields 'status 'name)))
                 (local-state (org-with-point-at marker
                                (substring-no-properties (or (org-get-todo-state) ""))))
                 (todo-kws (org-with-point-at marker org-todo-keywords-1))
                 (local-state-jira-names
                  (when (and local-state todo-kws (boundp 'ejira-todo-states-alist))
                    (let* ((pos (cl-position local-state todo-kws :test #'string=))
                           (idx (when pos (1+ pos))))
                      (when idx
                        (delq nil (mapcar (lambda (e) (when (= (cdr e) idx) (car e)))
                                          ejira-todo-states-alist))))))
                 (state-matches (member remote-status-name local-state-jira-names))
                 (changes (ejira-confirm-field-changes
                           `(("summary"     ,remote-summary  ,local-summary)
                             ("description" ,(or remote-desc-org "") ,local-desc-org)
                             ("assignee"    ,remote-assignee ,local-assignee)
                             ,@(when local-priority-name
                                 `(("priority" ,(or remote-priority-name "") ,local-priority-name)))
                             ("deadline"    ,(or remote-deadline "") ,(or local-deadline ""))
                             ,@(when (and local-state remote-status-name (not state-matches))
                                 `(("state" ,(or remote-status-name "") ,local-state))))))
                 (summary-changed (assoc "summary" changes))
                 (desc-changed (assoc "description" changes))
                 (assignee-changed (assoc "assignee" changes))
                 (priority-changed (assoc "priority" changes))
                 (deadline-changed (assoc "deadline" changes))
                 (state-changed (assoc "state" changes)))
            (if changes
              (push (list :op 'update
                          :object 'issue
                          :project project
                          :title key
                          :parent-issue key
                          :changes changes
                          :send (let ((key key) (marker marker)
                                      (local-summary local-summary)
                                      (local-desc-org local-desc-org)
                                      (local-assignee local-assignee)
                                      (local-priority-name local-priority-name)
                                      (local-deadline local-deadline)
                                      (local-state local-state)
                                      (todo-kws todo-kws)
                                      (summary-changed summary-changed)
                                      (desc-changed desc-changed)
                                      (assignee-changed assignee-changed)
                                      (priority-changed priority-changed)
                                      (deadline-changed deadline-changed)
                                      (state-changed state-changed))
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
                                    (when state-changed
                                      (ejira--transition-to-org-state key local-state todo-kws))
                                    (ejira--push-finalize marker))))
                    plans)
              ;; No changes vs remote — re-baseline to clear the dirty hash.
              (when item
                (org-with-point-at marker (ejira--update-push-baseline))))))))
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
                 (comment-data (jiralib2-get-comment issue-key commid))
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
                               :parent-issue issue-key
                               :changes changes
                                :send (let ((issue-key issue-key) (commid commid) (marker marker)
                                            (body local-body))
                                        (lambda ()
                                          (jiralib2-edit-comment
                                           issue-key commid
                                           (ejira-parser-org-to-jira body))
                                          (ejira--push-finalize marker))))))))
         ((and (eq op-type 'update) (eq object 'status))
          (let ((action-name (plist-get data :action-name)))
            (setq plan (list :op 'update
                             :object 'status
                             :project project
                             :title key
                             :parent-issue key
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
                             :parent-issue key
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
                             :parent-issue key
                             :changes `(("epic" "" ,new-epic))
                              :send (let ((key key) (marker marker) (new-epic new-epic)
                                          (epic-field ejira-epic-field))
                                      (lambda ()
                                        (jiralib2-update-issue key `(,epic-field . ,new-epic))
                                       (ejira--update-task key)
                                       (org-with-point-at marker
                                         (org-delete-property "PendingEpic"))))))))
         ((and (eq op-type 'create) (eq object 'subtask))
          (let* ((parent-key (plist-get data :parent-key))
                 (project-key (plist-get data :project-key))
                 (heading-title (org-with-point-at marker
                                  (ejira--strip-properties (org-get-heading t t t t))))
                 (local-body (ejira--get-heading-body marker))
                 (local-state (org-with-point-at marker
                                (substring-no-properties (or (org-get-todo-state) ""))))
                 (fields `(("title" ,heading-title)
                           ("state" ,local-state)
                           ("description" ,(or local-body "")))))
             (setq plan (list :op 'create
                              :object 'subtask
                              :project project-key
                              :title (format "new subtask: %s"
                                             (substring heading-title
                                                        0 (min 60 (length heading-title))))
                              :parent-issue parent-key
                              :fields fields
                              :assign-self (list ejira--assign-new-issues)
                                :send (let ((marker marker) (project-key project-key)
                                            (parent-key parent-key)
                                            (summary heading-title)
                                            (desc (ejira-parser-org-to-jira (or local-body "")))
                                            (subtask-type ejira-subtask-type-name)
                                            (orig-state local-state)
                                            (todo-kws (org-with-point-at marker
                                                         (when (boundp 'org-todo-keywords-1)
                                                           org-todo-keywords-1)))
                                            (assign-self (list ejira--assign-new-issues)))
                                       (lambda ()
                                         (let* ((result (jiralib2-create-issue
                                                         project-key subtask-type
                                                         summary desc
                                                         `(parent . ((key . ,parent-key)))))
                                                (new-key (ejira--alist-get result 'key)))
                                           (when (car assign-self)
                                             (let ((my-name (cdr (assoc 'name (jiralib2-get-user-info)))))
                                               (when my-name
                                                 (jiralib2-assign-issue new-key my-name))))
                                           (ejira--finalize-new-issue
                                            new-key marker orig-state todo-kws))))))))

          ((and (eq op-type 'create) (eq object 'issue))
           (let* ((project-key      (plist-get data :project-key))
                  (children         (plist-get data :children))
                  (issue-type       (or (plist-get data :issue-type)
                                        (or ejira-story-type-name "Task")))
                  (parent-epic      (plist-get data :parent-epic))
                  (parent-initiative (plist-get data :parent-initiative))
                  (parent-issue     (or parent-epic parent-initiative
                                        (plist-get op :parent-issue)))
                  (heading-title (org-with-point-at marker
                                   (ejira--strip-properties (org-get-heading t t t t))))
                  (local-body (ejira--get-heading-body marker))
                  (local-state (org-with-point-at marker
                                 (substring-no-properties (or (org-get-todo-state) ""))))
                  (fields `(("title" ,heading-title)
                            ("state" ,local-state)
                            ("description" ,(or local-body ""))))
                  (is-epic (equal issue-type ejira-epic-type-name))
                  (label (cond (is-epic "epic")
                               (parent-epic "task")
                               (t "issue"))))
              (setq plan (list :op 'create
                              :object 'issue
                              :project project-key
                              :label label
                              :title (format "new %s: %s" label heading-title)
                              :parent-issue parent-issue
                              :fields fields
                              :children children
                              :assign-self (list ejira--assign-new-issues)
                               :send (let ((marker marker) (project-key project-key)
                                           (orig-state local-state)
                                           (summary heading-title)
                                           (desc (ejira-parser-org-to-jira (or local-body "")))
                                           (children children)
                                           (issue-type issue-type)
                                           (parent-epic parent-epic)
                                           (is-epic is-epic)
                                           (assign-self (list ejira--assign-new-issues))
                                           (epic-field ejira-epic-field)
                                           (epic-summary-field ejira-epic-summary-field)
                                           (todo-kws (org-with-point-at marker
                                                        (when (boundp 'org-todo-keywords-1)
                                                          org-todo-keywords-1))))
                                      (lambda ()
                                        (let* ((epic-name-arg
                                                (when (and is-epic epic-summary-field)
                                                  `(,epic-summary-field . ,summary)))
                                               (result (apply #'jiralib2-create-issue
                                                              project-key issue-type
                                                              summary desc
                                                              (delq nil (list epic-name-arg))))
                                               (new-key (ejira--alist-get result 'key)))
                                          (when (and parent-epic epic-field)
                                            (jiralib2-update-issue
                                             new-key `(,epic-field . ,parent-epic)))
                                          (when (and parent-epic (not epic-field))
                                            (display-warning
                                             'ejira
                                             "ejira-epic-field is nil — run `ejira-guess-epic-sprint-fields' to auto-configure.  Epic Link not set for new issue."
                                             :warning))
                                          (when (and is-epic (not epic-summary-field))
                                            (display-warning
                                             'ejira
                                             "ejira-epic-summary-field is nil — run `ejira-guess-epic-sprint-fields' to auto-configure.  Epic Name not set for new Epic."
                                             :warning))
                                          (when (car assign-self)
                                            (let ((my-name (cdr (assoc 'name (jiralib2-get-user-info)))))
                                              (when my-name
                                                (jiralib2-assign-issue new-key my-name))))
                                          (ejira--finalize-new-issue
                                           new-key marker orig-state todo-kws)
                                          (dolist (child children)
                                            (condition-case err
                                                (ejira--push-create-cascaded-subtask
                                                 new-key project-key child todo-kws
                                                 (car assign-self))
                                              (error
                                               (display-warning
                                                'ejira
                                                (format "cascade subtask failed for %s: %s"
                                                        (plist-get child :title)
                                                        (error-message-string err))
                                                :error)))))))))))
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
                              :parent-issue issue-key
                              :preview preview
                              :send (let ((marker marker) (issue-key issue-key)
                                          (body (ejira--get-heading-body marker)))
                                      (lambda ()
                                        (let* ((comment (ejira--parse-comment
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
                             :parent-issue issue-key
                             :preview (or body "(empty)")
                             :send (let ((issue-key issue-key) (commid commid) (marker marker))
                                     (lambda ()
                                       (jiralib2-delete-comment issue-key commid)
                                       (org-with-point-at marker
                                         (ejira--with-expand-all
                                            (org-cut-subtree))))))))))
         (when plan (push plan plans))))
    (nreverse plans)))

(defmacro ejira--with-pre-scan (buf &rest body)
  "Bind `ejira--pre-scanning' t while scanning BUF for pending operations.
Causes all `ejira--with-expand-all' calls to skip their per-call
`outline-show-all', which is safe because org structural navigation
(org-goto-first-child, re-search-forward, org-narrow-to-subtree, etc.)
operates on buffer text regardless of fold state."
  (declare (indent 1))
  `(with-current-buffer ,buf
     (let ((ejira--pre-scanning t))
       ,@body)))

(defun ejira-push-at-point ()
  "Scan the ejira heading at point and show ejira-confirm for it."
  (interactive)
  (ejira--with-pre-scan (current-buffer)
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
        (message "ejira: nothing to push at point")))))

(defun ejira--push-on-save ()
  "Offer to push locally-edited ejira items after saving a managed buffer."
  (when (and ejira-push-on-save
             (not ejira--pushing)
             (not ejira--syncing)
             (derived-mode-p 'org-mode)
             (ejira--buffer-has-pushable-p))
    (ejira--with-pre-scan (current-buffer)
      (let* ((ops   (ejira--push-scan-buffer (current-buffer)))
             (plans (when ops (ejira--push-build-plans ops))))
        (when plans
          (ejira-confirm-show plans))))))

(add-hook 'after-save-hook #'ejira--push-on-save)

(provide 'ejira-push)
;;; ejira-push.el ends here
