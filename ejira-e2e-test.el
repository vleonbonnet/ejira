;;; ejira-e2e-test.el --- End-to-end tests against the configured Jira test server  -*- lexical-binding: t -*-

;; Set EJIRA_E2E_URL, EJIRA_E2E_PROJECT, and EJIRA_E2E_TOKEN before running.
;; The test suite creates a temporary issue on that server, exercises push
;; operations, then cancels it.
;;
;; Run: M-x ejira-e2e-run

;;; Code:

(require 'ejira-core)
(require 'ejira-push)
(require 'ejira-confirm)

;;; ── Test-server configuration ────────────────────────────────────────────────

(defconst ejira-e2e--url-env "EJIRA_E2E_URL")
(defconst ejira-e2e--project-env "EJIRA_E2E_PROJECT")
(defconst ejira-e2e--token-env "EJIRA_E2E_TOKEN")

(defun ejira-e2e--required-env (name)
  "Return environment variable NAME or signal a configuration error."
  (or (getenv name)
      (error "Set %s before running ejira-e2e-run" name)))

(defmacro ejira-e2e--with-test-server (&rest body)
  "Evaluate BODY with jiralib2 and ejira redirected to a test server.
A temporary org directory is used so production org files are untouched.
The directory is deleted after BODY completes."
  (declare (indent 0))
  `(let* ((jiralib2-url     (or (getenv ejira-e2e--url-env)
                                "http://localhost:8080"))
          (jiralib2-token   (ejira-e2e--required-env ejira-e2e--token-env))
          (jiralib2-auth    'bearer)
          (jiralib2--session nil)
          (jiralib2--projects-cache nil)
          (jiralib2--issuetypes-cache nil)
          (jiralib2--users-cache nil)
          (ejira--priority-scheme-cache (make-hash-table :test #'equal))
          (ejira-projects   (list (or (getenv ejira-e2e--project-env)
                                      "EJIRA")))
          (ejira-org-directory (make-temp-file "ejira-e2e-" t))
          ;; Shadow agenda files so the temp file is never added to the real list.
          (org-agenda-files org-agenda-files))
     (unwind-protect
         (progn ,@body)
       (delete-directory ejira-org-directory t))))

;;; ── Result reporting ─────────────────────────────────────────────────────────

(defvar ejira-e2e--results nil)
(defvar ejira-e2e--issue-key nil)
(defvar ejira-e2e--subtask-key nil)

(defun ejira-e2e--pass (label)
  (push (cons label 'pass) ejira-e2e--results)
  (message "  ✓ %s" label))

(defun ejira-e2e--fail (label reason)
  (push (cons label (cons 'fail reason)) ejira-e2e--results)
  (message "  ✗ %s: %s" label reason))

(defmacro ejira-e2e--check (label &rest body)
  "Evaluate BODY and record LABEL as PASS or FAIL.  Failures do not abort."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body (ejira-e2e--pass ,label))
     (error (ejira-e2e--fail ,label (error-message-string err)))))

;;; ── Navigation helpers ───────────────────────────────────────────────────────

(defun ejira-e2e--find-heading (key)
  "Find heading marker for KEY, refreshing the project file's org-id first."
  (let* ((proj (car (split-string key "-")))
         (file (expand-file-name (ejira--project-file-name proj))))
    (when (file-exists-p file)
      (org-id-update-id-locations (list file) t)))
  (ejira--find-heading key))

(defun ejira-e2e--find-comment-heading (issue-key)
  "Return marker of the first CommId-stamped comment under ISSUE-KEY's Comments."
  (let* ((issue-m    (ejira-e2e--find-heading issue-key))
         (comments-m (when issue-m
                       (org-with-point-at issue-m
                         (ejira--find-child-heading ejira-comments-heading-name)))))
    (when comments-m
      (org-with-point-at comments-m
        (ejira--find-child-heading-predicate
         (lambda ()
           (and (org-entry-get nil "CommId")
                (equal (org-entry-get nil "TYPE") "ejira-comment"))))))))

;;; ── Push pipeline helper (bypasses interactive confirm) ─────────────────────

(defun ejira-e2e--push-buffer (buf)
  "Scan BUF, build plans, execute every :send thunk; return plan count."
  (let* ((ops   (ejira--push-scan-buffer buf))
         (plans (when ops (ejira--push-build-plans ops))))
    (dolist (plan plans)
      (funcall (plist-get plan :send)))
    (length plans)))

;;; ── Jira query helpers ───────────────────────────────────────────────────────

(defun ejira-e2e--get-field (key field)
  "Fetch KEY from Jira and return FIELD (a symbol) from :fields."
  (alist-get field (alist-get 'fields (jiralib2-get-issue key))))

(defun ejira-e2e--get-comments (key)
  "Return list of comment body strings for issue KEY."
  (let ((issue (jiralib2-get-issue key)))
    (mapcar (lambda (c) (alist-get 'body c))
            (alist-get 'comments
                       (alist-get 'comment
                                  (alist-get 'fields issue))))))

(defun ejira-e2e--get-subtasks (key)
  "Return list of subtask alists for issue KEY."
  (let ((issue (jiralib2-get-issue key)))
    (alist-get 'subtasks (alist-get 'fields issue))))

;;; ── Cleanup helper ───────────────────────────────────────────────────────────

(defun ejira-e2e--cancel-issue (key)
  "Transition KEY to Cancelled, stepping via To Do if needed."
  (condition-case _
      (jiralib2-do-action key "121")
    (error
     (jiralib2-do-action key "11")
     (jiralib2-do-action key "121"))))

;;; ── Individual scenario steps ────────────────────────────────────────────────

(defun ejira-e2e--step-create ()
  (message "\n[1] Create test issue on test server")
  (ejira-e2e--check "create issue on test server"
                    (let ((result (jiralib2-create-issue
                                   ejira-e2e--project "Task"
                                   "ejira-e2e-test: DO NOT EDIT — automated test issue"
                                   "Created by ejira-e2e-test.el.  Will be cancelled automatically.")))
                      (setq ejira-e2e--issue-key (alist-get 'key result))
                      (unless ejira-e2e--issue-key (error "No key in create response"))))
  (ejira-e2e--check "pull issue into ejira org"
                    (ejira--update-task ejira-e2e--issue-key)
                    (unless (ejira-e2e--find-heading ejira-e2e--issue-key)
                      (error "Heading not found in org after pull"))))

(defun ejira-e2e--step-update-content ()
  (message "\n[2] Update summary and description")
  (let* ((key ejira-e2e--issue-key)
         (m   (or (ejira-e2e--find-heading key)
                  (error "Cannot find heading for %s" key)))
         (buf (marker-buffer m)))
    (ejira-e2e--check "modify summary locally"
                      (ejira--set-heading-summary m "ejira-e2e-test: DO NOT EDIT — UPDATED")
                      (org-with-point-at m (org-set-property "Pushhash" "DIRTY-FOR-E2E")))
    (ejira-e2e--check "scan+push content update to Jira"
                      (let ((n (ejira-e2e--push-buffer buf)))
                        (unless (> n 0) (error "No plans were pushed"))))
    (ejira-e2e--check "Jira summary reflects local change"
                      (let ((remote (ejira-e2e--get-field key 'summary)))
                        (unless (string-match-p "UPDATED" remote)
                          (error "Summary not updated on Jira: %s" remote))))))

(defun ejira-e2e--step-priority ()
  (message "\n[3] Update priority by exact Jira ID")
  (let* ((key ejira-e2e--issue-key)
         (m   (ejira-e2e--find-heading key))
         (buf (marker-buffer m))
         (scheme (ejira--get-priority-scheme key))
         (current (ejira-e2e--get-field key 'priority))
         (current-id (ejira--priority-id-string (alist-get 'id current)))
         (target (cl-find-if (lambda (entry)
                               (not (equal current-id (plist-get entry :id))))
                             scheme)))
    (ejira-e2e--check "stage exact priority ID locally"
                      (unless (and m target) (error "No alternative Jira priority available"))
                      (org-with-point-at m
                        (let ((rank (1+ (cl-position target scheme :test #'equal))))
                          (ejira--ensure-org-priority-range rank)
                          (org-priority (ejira--org-priority-for-rank rank))
                          (org-set-property ejira-priority-id-property (plist-get target :id))
                          (org-set-property ejira-priority-name-property (plist-get target :name))
                          (org-set-property "Pushhash" "DIRTY-FOR-E2E"))))
    (ejira-e2e--check "push exact priority ID to Jira"
                      (let ((n (ejira-e2e--push-buffer buf)))
                        (unless (> n 0) (error "No plans were pushed for priority"))))
    (ejira-e2e--check "Jira priority ID reflects local choice"
                      (let ((remote (ejira-e2e--get-field key 'priority)))
                        (unless (equal (plist-get target :id)
                                       (ejira--priority-id-string (alist-get 'id remote)))
                          (error "Jira priority ID was not updated: %S" remote))))
    (ejira-e2e--check "pull preserves priority ID and rank"
                      (ejira--update-task key)
                      (org-with-point-at m
                        (unless (equal (plist-get target :id)
                                       (org-entry-get (point-marker) ejira-priority-id-property))
                          (error "Org priority metadata did not round-trip"))))))

(defun ejira-e2e--step-create-comment ()
  (message "\n[4] Create comment via draft heading")
  (let* ((key ejira-e2e--issue-key)
         (m   (ejira-e2e--find-heading key))
         (buf (marker-buffer m)))
    (ejira-e2e--check "add draft heading under Comments"
                      (unless m (error "Heading not found for %s" key))
                      (org-with-point-at m
                        (ejira--with-expand-all
                          (let ((comments-m (ejira--get-subheading m ejira-comments-heading-name)))
                            (org-with-point-at comments-m
                              (org-insert-heading-respect-content t)
                              (insert "E2E test comment")
                              (org-demote-subtree)
                              (forward-line 1)
                              (insert "This comment was posted by ejira-e2e-test.el.\n"))))))
    (ejira-e2e--check "scan+push creates comment on Jira"
                      (let ((n (ejira-e2e--push-buffer buf)))
                        (unless (> n 0) (error "No plans were pushed"))))
    (ejira-e2e--check "comment present on Jira"
                      (let ((comments (ejira-e2e--get-comments key)))
                        (unless (cl-some (lambda (c) (string-match-p "ejira-e2e-test" c)) comments)
                          (error "Comment not found on Jira"))))))

(defun ejira-e2e--step-update-comment ()
  (message "\n[5] Update existing comment")
  (let* ((key ejira-e2e--issue-key)
         (buf (marker-buffer (ejira-e2e--find-heading key))))
    (ejira--update-task key)
    (ejira-e2e--check "dirty comment body and push"
                      (let ((comment-m (or (ejira-e2e--find-comment-heading key)
                                           (error "No CommId-stamped comment heading found"))))
                        (org-with-point-at comment-m
                          (ejira--set-heading-body comment-m "UPDATED by ejira-e2e-test.el.\n")
                          (org-set-property "Pushhash" "DIRTY-FOR-E2E")))
                      (let ((n (ejira-e2e--push-buffer buf)))
                        (unless (> n 0) (error "No plans pushed for comment update"))))
    (ejira-e2e--check "comment body updated on Jira"
                      (let ((comments (ejira-e2e--get-comments key)))
                        (unless (cl-some (lambda (c) (string-match-p "UPDATED by ejira-e2e" c)) comments)
                          (error "Updated comment not found on Jira"))))))

(defun ejira-e2e--step-delete-comment ()
  (message "\n[6] Delete comment")
  (let* ((key ejira-e2e--issue-key)
         (buf (marker-buffer (ejira-e2e--find-heading key))))
    (ejira-e2e--check "stage comment deletion via PendingDelete"
                      (let ((comment-m (or (ejira-e2e--find-comment-heading key)
                                           (error "No comment heading found"))))
                        (org-with-point-at comment-m
                          (org-set-property "PendingDelete" "t"))))
    (ejira-e2e--check "scan+push deletes comment on Jira"
                      (let ((n (ejira-e2e--push-buffer buf)))
                        (unless (> n 0) (error "No plans pushed for delete"))))
    (ejira-e2e--check "comment gone from Jira"
                      (let ((comments (ejira-e2e--get-comments key)))
                        (when (cl-some (lambda (c) (string-match-p "ejira-e2e-test" c)) comments)
                          (error "Comment still present on Jira after delete"))))))

(defun ejira-e2e--step-create-subtask ()
  (message "\n[7] Create subtask")
  (let* ((key ejira-e2e--issue-key)
         (m   (ejira-e2e--find-heading key))
         (buf (marker-buffer m)))
    (ejira-e2e--check "add TODO heading under issue"
                      (unless m (error "Heading not found for %s" key))
                      (org-with-point-at m
                        (ejira--with-expand-all
                          (org-insert-heading-respect-content t)
                          (insert "E2E subtask: auto-created by test")
                          (org-demote-subtree)
                          (org-todo (car org-todo-keywords-1)))))
    (ejira-e2e--check "scan+push creates subtask on Jira"
                      (let ((n (ejira-e2e--push-buffer buf)))
                        (unless (> n 0) (error "No plans pushed for subtask create"))))
    (ejira-e2e--check "subtask present on Jira"
                      (let ((subtasks (ejira-e2e--get-subtasks key)))
                        (unless (> (length subtasks) 0)
                          (error "No subtasks found on Jira for %s" key))
                        (setq ejira-e2e--subtask-key (alist-get 'key (car subtasks)))))))

(defun ejira-e2e--step-cleanup ()
  (message "\n[8] Cleanup")
  (ejira-e2e--check "cancel test issue on test server"
                    (when ejira-e2e--issue-key
                      (ejira-e2e--cancel-issue ejira-e2e--issue-key)
                      (setq ejira-e2e--issue-key   nil
                            ejira-e2e--subtask-key nil))))

;;; ── Main entry point ─────────────────────────────────────────────────────────

;;;###autoload
(defun ejira-e2e-run ()
  "Run ejira end-to-end tests against the configured Jira test server.
Creates a temporary EJIRA issue on test server, exercises all push operations,
then cancels it.  Production Jira and org files are never touched."
  (interactive)
  (unless (featurep 'ejira-push) (error "ejira-push not loaded"))
  (setq ejira-e2e--results    nil
        ejira-e2e--issue-key  nil
        ejira-e2e--subtask-key nil)
  (message "=== ejira E2E test run (test server) ===")
  (ejira-e2e--with-test-server
   (unwind-protect
       (progn
         (ejira-e2e--step-create)
         (when ejira-e2e--issue-key
           (ejira-e2e--step-update-content)
           (ejira-e2e--step-priority)
           (ejira-e2e--step-create-comment)
           (ejira-e2e--step-update-comment)
           (ejira-e2e--step-delete-comment)
           (ejira-e2e--step-create-subtask)))
     (ejira-e2e--step-cleanup)))
  (let* ((all    (reverse ejira-e2e--results))
         (passed (cl-count-if (lambda (r) (eq 'pass (cdr r))) all))
         (failed (cl-count-if (lambda (r) (consp (cdr r))) all)))
    (message "\n=== Results: %d/%d passed ===" passed (length all))
    (dolist (r all)
      (if (eq 'pass (cdr r))
          (message "  ✓ %s" (car r))
        (message "  ✗ %s — %s" (car r) (cddr r))))
    (when (> failed 0)
      (message "\n%d test(s) FAILED" failed))
    (list :passed passed :failed failed :total (length all))))

(provide 'ejira-e2e-test)
;;; ejira-e2e-test.el ends here
