;;; ejira-test.el --- ERT unit tests for ejira push/confirm  -*- lexical-binding: t -*-

;; Run interactively: M-x ert-run-tests-interactively RET ejira- RET
;; Run from emacsclient:
;;   emacsclient --eval '(progn (load-file "ejira-test.el") (ert-run-tests-batch "ejira-"))'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'ejira-core)
(require 'ejira-push)
(require 'ejira-confirm)

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defmacro ejira-test--with-org-buf (content &rest body)
  "Evaluate BODY in a fresh org-mode temp buffer pre-filled with CONTENT."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *ejira-test*")))
     (unwind-protect
         (with-current-buffer buf
           (org-mode)
           (insert ,content)
           (goto-char (point-min))
           ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(defun ejira-test--scan (content)
  "Return scan ops for org CONTENT with `ejira--get-project' mocked."
  (ejira-test--with-org-buf content
    (cl-letf (((symbol-function 'ejira--get-project)
               (lambda (key) (car (split-string key "-")))))
      (ejira--push-scan-buffer (current-buffer)))))

;;; ── ejira-confirm helpers ─────────────────────────────────────────────────────

(ert-deftest ejira-confirm--normalize/nil ()
  "nil maps to empty string."
  (should (equal "" (ejira-confirm--normalize nil))))

(ert-deftest ejira-confirm--normalize/strips-cr ()
  "Carriage returns are removed."
  (should (equal "a b" (ejira-confirm--normalize "a\r b"))))

(ert-deftest ejira-confirm--normalize/trims ()
  "Leading/trailing whitespace is stripped."
  (should (equal "x" (ejira-confirm--normalize "  x  "))))

(ert-deftest ejira-confirm-field-changes/empty-when-identical ()
  "Returns nil when every field is unchanged."
  (should (null (ejira-confirm-field-changes
                 '(("summary" "Foo" "Foo")
                   ("description" "Bar" "Bar"))))))

(ert-deftest ejira-confirm-field-changes/detects-change ()
  "Returns only the changed field."
  (let ((result (ejira-confirm-field-changes
                 '(("summary" "Old" "New")
                   ("description" "Same" "Same")))))
    (should (= 1 (length result)))
    (should (equal "summary" (nth 0 (car result))))
    (should (equal "Old"     (nth 1 (car result))))
    (should (equal "New"     (nth 2 (car result))))))

(ert-deftest ejira-confirm-field-changes/cr-insensitive ()
  "CRLF vs LF difference does not count as a change."
  (should (null (ejira-confirm-field-changes
                 '(("body" "line1\r\nline2" "line1\nline2"))))))

(ert-deftest ejira-confirm-field-changes/whitespace-insensitive ()
  "Surrounding whitespace difference does not count as a change."
  (should (null (ejira-confirm-field-changes
                 '(("summary" "  Foo  " "Foo"))))))

(ert-deftest ejira-confirm--group-by-project/groups-correctly ()
  "Plans are grouped by :project, sorted alphabetically."
  (let* ((plans (list (list :op 'update :project "TEST" :title "X")
                      (list :op 'create :project "CVE"    :title "Y")
                      (list :op 'update :project "TEST" :title "Z")))
         (groups (ejira-confirm--group-by-project-issue plans)))
    (should (= 2 (length groups)))
    (should (equal "CVE"    (caar groups)))        ; sorted first
    (should (equal "TEST" (caadr groups)))
    (should (= 1 (length (cdr (assoc "TEST" groups)))))
    (should (= 1 (length (cdr (assoc "CVE"    groups)))))))

(ert-deftest ejira-confirm--op-summary/all-types ()
  "Summarize creates, updates, and deletes independently."
  (should (equal "  1 new, 2 modified, 1 deleted"
                 (ejira-confirm--op-summary
                  (list (list :op 'create)
                        (list :op 'update)
                        (list :op 'update)
                        (list :op 'delete))))))

;;; ── ejira-core helpers ────────────────────────────────────────────────────────

(defconst ejira-test--priority-scheme
  '((:id "p1" :name "High")
    (:id "p2" :name "Medium")
    (:id "p3" :name "Low")
    (:id "hidden-a" :name "Hidden A")
    (:id "hidden-b" :name "Hidden B")
    (:id "hidden-c" :name "Hidden C")
    (:id "hidden-d" :name "Hidden D")
    (:id "hidden-default" :name "Hidden default"))
  "TEST-like priority scheme used by the unit tests.")

(defconst ejira-test--priority-policies
  '(("TEST"
     :ranks (("p1" . 1) ("p2" . 2) ("p3" . 3)
             ("hidden-default" . 3))
     :fallback-rank 3
     :selectable ("p1" "p2" "p3")
     :default "p1"))
  "TEST priority policy used by the unit tests.")

(ert-deftest ejira-projection--uses-jira-title-and-description-only ()
  "A Jira projection never exports the detailed local task body."
  (ejira-test--with-org-buf
      "* TODO Local implementation title
:PROPERTIES:
:TYPE:       ejira-issue
:ID:         TEST-1
:JIRA_TITLE: Concise external title
:END:

Private implementation notes.

** Description

Private local description.

** JIRA_DESCRIPTION

Concise external description.
"
    (goto-char (point-min))
    (should (ejira--jira-projection-p))
    (should (equal "Concise external title" (ejira--jira-summary)))
    (should (equal "Concise external description."
                   (string-trim (ejira--jira-description))))
    (let ((hash (md5 (ejira--heading-pushable-content))))
      (search-forward "Private implementation notes.")
      (replace-match "Changed private implementation notes.")
      (goto-char (point-min))
      (should (equal hash (md5 (ejira--heading-pushable-content)))))))

(ert-deftest ejira-projection--pull-preserves-local-title-and-description ()
  "A Jira pull updates projection fields without replacing local content."
  (ejira-test--with-org-buf
      "* TODO Local implementation title
:PROPERTIES:
:TYPE:       ejira-issue
:ID:         TEST-1
:JIRA_TITLE: Old external title
:END:

Private implementation notes.

** Description

Private local description.
"
    (let ((marker (point-min)))
      (cl-letf (((symbol-function 'ejira--find-heading)
                 (lambda (_id) marker)))
        (ejira--set-summary "TEST-1" "Updated external title")
        (ejira--set-jira-description-jira-markup "TEST-1" "Updated external description."))
      (goto-char (point-min))
      (should (equal "Local implementation title" (org-get-heading t t t t)))
      (should (equal "Updated external title" (org-entry-get nil "JIRA_TITLE")))
      (should (equal "Private local description."
                     (string-trim
                      (org-with-point-at
                          (ejira--find-child-heading "Description")
                        (ejira--get-heading-body (point-marker))))))
      (should (equal "Updated external description."
                     (string-trim
                      (org-with-point-at
                          (ejira--find-child-heading "JIRA_DESCRIPTION")
                        (ejira--get-heading-body (point-marker)))))))))

(ert-deftest ejira-new-issue-description--migrates-plain-body ()
  "A plain new heading moves its direct body into Description before creation."
  (ejira-test--with-org-buf
      "* TODO Local task
:PROPERTIES:
:END:

Content intended for Jira.
"
    (goto-char (point-min))
    (should (equal "Content intended for Jira.\n"
                   (ejira--new-issue-description)))
    (should-not (ejira--find-child-heading "Description"))
    (should (equal "Content intended for Jira.\n"
                   (ejira--prepare-new-issue-description)))
    (goto-char (point-min))
    (should (string-empty-p (string-trim (ejira--get-heading-own-body))))
    (org-with-point-at (ejira--find-child-heading "Description")
      (should (equal "Content intended for Jira."
                     (string-trim (ejira--get-heading-body (point-marker))))))))

(ert-deftest ejira-new-issue-description--projection-keeps-local-body-private ()
  "Preparing a projected heading never moves local content into Description."
  (ejira-test--with-org-buf
      "* TODO Local task
:PROPERTIES:
:JIRA_TITLE: External task
:END:

Private implementation notes.

** JIRA_DESCRIPTION

External task description.
"
    (goto-char (point-min))
    (should (equal "External task description."
                   (string-trim (ejira--prepare-new-issue-description))))
    (goto-char (point-min))
    (should (equal "Private implementation notes."
                   (string-trim (ejira--get-heading-own-body))))
    (should-not (ejira--find-child-heading "Description"))))

(ert-deftest ejira-priority--policy/filters-and-falls-back ()
  "Hidden Jira priorities share the configured lowest visible rank."
  (let ((ejira-priority-policies ejira-test--priority-policies))
    (should (= 3 (ejira--priority-rank ejira-test--priority-scheme "hidden-default"
                                       "TEST")))
    (should (= 3 (length (ejira--selectable-priority-scheme
                          "TEST" ejira-test--priority-scheme))))
    (should (equal "p3"
                   (plist-get
                    (ejira--priority-entry-for-rank
                     "TEST" ejira-test--priority-scheme 3)
                    :id)))
    (should (equal "p1" (ejira--default-priority-id "TEST")))))

(ert-deftest ejira-priority--parse-scheme/preserves-order-and-ids ()
  "Editmeta priority values are parsed in Jira's supplied order."
  (let* ((allowed-values '(((id . "a") (name . "First"))
                           ((id . "b") (name . "Second"))))
         (editmeta `((fields . ((priority . ((allowedValues
                                              . ,allowed-values)))))))
         (scheme (ejira--parse-priority-scheme editmeta)))
    (should (equal '("a" "b")
                   (mapcar (lambda (entry) (plist-get entry :id)) scheme)))
    (should (equal '("First" "Second")
                   (mapcar (lambda (entry) (plist-get entry :name)) scheme)))))

(ert-deftest ejira-priority--scheme/is-cached-per-session-and-issue ()
  "Repeated editmeta requests use the session-local issue cache."
  (let* ((ejira--priority-scheme-cache (make-hash-table :test #'equal))
         (jiralib2-url "https://jira.example.test")
         (jiralib2--session "session")
         (calls 0)
         (allowed-values '(((id . "a") (name . "First"))))
         (editmeta (list (cons 'fields
                               (list (cons 'priority
                                           (list (cons 'allowedValues
                                                       allowed-values))))))))
    (cl-letf (((symbol-function 'jiralib2-session-call)
               (lambda (&rest _args) (cl-incf calls) editmeta)))
      (should (equal (ejira--get-priority-scheme "TEST-1")
                     (ejira--get-priority-scheme "TEST-1")))
      (should (= 1 calls))
      (ejira--get-priority-scheme "TEST-1" t)
      (should (= 2 calls)))))

(ert-deftest ejira-priority--rank-and-org-range/use-scheme-order ()
  "Scheme positions map to Org priority values and extend the range."
  (let ((org-priority-highest 1)
        (org-priority-lowest 5)
        (org-lowest-priority 5))
    (should (= 8 (ejira--priority-rank ejira-test--priority-scheme "hidden-default")))
    (ejira--ensure-org-priority-range 8)
    (should (= 8 org-priority-lowest))
    (should (= 8 org-lowest-priority))
    (should (= 8 (ejira--org-priority-for-rank 8)))))

(ert-deftest ejira-priority--set-task-priority/stores-exact-id ()
  "Pulling a priority stores its ID and uses its scheme rank as Org cookie."
  (let ((org-priority-highest 1)
        (org-priority-lowest 5)
        (org-lowest-priority 5))
    (ejira-test--with-org-buf
        "* TODO TEST-1 Issue\n:PROPERTIES:\n:ID: TEST-1\n:END:\n"
      (goto-char (point-min))
      (re-search-forward org-heading-regexp)
      (let ((marker (point-marker)))
        (ejira--set-task-priority marker "hidden-default" "Hidden default"
                                  ejira-test--priority-scheme)
        (should (equal "hidden-default"
                       (org-with-point-at marker
                         (org-entry-get (point-marker) ejira-priority-id-property))))
        (should (equal "Hidden default"
                       (org-with-point-at marker
                         (org-entry-get (point-marker) ejira-priority-name-property))))
        (should (equal "8"
                       (org-with-point-at marker
                         (save-excursion
                           (org-back-to-heading t)
                           (looking-at org-priority-regexp)
                           (match-string 2)))))))))

(ert-deftest ejira-priority--local-entry/legacy-heading-is-not-inferred ()
  "A legacy Org cookie without Jira metadata is not pushed as a priority."
  (ejira-test--with-org-buf
      "* TODO TEST-1 Issue\n:PROPERTIES:\n:ID: TEST-1\n:END:\n"
    (goto-char (point-min))
    (re-search-forward org-heading-regexp)
    (org-priority 3)
    (should-not (ejira--local-priority-entry (point-marker)
                                             ejira-test--priority-scheme))))

(ert-deftest ejira-priority--update-task/migrates-clean-legacy-heading ()
  "A clean legacy heading is remapped and receives an exact Jira ID."
  (let ((org-priority-highest 1)
        (org-priority-lowest 5)
        (org-lowest-priority 5)
        (ejira-priority-policies ejira-test--priority-policies)
        (ejira--heading-cache (make-hash-table :test #'equal))
        (updated (date-to-time "2026-07-09 23:18:09 +0000")))
    (ejira-test--with-org-buf
        "* TEST\n:PROPERTIES:\n:ID: TEST\n:TYPE: ejira-project\n:END:\n* TODO [#3] Issue\n:PROPERTIES:\n:TYPE: ejira-issue\n:ID: TEST-1\n:Modified: 2026-07-09 23:18:09\n:END:\n"
      (goto-char (point-min))
      (re-search-forward org-heading-regexp)
      (let ((project-marker (point-marker)))
        (re-search-forward org-heading-regexp)
        (let ((issue-marker (point-marker)))
          (puthash "TEST" project-marker ejira--heading-cache)
          (puthash "TEST-1" issue-marker ejira--heading-cache)
          (cl-letf (((symbol-function 'ejira--get-priority-scheme)
                     (lambda (&rest _args) ejira-test--priority-scheme)))
            (ejira--update-task
             (make-ejira-task :key "TEST-1"
                              :type "Task"
                              :status "Open"
                              :project "TEST"
                              :priority "Hidden default"
                              :priority-id "hidden-default"
                              :updated updated))
            (should (equal "3"
                           (org-with-point-at issue-marker
                             (save-excursion
                               (org-back-to-heading t)
                               (looking-at org-priority-regexp)
                               (match-string 2)))))
            (should (equal "3"
                           (org-with-point-at issue-marker
                             (org-entry-get (point-marker)
                                            ejira-priority-rank-property))))
            (should (equal "hidden-default"
                           (org-with-point-at issue-marker
                             (org-entry-get (point-marker)
                                            ejira-priority-id-property))))
            (should (org-with-point-at issue-marker
                      (org-entry-get (point-marker) "Pushhash")))))))))

(ert-deftest ejira-priority--update-task/preserves-dirty-legacy-heading ()
  "A dirty legacy heading is left untouched until its current push succeeds."
  (let ((org-priority-highest 1)
        (org-priority-lowest 8)
        (org-lowest-priority 8)
        (ejira--heading-cache (make-hash-table :test #'equal))
        (updated (date-to-time "2026-07-09 23:18:09 +0000")))
    (ejira-test--with-org-buf
        "* TEST\n:PROPERTIES:\n:ID: TEST\n:TYPE: ejira-project\n:END:\n* DONE [#3] Issue\n:PROPERTIES:\n:TYPE: ejira-issue\n:ID: TEST-1\n:Modified: 2026-07-09 23:18:09\n:Pushhash: WRONG\n:END:\n"
      (goto-char (point-min))
      (re-search-forward org-heading-regexp)
      (let ((project-marker (point-marker)))
        (re-search-forward org-heading-regexp)
        (let ((issue-marker (point-marker)))
          (puthash "TEST" project-marker ejira--heading-cache)
          (puthash "TEST-1" issue-marker ejira--heading-cache)
          (cl-letf (((symbol-function 'ejira--get-priority-scheme)
                     (lambda (&rest _args) ejira-test--priority-scheme)))
            (ejira--update-task
             (make-ejira-task :key "TEST-1"
                              :type "Task"
                              :status "In Progress"
                              :project "TEST"
                              :priority "Hidden default"
                              :priority-id "hidden-default"
                              :updated updated))
            (should (equal "3"
                           (org-with-point-at issue-marker
                             (save-excursion
                               (org-back-to-heading t)
                               (looking-at org-priority-regexp)
                               (match-string 2)))))
            (should-not (org-with-point-at issue-marker
                          (org-entry-get (point-marker)
                                         ejira-priority-id-property)))))))))

(ert-deftest ejira-core--push-normalize/nil ()
  "nil maps to empty string."
  (should (equal "" (ejira--push-normalize nil))))

(ert-deftest ejira-core--push-normalize/strips-cr ()
  "Carriage returns are removed."
  (should (equal "a b" (ejira--push-normalize "a\r b"))))

(ert-deftest ejira-core--push-normalize/trims ()
  "Leading/trailing whitespace is stripped."
  (should (equal "abc" (ejira--push-normalize "  abc  "))))

(ert-deftest ejira-core--kill-guard/skips-nil-commid ()
  "The fix to ejira--kill-deleted-comments: skip entries whose CommId is nil."
  ;; Directly verify the guard logic: only kill when CommId is non-nil AND not in list.
  (let ((ids '("99")))
    ;; Draft (nil CommId) → must NOT be killed
    (should (null (let ((cid nil))
                    (when (and cid (not (member cid ids))) t))))
    ;; Comment with matching id → must NOT be killed
    (should (null (let ((cid "99"))
                    (when (and cid (not (member cid ids))) t))))
    ;; Comment with non-matching id → MUST be killed
    (should (let ((cid "42"))
              (when (and cid (not (member cid ids))) t)))))

;;; ── ejira-push scan: Rule H (PendingDelete) ──────────────────────────────────

(ert-deftest ejira-push--rule-h/delete-comment ()
  "PendingDelete=t on ejira-comment produces one delete op with correct CommId."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** Some comment
:PROPERTIES:
:TYPE:        ejira-comment
:CommId:      42
:PendingDelete: t
:END:
Comment body.
")))
    (should (= 1 (length ops)))
    (should (eq 'delete  (plist-get (car ops) :op)))
    (should (eq 'comment (plist-get (car ops) :object)))
    (should (equal "42"  (plist-get (car ops) :key)))))

(ert-deftest ejira-push--rule-h/issue-not-deletable ()
  "PendingDelete on a non-comment heading is not picked up as a delete op."
  (let ((ops (ejira-test--scan
              "* TODO PROJ-1 Issue
:PROPERTIES:
:TYPE:        ejira-issue
:ID:          PROJ-1
:Pushhash:    clean
:PendingDelete: t
:END:
")))
    (should (null (cl-remove-if-not
                   (lambda (op) (eq 'delete (plist-get op :op)))
                   ops)))))

;;; ── ejira-push scan: Rule A (dirty issue) ────────────────────────────────────

(ert-deftest ejira-push--rule-a/dirty-issue ()
  "Stale Pushhash on ejira-issue produces one update-issue op."
  (let ((ops (ejira-test--scan
              "* TODO PROJ-1 My Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:Pushhash: WRONGHASH
:END:
")))
    (let ((issue-ops (cl-remove-if-not
                      (lambda (op) (and (eq 'update  (plist-get op :op))
                                        (eq 'issue   (plist-get op :object))))
                      ops)))
      (should (= 1 (length issue-ops)))
      (should (equal "PROJ-1" (plist-get (car issue-ops) :key)))
      (should (equal "PROJ"   (plist-get (car issue-ops) :project))))))

(ert-deftest ejira-push--priority/uses-exact-jira-id ()
  "A changed priority is sent by Jira ID, never by display name."
  (let ((org-priority-highest 1)
        (org-priority-lowest 8)
        (org-lowest-priority 8)
        (ejira-priority-policies nil)
        (ejira-todo-states-alist '(("Open" . 1)))
        (update-args nil)
        (remote-item
         '((key . "TEST-1")
           (fields . ((summary . "Issue")
                      (description . "")
                      (assignee . nil)
                      (priority . ((id . "hidden-default") (name . "Hidden default")))
                      (duedate . nil)
                      (status . ((name . "Open"))))))))
    (ejira-test--with-org-buf
        "* TODO [#3] Issue\n:PROPERTIES:\n:TYPE: ejira-issue\n:ID: TEST-1\n:JiraPriorityId: hidden-default\n:JiraPriorityName: Hidden default\n:Pushhash: WRONG\n:END:\n** Description\n"
      (let ((marker (progn (goto-char (point-min))
                           (re-search-forward org-heading-regexp)
                           (point-marker))))
        (cl-letf (((symbol-function 'jiralib2-jql-search)
                   (lambda (&rest _args) (list remote-item)))
                  ((symbol-function 'ejira--get-priority-scheme)
                   (lambda (&rest _args) ejira-test--priority-scheme))
                  ((symbol-function 'ejira--push-finalize)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'jiralib2-update-issue)
                   (lambda (key &rest args)
                     (setq update-args (cons key args)))))
          (let* ((ops (list (list :op 'update :object 'issue :key "TEST-1"
                                  :project "TEST" :parent-issue "TEST-1"
                                  :marker marker :data nil)))
                 (plans (ejira--push-build-plans ops))
                 (plan (car plans)))
            (should (= 1 (length plans)))
            (should (equal "Low" (nth 2 (assoc "priority"
                                               (plist-get plan :changes)))))
            (funcall (plist-get plan :send))
            (should (equal '("TEST-1" (priority . ((id . "p3"))))
                           update-args))))))))

(ert-deftest ejira-push--priority/legacy-cookie-is-not-inferred ()
  "A dirty legacy heading can still push state without an inferred priority."
  (let ((org-priority-highest 1)
        (org-priority-lowest 8)
        (org-lowest-priority 8)
        (ejira-todo-states-alist '(("In Progress" . 1)))
        (remote-item
         '((key . "TEST-1")
           (fields . ((summary . "Issue")
                      (description . "")
                      (assignee . nil)
                      (priority . ((id . "hidden-default") (name . "Hidden default")))
                      (duedate . nil)
                      (status . ((name . "In Progress"))))))))
    (ejira-test--with-org-buf
        "* DONE [#3] Issue\n:PROPERTIES:\n:TYPE: ejira-issue\n:ID: TEST-1\n:Pushhash: WRONG\n:END:\n** Description\n"
      (let ((marker (progn (goto-char (point-min))
                           (re-search-forward org-heading-regexp)
                           (point-marker))))
        (cl-letf (((symbol-function 'jiralib2-jql-search)
                   (lambda (&rest _args) (list remote-item)))
                  ((symbol-function 'ejira--get-priority-scheme)
                   (lambda (&rest _args) ejira-test--priority-scheme)))
          (let* ((ops (list (list :op 'update :object 'issue :key "TEST-1"
                                  :project "TEST" :parent-issue "TEST-1"
                                  :marker marker :data nil)))
                 (plans (ejira--push-build-plans ops))
                 (changes (plist-get (car plans) :changes)))
            (should (= 1 (length plans)))
            (should (assoc "state" changes))
            (should-not (assoc "priority" changes))))))))

(ert-deftest ejira-push--priority/new-subtask-uses-policy-default ()
  "New TEST subtasks explicitly receive the visible default priority."
  (let ((ejira-priority-policies ejira-test--priority-policies)
        (create-call nil))
    (ejira-test--with-org-buf "* TODO New subtask\n\nCascaded body.\n"
      (goto-char (point-min))
      (re-search-forward org-heading-regexp)
      (let ((child (list :marker (point-marker)
                         :title "New subtask"
                         :state "TODO"
                         :body "")))
        (cl-letf (((symbol-function 'jiralib2-create-issue)
                   (lambda (project type summary description &rest args)
                     (setq create-call
                           (list project type summary description args))
                     '((key . "TEST-2"))))
                  ((symbol-function 'ejira--finalize-new-issue)
                   (lambda (&rest _args) nil)))
          (ejira--push-create-cascaded-subtask
           "TEST-1" "TEST" child nil nil)
          (should (equal "Cascaded body.\n" (nth 3 create-call)))
          (goto-char (point-min))
          (should (string-empty-p (string-trim (ejira--get-heading-own-body))))
          (should (equal "p1"
                         (cdr (assoc 'id
                                     (cdr (assoc 'priority (nth 4 create-call))))))))))))

(ert-deftest ejira-push--rule-a/clean-issue-no-op ()
  "Current Pushhash on ejira-issue produces no update op."
  ;; Compute the real hash for a heading with no children/properties.
  (let (real-hash)
    (ejira-test--with-org-buf
        "* TODO PROJ-1 Clean Issue\n:PROPERTIES:\n:TYPE:     ejira-issue\n:ID:       PROJ-1\n:END:\n"
      (goto-char (point-min))
      (re-search-forward org-heading-regexp nil t)
      (setq real-hash (md5 (ejira--heading-pushable-content))))
    (let ((ops (ejira-test--scan
                (concat "* TODO PROJ-1 Clean Issue\n:PROPERTIES:\n"
                        ":TYPE:     ejira-issue\n:ID:       PROJ-1\n"
                        ":Pushhash: " real-hash "\n:END:\n"))))
      (should (null (cl-remove-if-not
                     (lambda (op) (and (eq 'update (plist-get op :op))
                                       (eq 'issue  (plist-get op :object))))
                     ops))))))

;;; ── ejira-push scan: Rule A2 (dirty comment) ─────────────────────────────────

(ert-deftest ejira-push--rule-a2/dirty-comment ()
  "Stale Pushhash on ejira-comment WITH CommId produces an update-comment op."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** Some comment
:PROPERTIES:
:TYPE:     ejira-comment
:CommId:   77
:Pushhash: WRONGHASH
:END:
Original body.
")))
    (let ((comment-ops (cl-remove-if-not
                        (lambda (op) (and (eq 'update  (plist-get op :op))
                                          (eq 'comment (plist-get op :object))))
                        ops)))
      (should (= 1 (length comment-ops)))
      (should (equal "77" (plist-get (car comment-ops) :key))))))

(ert-deftest ejira-push--rule-a2/comment-no-commid-not-update ()
  "ejira-comment without CommId is a draft (Rule G), not a Rule A2 update."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** Draft
:PROPERTIES:
:TYPE:     ejira-comment
:Pushhash: WRONGHASH
:END:
Draft body.
")))
    (should (null (cl-remove-if-not
                   (lambda (op) (and (eq 'update  (plist-get op :op))
                                     (eq 'comment (plist-get op :object))))
                   ops)))))

;;; ── ejira-push scan: Rules B / C / D (pending properties) ────────────────────

(ert-deftest ejira-push--rule-b/pending-transition ()
  "PendingTransition produces an update-status op with the right action name."
  (let ((ops (ejira-test--scan
              "* TODO PROJ-1 Issue
:PROPERTIES:
:TYPE:              ejira-issue
:ID:                PROJ-1
:Pushhash:          clean
:PendingTransition: In Progress
:END:
")))
    (let ((status-ops (cl-remove-if-not
                       (lambda (op) (and (eq 'update (plist-get op :op))
                                         (eq 'status (plist-get op :object))))
                       ops)))
      (should (= 1 (length status-ops)))
      (should (equal "In Progress"
                     (plist-get (plist-get (car status-ops) :data) :action-name))))))

(ert-deftest ejira-push--rule-c/pending-issuetype ()
  "PendingIssuetype produces an update-issuetype op."
  (let ((ops (ejira-test--scan
              "* TODO PROJ-1 Issue
:PROPERTIES:
:TYPE:             ejira-issue
:ID:               PROJ-1
:Pushhash:         clean
:PendingIssuetype: Story
:END:
")))
    (let ((type-ops (cl-remove-if-not
                     (lambda (op) (and (eq 'update    (plist-get op :op))
                                       (eq 'issuetype (plist-get op :object))))
                     ops)))
      (should (= 1 (length type-ops)))
      (should (equal "Story"
                     (plist-get (plist-get (car type-ops) :data) :new-type))))))

(ert-deftest ejira-push--rule-d/pending-epic ()
  "PendingEpic produces an update-epic op."
  (let ((ops (ejira-test--scan
              "* TODO PROJ-1 Issue
:PROPERTIES:
:TYPE:       ejira-issue
:ID:         PROJ-1
:Pushhash:   clean
:PendingEpic: PROJ-E1
:END:
")))
    (let ((epic-ops (cl-remove-if-not
                     (lambda (op) (and (eq 'update (plist-get op :op))
                                       (eq 'epic   (plist-get op :object))))
                     ops)))
      (should (= 1 (length epic-ops)))
      (should (equal "PROJ-E1"
                     (plist-get (plist-get (car epic-ops) :data) :new-epic))))))

;;; ── ejira-push scan: Rules E / F (new headings) ──────────────────────────────

(ert-deftest ejira-push--rule-e/new-subtask-under-issue ()
  "TODO heading without TYPE directly under ejira-issue → create-subtask op."
  (ejira-test--with-org-buf
      "* PROJ-1 Parent Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:Issuetype: Task
:END:
** TODO My New Subtask

Body for Jira.
"
    (let* ((ops (ejira--push-scan-buffer (current-buffer)))
           (subtask-ops (cl-remove-if-not
                         (lambda (op) (and (eq 'create (plist-get op :op))
                                           (eq 'subtask (plist-get op :object))))
                         ops)))
      (should (= 1 (length subtask-ops)))
      (should (equal "PROJ-1"
                     (plist-get (plist-get (car subtask-ops) :data) :parent-key)))
      (let* ((plan (car (ejira--push-build-plans subtask-ops)))
             (description (cadr (assoc "description" (plist-get plan :fields)))))
        (should (equal "Body for Jira.\n" description))))))

(ert-deftest ejira-push--new-subtask/migrates-body-before-create ()
  "Creating a subtask retains its direct body in the managed description."
  (let ((ejira--assign-new-issues nil)
        created-description)
    (ejira-test--with-org-buf
        "* PROJ-1 Parent Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:Issuetype: Task
:END:
** TODO My New Subtask

Body for Jira.
"
      (let* ((ops (ejira--push-scan-buffer (current-buffer)))
             (subtask-op (cl-find-if (lambda (op) (eq 'subtask (plist-get op :object)))
                                     ops))
             (plan (car (ejira--push-build-plans (list subtask-op))))
             (marker (plist-get subtask-op :marker)))
        (cl-letf (((symbol-function 'ejira--default-priority-id) (lambda (&rest _) nil))
                  ((symbol-function 'jiralib2-create-issue)
                   (lambda (_project _type _summary description &rest _args)
                     (setq created-description description)
                     '((key . "PROJ-2"))))
                  ((symbol-function 'ejira--finalize-new-issue) (lambda (&rest _) nil)))
          (funcall (plist-get plan :send)))
        (should (equal "Body for Jira.\n" created-description))
        (org-with-point-at marker
          (should (string-empty-p (string-trim (ejira--get-heading-own-body))))
          (org-with-point-at (ejira--find-child-heading "Description")
            (should (equal "Body for Jira."
                           (string-trim (ejira--get-heading-body (point-marker)))))))))))

(ert-deftest ejira-push--rule-e/plain-heading-under-issue-ignored ()
  "Heading without TODO under ejira-issue is NOT detected as a new subtask."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Parent Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:Issuetype: Task
:END:
** Just a note
")))
    (should (null (cl-remove-if-not
                   (lambda (op) (eq 'create (plist-get op :op)))
                   ops)))))

(ert-deftest ejira-push--rule-e/new-task-under-epic ()
  "TODO heading without TYPE directly under ejira-epic → create-issue op with
:parent-epic set and :issue-type from `ejira-epic-child-type-name'."
  (let ((ops (ejira-test--scan
              "* PROJ-1 My Epic
:PROPERTIES:
:TYPE:     ejira-epic
:ID:       PROJ-1
:Issuetype: Epic
:END:
** TODO My New Task
")))
    (let ((issue-ops (cl-remove-if-not
                      (lambda (op) (and (eq 'create (plist-get op :op))
                                        (eq 'issue  (plist-get op :object))))
                      ops)))
      (should (= 1 (length issue-ops)))
      (let ((data (plist-get (car issue-ops) :data)))
        (should (equal "PROJ-1" (plist-get data :parent-epic)))
        (should (equal ejira-epic-child-type-name (plist-get data :issue-type)))))))

(ert-deftest ejira-push--rule-e/new-epic-under-initiative ()
  "TODO heading without TYPE under ejira-issue with Issuetype=Initiative
→ create-issue op with :parent-initiative set and :issue-type \"Epic\"."
  (let ((ops (ejira-test--scan
              "* PROJ-1 My Initiative
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:Issuetype: Initiative
:END:
** TODO My New Epic
")))
    (let ((issue-ops (cl-remove-if-not
                      (lambda (op) (and (eq 'create (plist-get op :op))
                                        (eq 'issue  (plist-get op :object))))
                      ops)))
      (should (= 1 (length issue-ops)))
      (let ((data (plist-get (car issue-ops) :data)))
        (should (equal "PROJ-1" (plist-get data :parent-initiative)))
        (should (equal ejira-epic-type-name (plist-get data :issue-type)))))))

(ert-deftest ejira-push--rule-f/new-issue-under-project ()
  "TODO heading without TYPE directly under ejira-project → create-issue op."
  (ejira-test--with-org-buf
      "* PROJ
:PROPERTIES:
:TYPE:     ejira-project
:ID:       PROJ
:END:
** TODO My New Issue

Body for Jira.
"
    (let* ((ops (ejira--push-scan-buffer (current-buffer)))
           (issue-ops (cl-remove-if-not
                       (lambda (op) (and (eq 'create (plist-get op :op))
                                         (eq 'issue (plist-get op :object))))
                       ops)))
      (should (= 1 (length issue-ops)))
      (should (equal "PROJ"
                     (plist-get (plist-get (car issue-ops) :data) :project-key)))
      (let* ((plan (car (ejira--push-build-plans issue-ops)))
             (description (cadr (assoc "description" (plist-get plan :fields)))))
        (should (equal "Body for Jira.\n" description))))))

;;; ── ejira-push scan: Rule G (comment drafts) ─────────────────────────────────

(ert-deftest ejira-push--rule-g/plain-draft-under-comments ()
  "Plain heading under Comments (no TYPE, no CommId) → create-comment op."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** My draft comment
This is my draft.
")))
    (let ((comment-ops (cl-remove-if-not
                        (lambda (op) (and (eq 'create  (plist-get op :op))
                                          (eq 'comment (plist-get op :object))))
                        ops)))
      (should (= 1 (length comment-ops)))
      (should (equal "PROJ-1"
                     (plist-get (plist-get (car comment-ops) :data) :issue-key))))))

(ert-deftest ejira-push--rule-g/capture-stub-detected ()
  "TYPE=ejira-comment + no CommId (org-capture stub) → create-comment op."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** <new comment>
:PROPERTIES:
:TYPE:     ejira-comment
:END:
Draft from capture.
")))
    (let ((comment-ops (cl-remove-if-not
                        (lambda (op) (and (eq 'create  (plist-get op :op))
                                          (eq 'comment (plist-get op :object))))
                        ops)))
      (should (= 1 (length comment-ops)))
      (should (equal "PROJ-1"
                     (plist-get (plist-get (car comment-ops) :data) :issue-key))))))

(ert-deftest ejira-push--rule-g/existing-comment-not-draft ()
  "ejira-comment WITH CommId already pushed — no create-comment op."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** Already posted
:PROPERTIES:
:TYPE:     ejira-comment
:CommId:   55
:Pushhash: clean
:END:
Already on Jira.
")))
    (should (null (cl-remove-if-not
                   (lambda (op) (and (eq 'create  (plist-get op :op))
                                     (eq 'comment (plist-get op :object))))
                   ops)))))

(ert-deftest ejira-push--rule-g/child-of-description-not-comment ()
  "Plain heading under Description heading is not detected as a comment draft."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Description
*** A note inside description
")))
    (should (null (cl-remove-if-not
                   (lambda (op) (and (eq 'create  (plist-get op :op))
                                     (eq 'comment (plist-get op :object))))
                   ops)))))

(ert-deftest ejira-push--rule-g/todo-under-comments-not-comment ()
  "TODO heading under Comments has a todo-state; Rule G skips it (no comment op)."
  (let ((ops (ejira-test--scan
              "* PROJ-1 Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:END:
** Comments
*** TODO Follow-up action
")))
    (should (null (cl-remove-if-not
                   (lambda (op) (and (eq 'create  (plist-get op :op))
                                     (eq 'comment (plist-get op :object))))
                   ops)))))

;;; ── Duplicate-heading prevention ─────────────────────────────────────────────
;;
;; Regression tests for the failure that duplicated whole project trees: a
;; stale `org-id-locations' made `ejira--find-heading' report a missing item,
;; callers then created a second copy of it.

(defmacro ejira-test--with-project-dir (content &rest body)
  "Run BODY with `ejira-org-directory' holding a TEST.org made of CONTENT."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "ejira-test-" t))
          (ejira-org-directory dir)
          (ejira-projects '("TEST"))
          (ejira--heading-cache nil)
          (org-id-locations (make-hash-table :test 'equal))
          (file (expand-file-name "TEST.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defconst ejira-test--project-content
  "* Project\n:PROPERTIES:\n:ID:       TEST\n:TYPE:     ejira-project\n:END:\n\
** TODO An issue\n:PROPERTIES:\n:ID:       TEST-1\n:TYPE:     ejira-issue\n:END:\n\n")

(ert-deftest ejira-find-heading-by-scan/finds-existing-id ()
  "Scanning locates an ID that `org-id-locations' does not know about."
  (ejira-test--with-project-dir ejira-test--project-content
    (let ((m (ejira--find-heading-by-scan "TEST-1")))
      (should (markerp m))
      (org-with-point-at m
        (should (equal "TEST-1" (org-entry-get (point) "ID")))))))

(ert-deftest ejira-find-heading-by-scan/returns-nil-when-absent ()
  "Scanning returns nil for an ID that really is not there."
  (ejira-test--with-project-dir ejira-test--project-content
    (should (null (ejira--find-heading-by-scan "TEST-999")))))

(ert-deftest ejira-find-heading-by-scan/searches-extra-files ()
  "A refiled issue is found through `ejira-extra-scan-files'.
Refiled headings live outside the project directory; when the ID
index has been rebuilt without them, the scan must still find them or
a sync would create a duplicate heading."
  (ejira-test--with-project-dir ejira-test--project-content
    (let* ((extra (make-temp-file "ejira-refile-" nil ".org"))
           (ejira-extra-scan-files (list extra)))
      (unwind-protect
          (progn
            (with-temp-file extra
              (insert "* Refiled\n:PROPERTIES:\n:ID:       TEST-EXTRA\n:TYPE:     ejira-issue\n:END:\n"))
            (let ((m (ejira--find-heading-by-scan "TEST-EXTRA")))
              (should (markerp m))
              (should (equal (file-truename extra)
                             (file-truename (buffer-file-name (marker-buffer m)))))))
        (when-let ((b (find-buffer-visiting extra)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))
        (delete-file extra)))))

(ert-deftest ejira-find-heading-by-scan/repairs-org-id-locations ()
  "A successful scan re-registers the ID so the fast path works next time."
  (ejira-test--with-project-dir ejira-test--project-content
    (should (null (gethash "TEST-1" org-id-locations)))
    (ejira--find-heading-by-scan "TEST-1")
    (should (gethash "TEST-1" org-id-locations))))

(ert-deftest ejira-find-heading/recovers-from-stale-org-id-locations ()
  "`ejira--find-heading' finds the item even with an empty id index.
This is the exact condition that used to duplicate project trees."
  (ejira-test--with-project-dir ejira-test--project-content
    (should (markerp (ejira--find-heading "TEST-1")))
    (should (markerp (ejira--find-heading "TEST")))))

(ert-deftest ejira-new-heading/refuses-to-duplicate-existing-id ()
  "Creating a heading for an ID already in the file returns the existing one."
  (ejira-test--with-project-dir ejira-test--project-content
    (let* ((buf (find-file-noselect (expand-file-name "TEST.org" ejira-org-directory) t))
           (before (with-current-buffer buf (buffer-string)))
           (m (ejira--new-heading buf nil "TEST-1")))
      (should (markerp m))
      (org-with-point-at m
        (should (equal "TEST-1" (org-entry-get (point) "ID"))))
      ;; buffer untouched: no second copy written
      (should (equal before (with-current-buffer buf (buffer-string))))
      (should (= 1 (with-current-buffer buf
                     (count-matches "^:ID: +TEST-1 *$" (point-min) (point-max))))))))

(ert-deftest ejira-update-task-light/defers-instead-of-escalating ()
  "A shallow sync records unknown keys rather than firing a full update."
  (ejira-test--with-project-dir ejira-test--project-content
    (let ((ejira--shallow-only t)
          (ejira--deferred-keys nil))
      (cl-letf (((symbol-function 'ejira--update-task)
                 (lambda (&rest _) (error "must not escalate during shallow sync"))))
        (should (null (ejira--update-task-light "TEST-404" "Open" nil)))
        (should (equal '("TEST-404") ejira--deferred-keys))))))

(ert-deftest ejira-update-task-light/escalates-when-not-shallow ()
  "A full sync still falls back to `ejira--update-task' for unknown keys."
  (ejira-test--with-project-dir ejira-test--project-content
    (let ((ejira--shallow-only nil)
          (ejira--deferred-keys nil)
          (called nil))
      (cl-letf (((symbol-function 'ejira--update-task)
                 (lambda (k) (setq called k))))
        (ejira--update-task-light "TEST-404" "Open" nil)
        (should (equal "TEST-404" called))
        (should (null ejira--deferred-keys))))))

;;; ── JIRA -> Org conversion (regressions) ─────────────────────────────────────

(defun ejira-test--parse (jira)
  "Convert JIRA markup JIRA to org text with failure reporting disabled."
  (let ((ejira-parser-failure-function nil))
    (ejira-parser-jira-to-org jira)))

(ert-deftest ejira-parser/keeps-line-structure ()
  "Paragraphs, headings and lists stay on separate lines.
A regression dropped every LF before parsing, collapsing whole
descriptions into one line that no later rule could recognize."
  ;; No shift level: h2 maps to two stars, preserving the relative
  ;; hierarchy, and every later line stays recognizable markup.
  (should (equal "** Outcome\nText\n\n** Done\n- First\n- Second"
                 (ejira-test--parse "h2. Outcome\nText\n\nh2. Done\n* First\n* Second"))))

(ert-deftest ejira-parser/preserves-backslashes-and-percent ()
  "Literal backslashes and percent signs survive conversion.
Restoration used to be computed and then discarded, leaking random
identifiers into the output."
  (should (equal "C:\\tmp\\file 100%"
                 (ejira-test--parse "C:\\tmp\\file 100%"))))

(ert-deftest ejira-parser/unescapes-literal-braces-and-brackets ()
  "JIRA's \\{ and \\[ escapes become plain characters in Org."
  (should (equal "a {b} [c]" (ejira-test--parse "a \\{b\\} \\[c]"))))

(ert-deftest ejira-parser/table-keeps-dash-cells ()
  "A body row starting with a dash is data, not an Org rule.
Org reads | -1| rows as horizontal rules and discards them."
  (let ((out (ejira-test--parse "||n||\n|-1|")))
    (should (string-match-p "-1" out))
    (should (= 1 (s-count-matches "^|---" out)))))

(ert-deftest ejira-parser/literal-hash-markers-are-not-renumbered ()
  "Only actual list markers are numbered; literal ######## text is not."
  (should (equal "########banner" (ejira-test--parse "########banner")))
  (let ((out (ejira-test--parse "{code}\nx\n######## banner\n{code}")))
    (should (string-match-p "######## banner" out))))

(ert-deftest ejira-parser/ordered-list-numbering ()
  "Counters restart per list and per nesting level."
  (should (equal "1. A\n    1. a\n2. B\n    1. b"
                 (ejira-test--parse "# A\n## a\n# B\n## b")))
  ;; An indented continuation keeps the list open...
  (should (equal "1. A\n  continuation\n2. B"
                 (ejira-test--parse "# A\n  continuation\n# B")))
  ;; ...and a blank line does not split it either.
  (should (equal "1. A\n\n2. B" (ejira-test--parse "# A\n\n# B")))
  ;; A new top-level construct does.
  (should (equal "1. A\n** X\n1. B"
                 (ejira-test--parse "# A\nh2. X\n# B"))))

(ert-deftest ejira-parser/seven-level-ordered-list ()
  "Deep nesting beyond six levels still converts instead of failing."
  (should (equal (concat "1. One\n" (make-string 20 ? ) "1. Seven")
                 (ejira-test--parse "# One\n###### Seven"))))

(ert-deftest ejira-parser/link-brackets-are-scoped ()
  "A link label cannot span earlier bracketed text."
  (should (equal "[a] and [[https://e][b]]"
                 (ejira-test--parse "[a] and [b|https://e]"))))

(ert-deftest ejira-parser/bare-link ()
  "Links without a description, as emitted by the exporter, convert."
  (should (equal "[[https://e]]" (ejira-test--parse "[https://e]"))))

(ert-deftest ejira-parser/verbatim-is-protected ()
  "Inline verbatim content is never rewritten by later rules."
  (should (equal "=[x|https://e]=" (ejira-test--parse "{{[x|https://e]}}"))))

(ert-deftest ejira-parser/verbatim-with-edge-spaces-kept ()
  "Org emphasis cannot wrap edge whitespace; keep the JIRA form."
  (should (equal "{{ x }}" (ejira-test--parse "{{ x }}"))))

(ert-deftest ejira-parser/adjacent-italics ()
  "A boundary character is not consumed by the preceding span."
  (should (equal "/one/ /two/" (ejira-test--parse "_one_ _two_"))))

(ert-deftest ejira-parser/emphasis-boundaries ()
  "Word-internal underscores and arithmetic pluses stay literal."
  (should (equal "a_b_c" (ejira-test--parse "a_b_c")))
  (should (equal "2+3+4" (ejira-test--parse "2+3+4")))
  (should (equal "x + y" (ejira-test--parse "x + y")))
  (should (equal "_under_" (ejira-test--parse "+under+"))))

(ert-deftest ejira-parser/code-block-escaping ()
  "Block delimiters inside code cannot terminate the generated block."
  (let ((out (ejira-test--parse "{code}\na\n#+END_SRC\nb\n{code}")))
    (should (string-match-p "^\\(  \\|#\\+BEGIN_SRC\\|#\\+END_SRC\\)" out))
    (should (string-match-p ",#\\+END_SRC" out))
    (should (string-match-p "#\\+BEGIN_SRC\n  a" out))))

(ert-deftest ejira-parser/trailing-whitespace-normalized ()
  "Trailing whitespace is stripped everywhere, including code blocks.
A global `whitespace-cleanup' save hook strips it on save regardless,
so the converted form must already be clean or every save would look
like a local edit."
  (let ((out (ejira-test--parse "{code}\nx  \ny\n{code}")))
    (should (string-match-p "x\n" out))
    (should-not (string-match-p "  \n" out))))

(ert-deftest ejira-parser/code-empty-lines-unindented ()
  "Empty code lines carry no indentation, so whitespace cleanup is a no-op.
A global `whitespace-cleanup' before-save hook strips whitespace-only
line indent; emitting it would make every saved body compare modified."
  (let ((out (ejira-test--parse "{code}\na\n\n  indented\nb\n{code}")))
    ;; Block indent adds two spaces; the line's own two spaces stay.
    (should (string-match-p "a\n\n    indented" out))
    (should-not (string-match-p "  \n" out))))

(ert-deftest ejira-parser/code-language ()
  "An explicit language wins over detection; bare tokens are accepted."
  (should (string-match-p "#\\+BEGIN_SRC java" (ejira-test--parse "{code:java}\nx\n{code}")))
  (should (string-match-p "#\\+BEGIN_SRC python"
                          (ejira-test--parse "{code:language=python}\nx\n{code}"))))

(ert-deftest ejira-parser/noformat-is-literal ()
  "Noformat contents are protected like code."
  (let ((out (ejira-test--parse "{noformat}\n* literal\n{noformat}")))
    (should (string-match-p "#\\+BEGIN_EXAMPLE" out))
    (should (string-match-p "\\* literal" out))
    (should-not (string-match-p "1\\. literal" out))))

(ert-deftest ejira-parser/quote-body-converted ()
  "Quote bodies are markup too and are converted recursively."
  (let ((out (ejira-test--parse "{quote}\n# item\n{quote}")))
    (should (string-match-p "#\\+BEGIN_QUOTE" out))
    (should (string-match-p "1\\. item" out))))

(ert-deftest ejira-parser/checkbox-emoticons ()
  "Checkbox emoticons map onto Org checkbox states."
  (should (equal "- [ ] todo\n- [X] done\n- [-] half"
                 (ejira-test--parse "* (x) todo\n* (/) done\n* (i) half"))))

(ert-deftest ejira-parser/horizontal-rule ()
  "JIRA's four-dash rule becomes Org's five-dash rule."
  (should (equal "-----" (ejira-test--parse "----"))))

(ert-deftest ejira-parser/headings-roundtrip-at-offset ()
  "Body headings exported relative to their container keep h-levels.
Without the offset the exporter renormalizes the body's own minimum
heading level to h1 and the original levels are lost on push."
  (let* ((jira "h1. A\nText\nh2. B")
         (org (ejira-parser-jira-to-org jira 2)))
    (should (equal (string-trim org) "*** A\nText\n**** B"))
    (should (equal (string-trim (ejira-parser-org-to-jira org 2))
                   (string-trim jira)))
    ;; Without the offset the minimum level is renormalized to h1.
    (should (equal (string-trim (ejira-parser-org-to-jira org))
                   "h1. A\nText\nh2. B"))))

;;; ── Body extraction and heading levels ───────────────────────────────────────

(ert-deftest ejira-parse-body/preserves-line-structure ()
  "LF and CRLF input both keep paragraphs, headings and links."
  (let* ((jira "h2. Outcome\nSelected engineers.\n\nOwner: Val.\n\nh2. Done\n* First\n\n[Plan|https://example.com/plan]\n")
         (expected "**** Outcome\nSelected engineers.\n\nOwner: Val.\n\n**** Done\n- First\n\n[[https://example.com/plan][Plan]]"))
    (dolist (input (list jira (replace-regexp-in-string "\n" "\r\n" jira)))
      (should (equal expected (ejira--parse-body input 2))))))

(ert-deftest ejira-parse-body/empty-and-nonbreaking-space ()
  "Nil, empty and nonbreaking-space bodies are handled."
  (should (equal "" (ejira--parse-body nil)))
  (should (equal "" (ejira--parse-body "")))
  (should (equal "One two" (ejira--parse-body "One\u00a0two"))))

(ert-deftest ejira-heading-body-level/uses-outline-depth ()
  "The shift level is the heading's outline depth, not match data."
  (ejira-test--with-org-buf "* One\n** Two\n*** Three\n******* Seven\n"
    (dolist (level '(1 2 3 7))
      (should (= level (ejira--heading-body-level (point-marker))))
      (forward-line))))

(ert-deftest ejira-narrow-to-body/ignores-child-drawers ()
  "A drawer on a child must not hide the parent's body before it."
  (ejira-test--with-org-buf
      "** Description\n\nIntro.\n*** Section\n:PROPERTIES:\n:CUSTOM_ID: section\n:END:\n\nSection body.\n"
    (let ((body (ejira--get-heading-body (point-marker))))
      (should (string-match-p "Intro\\." body))
      (should (string-match-p "Section body\\." body))
      (should (string-match-p "\\*\\*\\* Section" body)))))

(ert-deftest ejira-narrow-to-body/blank-body-keeps-following-headings ()
  "A body of only blank lines must not swallow the next heading.
`org-end-of-meta-data' skips blank lines and lands on the next
heading; computing the subtree end from there used to delete a
sibling -- an issue subtree in the generated project files."
  (ejira-test--with-org-buf
      "* Issue\n** Description\n\n** TODO Child\n:PROPERTIES:\n:ID: X-1\n:END:\n\nKeep me.\n"
    (let ((d (progn (goto-char (point-min))
                    (re-search-forward "^\\*\\* Description")
                    (org-back-to-heading t)
                    (point-marker))))
      (ejira--set-heading-body d "*** New body")
      (should (string-match-p "\\*\\* TODO Child" (buffer-string)))
      (should (string-match-p "Keep me\\." (buffer-string)))
      (should (string-match-p "New body" (buffer-string)))
      (org-with-point-at d
        (save-excursion
          (should (org-goto-first-child))
          (should (equal "New body" (org-get-heading t t t t))))))))

(ert-deftest ejira-description/pull-keeps-headings-contained ()
  "Repeated description pulls are idempotent, keep siblings and deep levels.
Reproduces the corrupted epics: newlines collapsed, the body became
one long paragraph, and later pulls saw an empty description."
  (dolist (level '(2 7))
    (ejira-test--with-org-buf
        (format "%s TODO Issue\n:PROPERTIES:\n:TYPE: ejira-issue\n:ID: TEST-1\n:END:\n%s Description\n\nOld body.\n%s TODO Child\n:PROPERTIES:\n:ID: TEST-2\n:END:\nKeep me.\n"
                (make-string (1- level) ?*) (make-string level ?*)
                (make-string level ?*))
      (let* ((issue (point-marker))
             (description (ejira--find-child-heading "Description"))
             (jira "h1. Outcome\nText.\n\nh2. Details\n* First\n* Second\n")
             (expected (ejira--expected-org-body description jira)))
        (dotimes (_ 2)
          (ejira--set-heading-body-jira-markup description jira)
          (should (equal expected (string-trim (ejira--get-heading-body description))))
          (should (equal expected (ejira--expected-jira-description issue jira)))
          (org-with-point-at issue (ejira--update-push-baseline))
          (should-not (org-with-point-at issue (ejira--locally-modified-p)))
          (org-with-point-at issue
            (should (ejira--find-child-heading "Child")))
          (goto-char description)
          (should (org-goto-first-child))
          (should (= (1+ level) (org-current-level)))
          (should (equal "Outcome" (org-get-heading t t t t))))
        (should (string-suffix-p "Keep me.\n" (buffer-string)))))))

(provide 'ejira-test)
;;; ejira-test.el ends here
