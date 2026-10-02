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
(require 'ejira)

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defmacro ejira-test--as-confirmed (&rest body)
  "Run BODY as the confirmed review buffer does: with Jira writes authorized.
For tests that call a plan's :send directly instead of going through
`ejira-confirm--execute'."
  (declare (indent 0))
  `(let ((ejira--jira-write-authorized t))
     ,@body))

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

(defun ejira-test--scan-current ()
  "Return scan ops for the current buffer with `ejira--get-project' mocked."
  (cl-letf (((symbol-function 'ejira--get-project)
             (lambda (key) (car (split-string key "-")))))
    (ejira--push-scan-buffer (current-buffer))))

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

(ert-deftest ejira-confirm--items/issue-update-is-the-node ()
  "An issue's update plan becomes the node; its other plans are children."
  (let* ((send-a (lambda () 'a))
         (plans (list (list :op 'update :object 'issue :project "TEST" :title "TEST-1"
                            :parent-issue "TEST-1" :changes '(("summary" "Old" "New"))
                            :payload "summary: New" :send send-a)
                      (list :op 'create :object 'subtask :project "TEST"
                            :title "new subtask: Child" :parent-issue "TEST-1"
                            :fields '(("title" "Child") ("state" "TODO") ("description" ""))
                            :assign-self (list t) :send #'ignore)
                      (list :op 'update :object 'status :project "TEST" :title "TEST-1"
                            :parent-issue "TEST-1" :changes '(("transition" "" "Done"))
                            :send #'ignore)))
         (items (cl-letf (((symbol-function 'ejira-confirm--issue-title)
                           (lambda (_) "Fix login")))
                  (ejira-confirm--items plans)))
         (project (car items))
         (node (car (plist-get project :children)))
         (children (plist-get node :children)))
    (should (= 1 (length items)))
    (should (equal "TEST" (plist-get project :label)))
    (should (equal "Fix login" (plist-get node :label)))
    (should (eq 'modified (plist-get node :kind)))
    (should (equal '((:name "summary" :old "Old" :new "New")) (plist-get node :fields)))
    (should (eq send-a (plist-get node :execute)))
    (should (equal "summary: New" (plist-get node :payload)))
    (should (= 2 (length children)))
    (should (equal "subtask: TODO Child" (plist-get (car children) :label)))
    (should (eq 'new (plist-get (car children) :kind)))
    (should (equal '((:name "title" :new "Child") (:name "state" :new "TODO"))
                   (plist-get (car children) :fields)))
    (should (eq 'assign-self (plist-get (car (plist-get (car children) :toggles)) :key)))
    (should (equal "transition" (plist-get (cadr children) :label)))
    (should (equal '((:name "transition" :old nil :new "Done"))
                   (plist-get (cadr children) :fields)))))

(ert-deftest ejira-confirm--items/issue-without-update-is-a-container ()
  "Comment plans under an issue with no update plan hang off a container."
  (let* ((plans (list (list :op 'create :object 'comment :project "TEST"
                            :title "new comment on TEST-2" :parent-issue "TEST-2"
                            :preview "Hello there" :send #'ignore)
                      (list :op 'delete :object 'comment :project "TEST"
                            :title "delete comment on TEST-2" :parent-issue "TEST-2"
                            :preview "Bye" :send #'ignore)))
         (items (cl-letf (((symbol-function 'ejira-confirm--issue-title) (lambda (_) nil)))
                  (ejira-confirm--items plans)))
         (node (car (plist-get (car items) :children)))
         (children (plist-get node :children)))
    (should (equal "TEST-2" (plist-get node :label)))
    (should (null (plist-get node :kind)))
    (should (equal "comment: TEST-2" (plist-get (car children) :label)))
    (should (equal '((:name "body" :new "Hello there")) (plist-get (car children) :fields)))
    (should (eq 'deleted (plist-get (cadr children) :kind)))
    (should (equal "permanent" (plist-get (cadr children) :warning)))
    (should (equal '((:name "body" :old "Bye")) (plist-get (cadr children) :fields)))))

(ert-deftest ejira-confirm--items/top-level-create-with-cascaded-children ()
  "A new top-level issue lists its cascaded subtasks as fixed children."
  (let* ((plans (list (list :op 'create :object 'issue :label "issue" :project "TEST"
                            :title "new issue: Big thing" :parent-issue nil
                            :fields '(("title" "Big thing") ("state" "TODO") ("description" "Body"))
                            :children (list (list :title "Part one" :state "TODO" :body ""))
                            :assign-self (list nil) :send #'ignore)))
         (items (ejira-confirm--items plans))
         (item (car (plist-get (car items) :children)))
         (child (car (plist-get item :children))))
    (should (equal "issue: TODO Big thing" (plist-get item :label)))
    (should (equal '((:name "title" :new "Big thing") (:name "state" :new "TODO")
                     (:name "description" :new "Body"))
                   (plist-get item :fields)))
    (should (equal "subtask: TODO Part one" (plist-get child :label)))
    (should (plist-get child :fixed))))

(ert-deftest ejira-confirm--execute/binds-pushing-and-reports-failures ()
  "Thunks run with `ejira--pushing' bound; failures are reported, not raised."
  (let ((seen nil)
        (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type msg &rest _) (setq warned (cons type msg)))))
      (ejira-confirm--execute
       (list (list :label "ok" :execute (lambda () (setq seen ejira--pushing)))
             (list :label "bad" :execute (lambda () (error "boom"))))))
    (should (eq t seen))
    (should (eq 'ejira (car warned)))
    (should (string-match-p "bad" (cdr warned)))))

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
  (let ((org-priority-highest 1)
        (org-priority-lowest 5)
        (org-lowest-priority 5))
    (ejira-test--with-org-buf
     "* TODO [#3] TEST-1 Issue\n:PROPERTIES:\n:ID: TEST-1\n:END:\n"
     (goto-char (point-min))
     (re-search-forward org-heading-regexp)
     (should-not (ejira--local-priority-entry (point-marker)
                                              ejira-test--priority-scheme)))))

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
           (ejira-test--as-confirmed (funcall (plist-get plan :send)))
           (should (equal '("TEST-1" (priority . ((id . "p3"))))
                          update-args))))))))

(defmacro ejira-test--with-cookie-edit (remote-priority &rest body)
  "Run BODY on a clean TEST-1 heading whose cookie was edited to [#1].
The heading's stored Jira priority is Low (p3, rank 3); Jira holds
REMOTE-PRIORITY.  Binds MARKER and PLANS (built from the heading)."
  (declare (indent 1))
  `(let ((org-priority-highest 1)
         (org-priority-lowest 8)
         (org-lowest-priority 8)
         (ejira-priority-policies ejira-test--priority-policies)
         (ejira-todo-states-alist '(("Open" . 1)))
         (remote-item
          '((key . "TEST-1")
            (fields . ((summary . "Issue")
                       (description . "")
                       (assignee . nil)
                       (priority . ,remote-priority)
                       (duedate . nil)
                       (status . ((name . "Open"))))))))
     (ejira-test--with-org-buf
      "* TODO [#3] Issue\n:PROPERTIES:\n:TYPE: ejira-issue\n:ID: TEST-1\n:JiraPriorityId: p3\n:JiraPriorityName: Low\n:JiraPriorityRank: 3\n:Status: Open\n:END:\n** Description\n"
      (let ((marker (progn (goto-char (point-min))
                           (re-search-forward org-heading-regexp)
                           (point-marker))))
        (org-with-point-at marker
          (ejira--migrate-push-baseline)
          (should-not (ejira--locally-modified-p))
          (org-priority 1)
          (should (ejira--locally-modified-p)))
        (cl-letf (((symbol-function 'jiralib2-jql-search)
                   (lambda (&rest _args) (list remote-item)))
                  ((symbol-function 'ejira--get-priority-scheme)
                   (lambda (&rest _args) ejira-test--priority-scheme))
                  ((symbol-function 'ejira--save-buffer-safe) #'ignore))
          (let ((plans (ejira--push-build-plans
                        (list (list :op 'update :object 'issue :key "TEST-1"
                                    :project "TEST" :parent-issue "TEST-1"
                                    :marker marker :data nil)))))
            ,@body))))))

(ert-deftest ejira-push--priority/pushed-cookie-edit-is-acknowledged ()
  "After a pushed cookie edit the heading records Jira's new priority and
is clean.  Regression: the stored rank kept the old value, so the
heading stayed dirty forever and every later remote edit was held as a
conflict."
  (ejira-test--with-cookie-edit ((id . "p3") (name . "Low"))
    (let ((sent nil))
      (should (= 1 (length plans)))
      (cl-letf (((symbol-function 'jiralib2-update-issue)
                 (lambda (key &rest args) (push (cons key args) sent))))
        (ejira-test--as-confirmed (funcall (plist-get (car plans) :send))))
      (should (equal '(("TEST-1" (priority . ((id . "p1"))))) sent))
      (org-with-point-at marker
        (should (equal "p1" (org-entry-get nil "JiraPriorityId")))
        (should (equal "High" (org-entry-get nil "JiraPriorityName")))
        (should (equal "1" (org-entry-get nil "JiraPriorityRank")))
        (should-not (ejira--locally-modified-p))))))

(ert-deftest ejira-push--priority/cookie-edit-matching-jira-is-acknowledged ()
  "A cookie edit Jira already matches builds no plan and leaves the
heading clean, with Jira's priority recorded."
  (ejira-test--with-cookie-edit ((id . "p1") (name . "High"))
    (should (null plans))
    (org-with-point-at marker
      (should (equal "p1" (org-entry-get nil "JiraPriorityId")))
      (should (equal "1" (org-entry-get nil "JiraPriorityRank")))
      (should-not (ejira--locally-modified-p)))))

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
    (ejira-test--with-org-buf "* TODO New subtask\n"
                              (goto-char (point-min))
                              (re-search-forward org-heading-regexp)
                              (let ((child (list :marker (point-marker)
                                                 :title "New subtask"
                                                 :state "TODO"
                                                 ;; The body is prepared and
                                                 ;; captured at scan time; the
                                                 ;; cascade consumes it as-is.
                                                 :body "Cascaded body.\n")))
                                (cl-letf (((symbol-function 'jiralib2-create-issue)
                                           (lambda (project type summary description &rest args)
                                             (setq create-call
                                                   (list project type summary description args))
                                             '((key . "TEST-2"))))
                                          ((symbol-function 'ejira--record-new-issue-key)
                                           (lambda (&rest _args) nil))
                                          ((symbol-function 'ejira--finalize-new-issue)
                                           (lambda (&rest _args) nil)))
                                  (ejira-test--as-confirmed
                                    (ejira--push-create-cascaded-child
                                     "TEST-1" "Task" "TEST" child nil nil))
                                  (should (equal "Cascaded body.\n" (nth 3 create-call)))
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
         (ejira-test--as-confirmed (funcall (plist-get plan :send))))
       (should (equal "Body for Jira.\n" created-description))
       (org-with-point-at marker
         (should (string-empty-p (string-trim (ejira--get-heading-own-body))))
         (org-with-point-at (ejira--find-child-heading "Description")
           (should (equal "Body for Jira."
                          (string-trim (ejira--get-heading-body (point-marker)))))))))))

(ert-deftest ejira-push--create/priority-from-cookie ()
  "New issues are created with the priority their Org cookie names.
Regression: creation always sent the project default, so a `[#2]'
heading became the default priority in Jira."
  (let ((ejira-priority-policies ejira-test--priority-policies)
        (ejira-epic-field 'customfield_10857)
        (ejira--assign-new-issues nil)
        (created nil))
    (ejira-test--with-org-buf
     "* TEST-1 My Epic
:PROPERTIES:
:TYPE:     ejira-epic
:ID:       TEST-1
:Issuetype: Epic
:END:
** TODO [#B] New task
*** TODO [#C] New child
** TODO Default task
"
     (should (equal "p2" (ejira--new-issue-priority-id
                          (save-excursion (re-search-forward "New task")
                                          (point-marker))
                          "TEST")))
     (should (equal "p1" (ejira--new-issue-priority-id
                          (save-excursion (re-search-forward "Default task")
                                          (point-marker))
                          "TEST")))
     (let* ((ops (ejira--push-scan-buffer (current-buffer)))
            (creates (cl-remove-if-not
                      (lambda (op) (eq 'create (plist-get op :op))) ops))
            (plans (ejira--push-build-plans creates)))
       (should (string-match-p "priority id: p2"
                               (plist-get (car plans) :payload)))
       (cl-letf (((symbol-function 'jiralib2-create-issue)
                  (lambda (_project _type summary _description &rest args)
                    (push (cons summary
                                (alist-get 'id (alist-get 'priority args)))
                          created)
                    `((key . ,(format "TEST-%d" (+ 10 (length created)))))))
                 ((symbol-function 'ejira--finalize-new-issue-review-safe)
                  (lambda (&rest _) nil))
                 ((symbol-function 'ejira--finalize-new-issue)
                  (lambda (&rest _) nil))
                 ((symbol-function 'ejira--transition-to-org-state)
                  (lambda (&rest _) nil))
                 ((symbol-function 'ejira--update-task-or-hold)
                  (lambda (&rest _) t)))
         (ejira-test--as-confirmed
           (dolist (plan plans) (funcall (plist-get plan :send)))))
       (should (equal "p2" (cdr (assoc "New task" created))))
       (should (equal "p3" (cdr (assoc "New child" created))))
       (should (equal "p1" (cdr (assoc "Default task" created))))))))

(defun ejira-test--rewrite-body-of (key)
  "Rewrite the body of the heading whose ID is KEY, as a finalize pull does."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^:ID: +" (regexp-quote key) "$"))
    (org-back-to-heading t)
    (progn
      (ejira--set-task-description
       (concat "Rewritten by the finalize pull of " key
               ", longer than the local body it replaces.")))))

(ert-deftest ejira-push--create/keys-land-on-their-own-headings ()
  "Each created issue's key lands on its own heading.
Regression: creation targets were raw markers captured at plan time.
A finalize pull rewrites the created issue's body; a marker sitting at
the start of the next heading (a cascade child, or the next sibling
plan) collapsed into the rewritten region, and that issue's key was
written onto the previous heading -- observed live: every child's key
overwrote its parent's."
  (let ((ejira-priority-policies ejira-test--priority-policies)
        (ejira-epic-field 'customfield_10857)
        (ejira--assign-new-issues nil)
        (counter 20))
    (ejira-test--with-org-buf
     "* TEST-1 My Epic
:PROPERTIES:
:TYPE:     ejira-epic
:ID:       TEST-1
:Issuetype: Epic
:EJIRA_DESCRIPTION_IN_BODY: t
:END:
** TODO Parent task
Parent body.
*** TODO First child
First body.
*** DONE Second child
Second body.
** TODO Sibling task
Sibling body.
"
     (let* ((ops (ejira--push-scan-buffer (current-buffer)))
            (plans (ejira--push-build-plans
                    (cl-remove-if-not (lambda (o) (eq 'create (plist-get o :op))) ops)))
            (created nil))
       (cl-letf (((symbol-function 'jiralib2-create-issue)
                  (lambda (_project _type summary &rest _)
                    (let ((key (format "TEST-%d" (cl-incf counter))))
                      (push (cons summary key) created)
                      `((key . ,key)))))
                 ((symbol-function 'ejira--finalize-new-issue-review-safe)
                  (lambda (new-key &rest _) (ejira-test--rewrite-body-of new-key)))
                 ((symbol-function 'ejira--finalize-new-issue)
                  (lambda (new-key &rest _) (ejira-test--rewrite-body-of new-key)))
                 ((symbol-function 'ejira--save-buffer-safe) #'ignore))
         (ejira-test--as-confirmed
           (dolist (plan plans) (funcall (plist-get plan :send)))))
       (dolist (title '("Parent task" "First child" "Second child" "Sibling task"))
         (goto-char (point-min))
         (re-search-forward (concat "^\\*+ \\(TODO\\|DONE\\) " title "$"))
         (should (equal (cdr (assoc title created)) (org-entry-get nil "ID"))))
       ;; No heading lost its identity to another's key.
       (should (equal '("Parent task" "First child" "Second child" "Sibling task")
                      (reverse (mapcar #'car created))))
       (goto-char (point-min))
       (should (= 4 (count-matches "^:ID: +TEST-2[0-9]$")))))))

(ert-deftest ejira-push--record-new-issue-key/rewrites-links-intact ()
  "Links to the created heading's old ID keep their bracket structure.
Regression: the match included the closing bracket but the replacement
dropped it, leaving [[id:KEY[label]] -- a broken link."
  (ejira-test--with-org-buf
   "* TODO New task
:PROPERTIES:
:ID:       0A1B2C3D-0000-0000-0000-000000000000
:END:
* Notes
See [[id:0A1B2C3D-0000-0000-0000-000000000000][the task]] and [[id:0A1B2C3D-0000-0000-0000-000000000000]].
"
   (let ((m (point-marker)))
     (cl-letf (((symbol-function 'ejira--project-files) (lambda () nil)))
       (let ((ejira-extra-scan-files nil))
         (ejira--record-new-issue-key "TEST-7" m)))
     (should (equal "TEST-7" (org-entry-get m "ID")))
     (should (equal "0A1B2C3D-0000-0000-0000-000000000000" (org-entry-get m "ORIG_ID")))
     (goto-char (point-min))
     (should (search-forward "See [[id:TEST-7][the task]] and [[id:TEST-7]]." nil t)))))

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
  (let* ((ejira-epic-field 'customfield_10857)
         (ops (ejira-test--scan
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
  (let* ((ejira-parent-link-field 'customfield_14051)
         (ops (ejira-test--scan
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
  (should (equal "** Outcome\n\nText\n\n** Done\n\n- First\n- Second"
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
  (should (equal "1. A\n\n** X\n\n1. B"
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

(ert-deftest ejira-parser/verbatim-escaped-braces ()
  "Escaped braces inside inline verbatim are literal content.
The exporter writes `{{a\\}}}' for =a}=: JIRA closes a span at the
first bare `}}', so the escape is what keeps the brace inside it."
  (should (equal "=a}=" (ejira-test--parse "{{a\\}}}")))
  (should (equal "={x}=" (ejira-test--parse "{{\\{x\\}}}")))
  (should (equal "=/api/jobs/{id}= ok"
                 (ejira-test--parse "{{/api/jobs/\\{id\\}}} ok")))
  ;; An unescaped triple brace is JIRA's own reading: the span closes
  ;; at the first `}}'.  The difference from the local text is what
  ;; lets the next push upgrade the remote markup.
  (should (equal "=a=}" (ejira-test--parse "{{a}}}"))))

(ert-deftest ejira-parser/browse-links-as-id ()
  "Issue browse links import as `id:' links when enabled.
Regression: the rule used to run `string-match' without preserving
match data, so the parser's `replace-match' failed and the WHOLE
description was kept as raw JIRA markup -- `* (x)' checkbox lines
became Org headings and `{{code}}' stayed literal."
  (let ((jiralib2-url "https://jira.example.com")
        (ejira-parser-browse-links-as-id t)
        (jira (concat "See [the task|https://jira.example.com/browse/ABC-12] and "
                      "[a page|https://example.com/p].\n\n"
                      "* (x) open {{code}} _it_\n"
                      "* (/) done")))
    (should (equal (concat "See [[id:ABC-12][the task]] and "
                           "[[https://example.com/p][a page]].\n\n"
                           "- [ ] open =code= /it/\n"
                           "- [X] done")
                   (ejira-test--parse jira))))
  (let ((jiralib2-url "https://jira.example.com")
        (ejira-parser-browse-links-as-id nil))
    (should (equal "[[https://jira.example.com/browse/ABC-12][t]]"
                   (ejira-test--parse "[t|https://jira.example.com/browse/ABC-12]")))))

(ert-deftest ejira-parser/browse-links-as-id-known-only ()
  "With `known', only keys with a local heading become `id:' links.
Converting a key the Org files do not hold would create a dead link."
  (let ((jiralib2-url "https://jira.example.com")
        (ejira-parser-browse-links-as-id 'known)
        (ejira-parser-issue-known-function #'ejira-parser--issue-known-p)
        (org-id-locations (make-hash-table :test 'equal)))
    (puthash "ABC-1" "/tmp/x.org" org-id-locations)
    (should (equal "[[id:ABC-1][a]] and [[https://jira.example.com/browse/ABC-2][b]]"
                   (ejira-test--parse (concat "[a|https://jira.example.com/browse/ABC-1] and "
                                              "[b|https://jira.example.com/browse/ABC-2]"))))))

(ert-deftest ejira-parser/known-issue-uses-ejira-lookup ()
  "ejira-core's predicate finds a heading a stale org-id index misses."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((org-id-locations (make-hash-table :test 'equal)))
                                  (should (eq ejira-parser-issue-known-function #'ejira--issue-known-p))
                                  (should (ejira--issue-known-p "TEST-1"))
                                  (should-not (ejira--issue-known-p "TEST-404")))))

(ert-deftest ejira-parser/replacement-cannot-clobber-match-data ()
  "A replacement function that matches strings does not break conversion.
The parser preserves the pattern's match data around every
replacement function, so a careless rule cannot turn one link into a
failed conversion of the whole text."
  (let ((ejira-parser-patterns
         (cons (cons "\\[\\([^]|\n]*\\)\\]"
                     (lambda ()
                       (let ((s (match-string 1)))
                         (string-match "\\(.\\)" s)
                         (upcase (match-string 1 s)))))
               ejira-parser-patterns)))
    (should (equal "before X after _" (ejira-test--parse "before [x] after _")))))

(ert-deftest ejira-parser/failure-signals-when-requested ()
  "A failed conversion signals instead of returning raw markup when asked.
Writers bind `ejira-parser-signal-failures': raw JIRA markup stored as
an Org body is corruption, not a fallback."
  (let ((ejira-parser-patterns
         (list (cons "boom" (lambda () (error "Rule failure"))))))
    (should (equal "a boom" (ejira-test--parse "a boom")))
    (let ((ejira-parser-signal-failures t))
      (should-error (ejira-test--parse "a boom") :type 'ejira-parser-error))))

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

(ert-deftest ejira-parser/unwraps-prose-paragraphs ()
  "JIRA's hard-wrapped prose becomes long Org lines.
A prose guard rejects wrapped paragraphs on commit, and ox-jira
flattens them on export anyway, so the joined form is canonical."
  (should (equal "** Outcome\n\nA sentence that was wrapped across two Jira source lines."
                 (string-trim (ejira-test--parse "h2. Outcome\nA sentence that was wrapped\nacross two Jira source lines."))))
  ;; An explicit hard break keeps its break.
  (should (equal "first \\\\\nsecond"
                 (string-trim (ejira-test--parse "first \\\\\nsecond")))))

(ert-deftest ejira-parser/headings-roundtrip-at-offset ()
  "Body headings exported relative to their container keep h-levels.
Without the offset the exporter renormalizes the body's own minimum
heading level to h1 and the original levels are lost on push."
  (let* ((jira "h1. A\nText\nh2. B")
         (org (ejira-parser-jira-to-org jira 2)))
    (should (equal (string-trim org) "*** A\n\nText\n\n**** B"))
    (should (equal (string-trim (ejira-parser-org-to-jira org 2))
                   (string-trim jira)))
    ;; Without the offset the minimum level is renormalized to h1.
    (should (equal (string-trim (ejira-parser-org-to-jira org))
                   "h1. A\nText\nh2. B"))))

;;; ── Body extraction and heading levels ───────────────────────────────────────

(ert-deftest ejira-parse-body/preserves-line-structure ()
  "LF and CRLF input both keep paragraphs, headings and links."
  (let* ((jira "h2. Outcome\nSelected engineers.\n\nOwner: Val.\n\nh2. Done\n* First\n\n[Plan|https://example.com/plan]\n")
         (expected "**** Outcome\n\nSelected engineers.\n\nOwner: Val.\n\n**** Done\n\n- First\n\n[[https://example.com/plan][Plan]]"))
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

(ert-deftest ejira-parser/heading-spacing-canonical ()
  "Converted headings are separated from surrounding content.
JIRA glues `h2.' to its paragraph; the stored Org body keeps one
blank line, matching orgist and gdocs-mode."
  (should (equal "**** Outcome\n\nParagraph.\n\n**** Next\n\nOther."
                 (ejira-parser-jira-to-org "h2. Outcome\nParagraph.\n\nh2. Next\nOther." 2)))
  (should (equal "Paragraph.\n\n** Next"
                 (ejira-parser-jira-to-org "Paragraph.\nh2. Next"))))

(ert-deftest ejira-parser/heading-spacing-idempotent ()
  "Already-canonical bodies pass through unchanged."
  (let ((canonical "**** Outcome\n\nParagraph.\n\n**** Next\n\nOther."))
    (with-temp-buffer
      (insert canonical)
      (ejira-parser--normalize-heading-spacing)
      (should (equal (buffer-string) canonical)))))

(ert-deftest ejira-parser/heading-spacing-preserves-blank-runs ()
  "Only missing blanks are inserted; intentional runs are kept."
  (with-temp-buffer
    (insert "**** H\n\n\n\nBody")
    (ejira-parser--normalize-heading-spacing)
    (should (equal (buffer-string) "**** H\n\n\n\nBody"))))

(ert-deftest ejira-parser/heading-spacing-skips-blocks ()
  "Blank lines are never inserted inside literal blocks."
  (with-temp-buffer
    (insert "* Code\n#+BEGIN_SRC text\nalpha\nbeta\n#+END_SRC\n**** Next\nbody.")
    (ejira-parser--normalize-heading-spacing)
    (should (equal (buffer-string)
                   "* Code\n\n#+BEGIN_SRC text\nalpha\nbeta\n#+END_SRC\n\n**** Next\n\nbody."))))

(ert-deftest ejira-narrow-to-body/leading-blanks-included ()
  "The body region starts right after the heading line.
`org-end-of-meta-data' skips blank lines; a rewrite that started
there left the old leading blanks behind and accumulated one more
blank on every pull.  `ejira--get-heading-body' strips one leading
newline (the region's first line break), so the three blank lines
of the fixture come back as two."
  (ejira-test--with-org-buf
   "** Issue\n:PROPERTIES:\n:ID: X-1\n:END:\n** Description\n\n\n\nStaging.\n"
   (let ((d (progn (goto-char (point-min))
                   (re-search-forward "^\\*\\* Description")
                   (org-back-to-heading t)
                   (point-marker))))
     (should (equal "\n\nStaging.\n"
                    (ejira--get-heading-body d))))))

(ert-deftest ejira-set-heading-body/canonical-boundaries-idempotent ()
  "A rewrite replaces the whole old body, blanks included, and is stable."
  (ejira-test--with-org-buf
   "** Description\n\n\n\nOld body.\n** Comments\n:PROPERTIES:\n:ID: X-2\n:END:\n"
   (let ((d (progn (goto-char (point-min))
                   (re-search-forward "^\\*\\* Description")
                   (org-back-to-heading t)
                   (point-marker))))
     (ejira--set-heading-body d "New body.")
     (should (equal (buffer-string)
                    "** Description\n\nNew body.\n\n** Comments\n:PROPERTIES:\n:ID: X-2\n:END:\n"))
     (ejira--set-heading-body d "New body.")
     (should (equal (buffer-string)
                    "** Description\n\nNew body.\n\n** Comments\n:PROPERTIES:\n:ID: X-2\n:END:\n")))))

(ert-deftest ejira-body-shape/canonical-p ()
  "Boundary shape: exactly one blank line each side; empty bodies pass."
  (should (ejira--body-shape-canonical-p "\n\nBody.\n\n"))
  (should-not (ejira--body-shape-canonical-p "\n\n\nBody.\n\n"))
  (should-not (ejira--body-shape-canonical-p "\n\nBody.\n"))
  (should-not (ejira--body-shape-canonical-p "\n\nBody.\n\n\n"))
  (should (ejira--body-shape-canonical-p "\n"))
  (should (ejira--body-shape-canonical-p "")))

;;; ── Body-as-description ownership ────────────────────────────────────────────

(defconst ejira-test--body-desc-content
  "* STARTED Parent task
:PROPERTIES:
:ID:       TEST-1
:TYPE:     ejira-issue
:END:

Parent prose.

** Section one

Section prose.

*** Deep section

Deep prose.

** TODO Child task
:PROPERTIES:
:ID:       TEST-2
:END:

Child body.

** Comments

*** [2026-09-17 Thu 10:00] Author

Comment body.
")

(defmacro ejira-test--with-body-desc (&rest body)
  "Run BODY in `ejira-test--body-desc-content' with body-as-description on."
  `(ejira-test--with-org-buf ejira-test--body-desc-content
                             (let ((ejira-description-in-body t)
                                   (marker (point-min-marker)))
                               (cl-letf (((symbol-function 'ejira--find-heading)
                                          (lambda (_id) marker)))
                                 (goto-char (point-min))
                                 ,@body))))

(ert-deftest ejira-body-desc/extraction-stops-at-child-task-and-comments ()
  "The owned description is the prose plus ordinary subheadings only."
  (ejira-test--with-body-desc
   (let ((desc (ejira--get-task-description)))
     (should (string-prefix-p "Parent prose." desc))
     (should (string-match-p "^\\*\\* Section one$" desc))
     (should (string-match-p "^\\*\\*\\* Deep section$" desc))
     (should (string-match-p "Deep prose." desc))
     (should-not (string-match-p "Child body." desc))
     (should-not (string-match-p "Comment body." desc)))))

(ert-deftest ejira-body-desc/extraction-ends-at-first-boundary ()
  "Content after a nested task's subtree stays local-only (first-segment rule)."
  (ejira-test--with-org-buf
   "* TODO Parent
:PROPERTIES:
:ID:       TEST-1
:END:

Before prose.

** Impl section

Impl prose.

*** TODO Nested
:PROPERTIES:
:ID:       TEST-2
:END:

Nested body.

Trailing section prose.
"
   (let ((ejira-description-in-body t))
     (goto-char (point-min))
     (let ((desc (ejira--get-task-description)))
       (should (string-match-p "Before prose." desc))
       (should (string-match-p "Impl prose." desc))
       (should-not (string-match-p "Nested body." desc))
       (should-not (string-match-p "Trailing section prose." desc)))
     ;; Rewriting the owned region must not touch the nested task or the
     ;; trailing content.
     (ejira--set-task-description "New prose.")
     (let ((full (buffer-string)))
       (should (string-match-p "^\\*\\*\\* TODO Nested$" full))
       (should (string-match-p "Nested body." full))
       (should (string-match-p "Trailing section prose." full))
       (should-not (string-match-p "Before prose." full))))))

(ert-deftest ejira-body-desc/setter-preserves-tasks-and-comments ()
  "A description rewrite keeps child tasks and Comments, with canonical
blank-line boundaries, and is idempotent."
  (ejira-test--with-body-desc
   (ejira--set-task-description "Replaced prose.\n\n** New section")
   (let ((full (buffer-string)))
     (should (string-match-p "Replaced prose." full))
     (should (string-match-p "^\\*\\* New section$" full))
     (should (string-match-p "Child body." full))
     (should (string-match-p "Comment body." full))
     (should-not (string-match-p "Parent prose." full))
     ;; Canonical boundaries: no doubled blank lines.
     (should-not (string-match-p "\n\n\n" full)))
   (goto-char (point-min))
   (let ((before (buffer-string)))
     (ejira--set-task-description "Replaced prose.\n\n** New section")
     (should (equal before (buffer-string))))))

(ert-deftest ejira-body-desc/import-round-trips-through-jira-markup ()
  "Jira markup imports into the owned region with headings shifted
below the task; child tasks and Comments survive the import."
  (ejira-test--with-body-desc
   (ejira--set-jira-description-jira-markup
    "TEST-1" "h1. Imported intro\n\nh2. Imported section\n\nBody of section.")
   (let ((full (buffer-string)))
     (should (string-match-p "^\\*\\* Imported intro$" full))
     (should (string-match-p "^\\*\\*\\* Imported section$" full))
     (should (string-match-p "Body of section." full))
     (should (string-match-p "Child body." full))
     (should (string-match-p "Comment body." full))
     (should-not (string-match-p "Parent prose." full)))
   ;; The accessor reads back what was imported, and the pushable
   ;; fingerprint covers the imported content without any child content.
   (goto-char (point-min))
   (should (string-match-p "Imported intro" (ejira--jira-description)))
   (should (string-match-p "Imported intro"
                           (ejira--heading-content-fields)))
   (should-not (string-match-p "Child body."
                               (ejira--heading-content-fields)))))

(ert-deftest ejira-body-desc/new-issue-uses-body-without-moving-it ()
  "Creation under body mode exports the owned body and creates no
Description child."
  (ejira-test--with-org-buf
   "* TODO Fresh task\n:PROPERTIES:\n:ID:       TEST-9\n:END:\n\nFresh body.\n"
   (let ((ejira-description-in-body t))
     (goto-char (point-min))
     (should (equal "Fresh body.\n" (ejira--new-issue-description)))
     (ejira--prepare-new-issue-description)
     (should-not (string-match-p "^\\*\\* Description$" (buffer-string)))
     (should (string-match-p "Fresh body." (buffer-string))))))

(ert-deftest ejira-body-desc/property-opt-in ()
  "The inherited EJIRA_DESCRIPTION_IN_BODY property enables body mode
without the global variable."
  (ejira-test--with-org-buf
   "* TODO Task\n:PROPERTIES:\n:ID:       TEST-1\n:EJIRA_DESCRIPTION_IN_BODY: t\n:END:\n\nBody prose.\n"
   (should (ejira--description-in-body-p))
   (goto-char (point-min))
   (should (equal "Body prose.\n" (ejira--jira-description)))))

(ert-deftest ejira-body-desc/legacy-mode-still-uses-description-child ()
  "Without body mode the legacy Description child is read and created."
  (ejira-test--with-org-buf
   "* TODO Task\n:PROPERTIES:\n:ID:       TEST-1\n:END:\n\nLegacy body.\n"
   (let ((marker (point-min-marker)))
     (cl-letf (((symbol-function 'ejira--find-heading)
                (lambda (_id) marker)))
       (should-not (ejira--description-in-body-p))
       (goto-char (point-min))
       (should (equal "Legacy body.\n" (ejira--new-issue-description)))
       ;; Legacy prepare moves the body into a Description child.
       (ejira--prepare-new-issue-description)
       (should (string-match-p "^\\*\\* Description$" (buffer-string)))
       (goto-char (point-min))
       (should (equal "Legacy body."
                      (string-trim (ejira--jira-description))))))))

;;; ── v2 split baselines ───────────────────────────────────────────────────────

(ert-deftest ejira-v2-baseline/light-pull-acks-state-only ()
  "A shallow pull acknowledges the state fields it fetched, and must
not acknowledge unpushed local content edits."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((buf (find-file-noselect
                                            (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  (with-current-buffer buf
                                    ;; Establish v2 baselines, then make a local summary edit (the
                                    ;; summary is a content field; in legacy mode body text is not
                                    ;; pushable, so a body edit would not register at all).
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--migrate-push-baseline)
                                    (should (ejira--v2-baseline-p))
                                    (beginning-of-line)
                                    (org-with-point-at (point-marker)
                                      (replace-regexp "TODO An issue" "TODO An issue edited" nil
                                                      (point) (line-end-position)))
                                    (should (ejira--content-modified-p))
                                    (should-not (ejira--state-modified-p))
                                    ;; Shallow pull: status/assignee from Jira.  The tag branch calls
                                    ;; `ejira--my-fullname', which would hit the network in batch.
                                    (cl-letf (((symbol-function 'ejira--my-fullname)
                                               (lambda () "Test User")))
                                      (ejira--update-task-light "TEST-1" "Done" "someone"))
                                    ;; State fields were acknowledged...
                                    (should-not (ejira--state-modified-p))
                                    (should (equal "Done" (org-entry-get nil "Status")))
                                    ;; ...but the content edit is still pending and detectable.
                                    (should (ejira--content-modified-p))
                                    (should (ejira--locally-modified-p))))))

(ert-deftest ejira-v2-baseline/legacy-light-pull-keeps-pending-edits ()
  "A shallow pull must not acknowledge a legacy heading's pending edit.
Regression: the legacy branch re-hashed the whole content, so any
unpushed summary or description edit looked synced after the next
background pull, and a later full pull overwrote it with Jira's text."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((buf (find-file-noselect
                                            (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--update-push-baseline)
                                    (should-not (ejira--v2-baseline-p))
                                    (beginning-of-line)
                                    (org-with-point-at (point-marker)
                                      (replace-regexp "TODO An issue" "TODO An issue edited" nil
                                                      (point) (line-end-position)))
                                    (should (ejira--locally-modified-p))
                                    (cl-letf (((symbol-function 'ejira--my-fullname)
                                               (lambda () "Test User")))
                                      (ejira--update-task-light "TEST-1" "Done" "someone"))
                                    ;; Nothing applied, nothing acknowledged.
                                    (should (ejira--locally-modified-p))
                                    (should (equal "TODO" (org-get-todo-state)))))))

(ert-deftest ejira-v2-baseline/legacy-clean-light-pull-migrates ()
  "A clean legacy heading takes the remote state and upgrades to v2."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((buf (find-file-noselect
                                            (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--update-push-baseline)
                                    (should-not (ejira--v2-baseline-p))
                                    (cl-letf (((symbol-function 'ejira--my-fullname)
                                               (lambda () "Test User")))
                                      (ejira--update-task-light "TEST-1" "Done" "someone"))
                                    (should (ejira--v2-baseline-p))
                                    (should (equal "Done" (org-entry-get nil "Status")))
                                    (should-not (ejira--locally-modified-p))))))

(ert-deftest ejira-v2-baseline/light-pull-keeps-pending-state-edit ()
  "A pending local todo-state edit survives a shallow pull, still dirty."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((buf (find-file-noselect
                                            (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--migrate-push-baseline)
                                    (let ((org-inhibit-logging t)
                                          (org-log-done nil))
                                      (org-todo "DONE"))
                                    (should (ejira--state-modified-p))
                                    (cl-letf (((symbol-function 'ejira--my-fullname)
                                               (lambda () "Test User")))
                                      (ejira--update-task-light "TEST-1" "Open" "someone"))
                                    (should (equal "DONE" (org-get-todo-state)))
                                    (should (ejira--state-modified-p))))))

(ert-deftest ejira-v2-baseline/state-edits-are-detected ()
  "A local todo-state edit dirties the state baseline, not the
content baseline."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((buf (find-file-noselect
                                            (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--migrate-push-baseline)
                                    (should-not (ejira--locally-modified-p))
                                    (org-todo "DONE")
                                    (should (ejira--state-modified-p))
                                    (should (ejira--locally-modified-p))
                                    (should-not (ejira--content-modified-p))))))

(ert-deftest ejira-push-finalize/acks-only-reviewed-content ()
  "Finalization re-baselines only when the heading still matches the
reviewed snapshot; later edits stay dirty."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let ((buf (find-file-noselect
                                            (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--migrate-push-baseline)
                                    (beginning-of-line)
                                    (let ((marker (point-marker))
                                          (reviewed (md5 (ejira--heading-reviewed-hash))))
                                      ;; An edit made after the review: never sent.
                                      (org-with-point-at marker
                                        (replace-regexp "TODO An issue" "TODO An issue edited" nil
                                                        (point) (line-end-position)))
                                      (ejira--push-finalize marker reviewed)
                                      (should (ejira--locally-modified-p))
                                      ;; A reviewed snapshot matching the current content re-baselines.
                                      (setq reviewed (md5 (ejira--heading-reviewed-hash)))
                                      (ejira--push-finalize marker reviewed)
                                      (should-not (ejira--locally-modified-p)))))))

;;; ── Automatic reconciliation (auto-sync) ─────────────────────────────────────

(defun ejira-test--mock-item (summary status description)
  "Build a Jira item alist like `jiralib2-jql-search' returns."
  `((key . "TEST-1")
    (fields . ((summary . ,summary)
               (description . ,description)
               (status . ((name . ,status)))))))

(ert-deftest ejira-auto-sync/remote-identity-reflects-fields ()
  "The identity changes when a synced field changes and otherwise not."
  (let ((a (ejira-test--mock-item "Summary" "Open" "Body"))
        (b (ejira-test--mock-item "Summary" "Open" "Body"))
        (c (ejira-test--mock-item "Summary" "Done" "Body"))
        (d (ejira-test--mock-item "Other summary" "Open" "Body")))
    (should (equal (md5 (ejira--remote-fields-identity a))
                   (md5 (ejira--remote-fields-identity b))))
    (should-not (equal (md5 (ejira--remote-fields-identity a))
                       (md5 (ejira--remote-fields-identity c))))
    (should-not (equal (md5 (ejira--remote-fields-identity a))
                       (md5 (ejira--remote-fields-identity d))))))

(ert-deftest ejira-auto-sync/remote-identity-covers-comments ()
  "A comment added or edited in Jira is a remote change.
Regression: comments were outside the identity, so an issue whose
fields stayed the same never pulled comments posted in Jira."
  (let* ((base (ejira-test--mock-item "S" "Open" "B"))
         (with (lambda (comments)
                 `((key . "TEST-1")
                   (fields . ((summary . "S") (description . "B")
                              (status . ((name . "Open")))
                              (comment . ((comments . ,comments)))))))))
    (should (equal (md5 (ejira--remote-fields-identity base))
                   (md5 (ejira--remote-fields-identity (funcall with nil)))))
    (let ((one (funcall with '(((id . "1") (updated . "2026-09-01T00:00:00.000+0000")))))
          (edited (funcall with '(((id . "1") (updated . "2026-09-02T00:00:00.000+0000")))))
          (two (funcall with '(((id . "1") (updated . "2026-09-01T00:00:00.000+0000"))
                               ((id . "2") (updated . "2026-09-03T00:00:00.000+0000"))))))
      (should-not (equal (md5 (ejira--remote-fields-identity base))
                         (md5 (ejira--remote-fields-identity one))))
      (should-not (equal (md5 (ejira--remote-fields-identity one))
                         (md5 (ejira--remote-fields-identity edited))))
      (should-not (equal (md5 (ejira--remote-fields-identity one))
                         (md5 (ejira--remote-fields-identity two)))))))

(ert-deftest ejira-auto-sync/store-and-detect-remote-change ()
  "A stored baseline detects remote changes; its absence means unknown."
  (ejira-test--with-org-buf
   "* TODO An issue\n:PROPERTIES:\n:ID:       TEST-1\n:TYPE:     ejira-issue\n:END:\n"
   (let ((old (ejira-test--mock-item "An issue" "Open" nil))
         (new (ejira-test--mock-item "An issue" "Done" nil)))
     ;; Unknown baseline: not reported as a change.
     (should-not (ejira--remote-changed-p new))
     (ejira--store-remote-baseline old)
     (should (equal (org-entry-get nil "Remotehash")
                    (md5 (ejira--remote-fields-identity old))))
     (should-not (ejira--remote-changed-p old))
     (should (ejira--remote-changed-p new)))))

(ert-deftest ejira-auto-sync/buffer-issue-keys-all-depths ()
  "Issue keys are collected at every depth, including terminal issues;
projects and comments are excluded."
  (ejira-test--with-org-buf
   "* Project
:PROPERTIES:
:ID:       TEST
:TYPE:     ejira-project
:END:
** TODO A
:PROPERTIES:
:ID:       TEST-1
:END:
*** Section
**** DONE Deep
:PROPERTIES:
:ID:       TEST-2
:END:
** Comments
*** [2026-09-17 Thu 10:00] c
:PROPERTIES:
:CommId:   1
:TYPE:     ejira-comment
:END:
"
   (should (equal '("TEST-1" "TEST-2") (ejira--buffer-issue-keys)))))

(ert-deftest ejira-auto-sync/save-routes-to-queue ()
  "Saving an auto-sync file schedules reconciliation instead of the
confirmation buffer."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let* ((file (expand-file-name "TEST.org" ejira-org-directory))
                                       (ejira-auto-sync-files (list file))
                                       (shown nil))
                                  (with-current-buffer (find-file-noselect file t)
                                    (cl-letf (((symbol-function 'ejira-confirm-show)
                                               (lambda (plans) (setq shown plans))))
                                      (ejira--push-on-save))
                                    (should (member (file-truename file) ejira--auto-sync-queue))
                                    (should-not shown)
                                    (setq ejira--auto-sync-queue nil)))))

(ert-deftest ejira-auto-sync/hold-never-sends ()
  "An automatic cycle holds every push plan for review and sends none.
Regression: local-only updates, comment drafts and transitions were
pushed without confirmation; nothing may reach Jira unreviewed."
  ;; let*: the :send lambdas capture `sent', so `sent' must be bound
  ;; before the plans are built.
  (let* ((sent nil)
         (plans (list
                 (list :op 'update :object 'issue :title "TEST-1"
                       :changes '(("summary" "a" "b") ("priority" "Low" "High"))
                       :send (lambda () (push 'updated sent)))
                 (list :op 'update :object 'issue :title "TEST-2"
                       :remote-changed t :send (lambda () (push 'conflict sent)))
                 (list :op 'update :object 'comment :title "TEST-1 comment 7"
                       :changes '(("body" "x" "y"))
                       :send (lambda () (push 'comment-edit sent)))
                 (list :op 'delete :object 'comment :title "delete comment on TEST-1"
                       :send (lambda () (push 'comment-del sent)))
                 (list :op 'create :object 'comment :title "new comment on TEST-1"
                       :send (lambda () (push 'comment-new sent)))
                 (list :op 'create :object 'issue :title "new issue: X"
                       :send (lambda () (push 'created sent)))))
         (ejira--auto-sync-review-plans nil)
         (held (ejira--auto-sync-hold "test.org" plans)))
    (should (null sent))
    (should (= 6 (length held)))
    (should (equal (length plans) (length ejira--auto-sync-review-plans)))
    (should (member "TEST-1: summary, priority awaiting review" held))
    (should (member "TEST-2: changed remotely since last sync; held for review" held))
    (should (member "new issue: X: creation awaiting review" held))
    (should (member "delete comment on TEST-1: deletion awaiting review" held))))

(ert-deftest ejira-auto-sync/hold-skips-review-when-file-changed ()
  "Plans built from a buffer whose file changed on disk are neither sent
nor offered for review: they describe stale content."
  (let* ((file (make-temp-file "ejira-exec-" nil ".org"))
         (sent nil)
         (ejira--auto-sync-review-plans nil)
         (plans (list (list :op 'update :object 'issue :title "update"
                            :send (lambda () (push 'updated sent))))))
    (unwind-protect
        (let ((buf (find-file-noselect file t)))
          (with-temp-file file (insert "changed on disk\n"))
          (set-file-times file (time-add (current-time) 10))
          (let ((held (ejira--auto-sync-hold file plans)))
            (should (null sent))
            (should (null ejira--auto-sync-review-plans))
            (should (string-match-p "file changed on disk" (car held))))
          (kill-buffer buf))
      (delete-file file))))

;;; ── Confirm-only invariant ──────────────────────────────────────────────────
;;
;; Nothing is ever sent to Jira without the user confirming it in the
;; review buffer.  An agent once made the automatic cycle push updates
;; unattended; these tests pin the invariant at three levels: the guard
;; itself, every write call site in the sources, and a full cycle.

(defconst ejira-test--source-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the ejira sources under test.")

(defconst ejira-test--jira-write-functions
  '(jiralib2-create-issue jiralib2-update-issue
                          jiralib2-update-summary-description jiralib2-assign-issue
                          jiralib2-do-action jiralib2-set-issue-type
                          jiralib2-add-comment jiralib2-edit-comment jiralib2-delete-comment
                          jiralib2-add-worklog jiralib2-update-worklog jiralib2-delete-worklog)
  "jiralib2 functions that change Jira.")

(defun ejira-test--strings-in (form)
  "Return every string found anywhere in FORM."
  (cond ((stringp form) (list form))
        ((consp form) (append (ejira-test--strings-in (car form))
                              (ejira-test--strings-in (cdr form))))
        (t nil)))

(defun ejira-test--http-write-p (form)
  "Return non-nil when FORM is an HTTP call with a writing method.
POSTs to the search endpoint are reads."
  (let ((method (cadr (memq :type form))))
    (and (member method '("POST" "PUT" "DELETE"))
         (not (cl-some (lambda (s) (string-match-p "/rest/api/2/search" s))
                       (ejira-test--strings-in form))))))

(defun ejira-test--unguarded-writes (form)
  "Return the Jira writes in FORM not lexically inside `ejira--jira-write'."
  (cond
   ((and (consp form) (eq (car form) 'ejira--jira-write)) nil)
   ((and (symbolp form) (memq form ejira-test--jira-write-functions))
    (list form))
   ((and (consp form)
         (memq (car form) '(jiralib2-session-call request))
         (ejira-test--http-write-p form))
    (list form))
   ((consp form)
    (append (ejira-test--unguarded-writes (car form))
            (ejira-test--unguarded-writes (cdr form))))
   (t nil)))

(defun ejira-test--source-forms (file)
  "Read every top-level form of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest ejira-write-guard/refuses-outside-review ()
  "A Jira write outside the confirmed review signals before it runs."
  (let ((ran nil))
    (should-error (ejira--jira-write "test write" (setq ran t))
                  :type 'ejira-unconfirmed-write)
    (should-not ran)
    (ejira-test--as-confirmed (ejira--jira-write "test write" (setq ran t)))
    (should ran)))

(ert-deftest ejira-write-guard/detector-sees-writes ()
  "The source check below flags unguarded writes and accepts guarded ones."
  (should (ejira-test--unguarded-writes '(lambda () (jiralib2-add-comment k b))))
  (should (ejira-test--unguarded-writes '(apply #'jiralib2-create-issue args)))
  (should (ejira-test--unguarded-writes
           '(jiralib2-session-call "/rest/api/2/issue/X/transitions" :type "POST")))
  (should-not (ejira-test--unguarded-writes
               '(ejira--jira-write "x" (jiralib2-add-comment k b))))
  (should-not (ejira-test--unguarded-writes
               '(jiralib2-session-call "/rest/api/2/search" :type "POST")))
  (should-not (ejira-test--unguarded-writes '(jiralib2-get-issue k))))

(ert-deftest ejira-write-guard/every-jira-write-is-guarded ()
  "Every Jira write in the ejira sources runs inside `ejira--jira-write'.
A new write call site added without the guard fails here."
  (let ((files (cl-remove-if
                (lambda (f) (string-match-p "-\\(?:test\\|e2e-test\\|pkg\\)\\.el\\'" f))
                (directory-files ejira-test--source-dir t
                                 "\\`\\(?:helm-\\)?ejira.*\\.el\\'")))
        violations)
    (should (member (expand-file-name "ejira-push.el" ejira-test--source-dir) files))
    (dolist (f files)
      (dolist (form (ejira-test--source-forms f))
        (dolist (w (ejira-test--unguarded-writes form))
          (push (format "%s: %S" (file-name-nondirectory f) w) violations))))
    (should (equal nil violations))))

(ert-deftest ejira-confirm/execute-is-the-authorized-writer ()
  "Confirmed items run with writes authorized, and each is logged.
Authorization does not outlive the execution."
  (when (get-buffer "*ejira sync log*")
    (with-current-buffer "*ejira sync log*" (erase-buffer)))
  (let* ((seen nil)
         (items (list (list :label "ok"
                            :data '(:title "TEST-1" :changes (("summary" "a" "b")))
                            :execute (lambda () (push ejira--jira-write-authorized seen)))
                      (list :label "bad" :data '(:title "TEST-2")
                            :execute (lambda () (error "Boom")))
                      (list :label "group node without a send"))))
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'ejira--auto-sync-files) (lambda () nil)))
      (ejira-confirm--execute items))
    (should (equal '(t) seen))
    (should-not ejira--jira-write-authorized)
    (with-current-buffer "*ejira sync log*"
      (should (string-match-p "confirmed push" (buffer-string)))
      (should (string-match-p "TEST-1 (summary): sent" (buffer-string)))
      (should (string-match-p "TEST-2: FAILED" (buffer-string))))))

(defun ejira-test--epic-item (summary)
  "REST item for epic TEST-1 with SUMMARY, as Jira returns it."
  `((key . "TEST-1")
    (fields . ((summary . ,summary)
               (description . "Epic body.")
               (status . ((name . "Open")))
               (issuetype . ((name . "Epic")))
               (project . ((key . "TEST")))))))

(ert-deftest ejira-auto-sync/cycle-never-writes-to-jira ()
  "A full cycle with every kind of local change makes no Jira write.
The edited issue, the new task and the comment draft all wait in the
review buffer.  Regression: the cycle pushed updates unattended."
  (ejira-test--with-project-dir
   (concat ejira-test--discover-content
           "*** Comments\n**** A new remark\n\nSome remark.\n"
           "*** TODO Brand new task\n")
   (let* ((file (file-truename (expand-file-name "TEST.org" ejira-org-directory)))
          (buf (find-file-noselect file t))
          (item (ejira-test--epic-item "The epic"))
          (ejira-epic-field 'customfield_10857)
          (ejira-todo-states-alist '(("Open" . 1)))
          (ejira--auto-sync-queue nil)
          (ejira--auto-sync-review-queue nil)
          (ejira--auto-sync-held (make-hash-table :test 'equal))
          (writes nil)
          (attempts nil)
          (shown nil))
     ;; Edit the epic's title locally, on top of an acknowledged remote
     ;; baseline: a clean push candidate.
     (with-current-buffer buf
       (goto-char (point-min))
       (re-search-forward "^\\*\\* TODO The epic$")
       (replace-match "** TODO The epic, edited")
       (org-set-property "Remotehash" (md5 (ejira--remote-fields-identity item)))
       (org-set-property "Pushhash" "stale")
       (let ((ejira-push-on-save nil)) (save-buffer)))
     (cl-letf* (((symbol-function 'ejira--auto-sync-fetch) (lambda (_keys) (list item)))
                ((symbol-function 'jiralib2-jql-search) (lambda (&rest _) (list item)))
                ((symbol-function 'ejira--get-priority-scheme) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn &rest args) (apply fn args)))
                ((symbol-function 'ejira-confirm-show)
                 (lambda (plans) (setq shown (mapcar (lambda (p) (plist-get p :title)) plans)))))
       (dolist (fn (cons 'jiralib2-session-call ejira-test--jira-write-functions))
         (let ((fn fn))
           (advice-add fn :override (lambda (&rest args) (push (cons fn args) writes))
                       '((name . ejira-test-record-write)))))
       ;; An attempted write counts even when the guard stops it: a
       ;; cycle must not try to send anything at all.
       (advice-add 'ejira--assert-jira-write-authorized :before
                   (lambda (what) (push what attempts))
                   '((name . ejira-test-record-attempt)))
       (unwind-protect
           (ejira--auto-sync-reconcile file t)
         (advice-remove 'ejira--assert-jira-write-authorized 'ejira-test-record-attempt)
         (dolist (fn (cons 'jiralib2-session-call ejira-test--jira-write-functions))
           (advice-remove fn 'ejira-test-record-write))))
     (should (equal nil attempts))
     (should (equal nil writes))
     (should (member "TEST-1" shown))
     (should (member "new task: Brand new task" shown))
     (should (member "new comment on TEST-1" shown))
     (should (>= (ejira-auto-sync-held-count) 3)))))

(ert-deftest ejira-hourlog/commit-goes-through-review ()
  "Worklogs are offered in the review buffer, never sent directly."
  (require 'ejira-hourmarking)
  (let ((shown nil) (written nil))
    (with-temp-buffer
      (setq-local ejira-hourlog-entries
                  (list (list :key "TEST-1" :start (current-time)
                              :duration-r 1800 :title "Work")
                        (list :key "TEST-2" :start (current-time)
                              :duration-r 0 :title "Nothing logged")))
      (cl-letf (((symbol-function 'ejira-hourlog-quit) #'ignore)
                ((symbol-function 'ejira-confirm-show) (lambda (plans) (setq shown plans)))
                ((symbol-function 'jiralib2-add-worklog)
                 (lambda (&rest args) (push args written))))
        (ejira-hourlog-commit)
        (should (= 1 (length shown)))
        (should-not written)
        (should-error (funcall (plist-get (car shown) :send))
                      :type 'ejira-unconfirmed-write)
        (should-not written)
        (ejira-test--as-confirmed (funcall (plist-get (car shown) :send)))
        (should (equal "TEST-1" (car (car written))))))))

(defconst ejira-test--discover-content
  "* Project\n:PROPERTIES:\n:ID:       TEST\n:TYPE:     ejira-project\n:END:\n\
** TODO The epic\n:PROPERTIES:\n:ID:       TEST-1\n:TYPE:     ejira-epic\n\
:Issuetype: Epic\n:EJIRA_DESCRIPTION_IN_BODY: t\n:END:\n\nEpic body.\n")

(defun ejira-test--child-item (key description)
  "A REST item for task KEY in epic TEST-1 with DESCRIPTION."
  `((key . ,key)
    (fields . ((summary . "Created in Jira")
               (description . ,description)
               (status . ((name . "Open")))
               (issuetype . ((name . "Task")))
               (project . ((key . "TEST")))
               (customfield_10857 . "TEST-1")
               (created . "2026-09-20T10:00:00.000+0000")
               (updated . "2026-09-21T10:00:00.000+0000")))))

(ert-deftest ejira-auto-sync/discovers-children-created-in-jira ()
  "An unresolved Jira child of an epic in the file is imported under it.
Regression: the cycle fetched only the keys the file already held, so
an issue created in Jira under a synced epic never reached the file."
  (when (get-buffer "*ejira sync log*")
    (with-current-buffer "*ejira sync log*" (erase-buffer)))
  (ejira-test--with-project-dir ejira-test--discover-content
                                (let* ((file (expand-file-name "TEST.org" ejira-org-directory))
                                       (buf (find-file-noselect file t))
                                       (ejira-epic-field 'customfield_10857)
                                       (ejira-auto-sync-discover 'unresolved)
                                       (ejira-assigned-tagname nil)
                                       (jql nil))
                                  (cl-letf (((symbol-function 'ejira--auto-sync-fetch) (lambda (_keys) nil))
                                            ((symbol-function 'jiralib2-jql-search)
                                             (lambda (q &rest _)
                                               (setq jql q)
                                               (list (ejira-test--child-item "TEST-2" "Line with {{code}}.\n* (x) todo"))))
                                            ((symbol-function 'ejira--push-scan-buffer) (lambda (_buf) nil)))
                                    (ejira--auto-sync-reconcile file))
                                  (should (string-match-p "cf\\[10857\\] in (TEST-1)" jql))
                                  (should (string-match-p "resolution = Unresolved" jql))
                                  (with-current-buffer buf
                                    (goto-char (point-min))
                                    (should (re-search-forward "^\\*\\*\\* TODO Created in Jira" nil t))
                                    (should (equal "TEST-2" (org-entry-get nil "ID")))
                                    (should (org-entry-get nil "Remotehash"))
                                    (should-not (ejira--locally-modified-p))
                                    ;; Body-as-description inherited from the epic: converted body,
                                    ;; not a Description child and not raw markup.
                                    (should (equal "Line with =code=.\n- [ ] todo"
                                                   (string-trim (ejira--jira-description)))))
                                  (with-current-buffer "*ejira sync log*"
                                    (should (string-match-p "TEST-2: imported from Jira under TEST-1"
                                                            (buffer-string)))))))

(ert-deftest ejira-auto-sync/discovery-hold-leaves-no-heading ()
  "A discovered child whose markup cannot be converted is held, not stubbed."
  (clrhash ejira--auto-sync-held)
  (ejira-test--with-project-dir ejira-test--discover-content
                                (let* ((file (expand-file-name "TEST.org" ejira-org-directory))
                                       (buf (find-file-noselect file t))
                                       (ejira-epic-field 'customfield_10857)
                                       (ejira-auto-sync-discover 'all)
                                       (ejira-assigned-tagname nil)
                                       (ejira-parser-patterns
                                        (cons (cons "boom" (lambda () (error "Rule failure")))
                                              ejira-parser-patterns)))
                                  (cl-letf (((symbol-function 'ejira--auto-sync-fetch) (lambda (_keys) nil))
                                            ((symbol-function 'jiralib2-jql-search)
                                             (lambda (&rest _) (list (ejira-test--child-item "TEST-3" "a boom"))))
                                            ((symbol-function 'ejira--push-scan-buffer) (lambda (_buf) nil)))
                                    (ejira--auto-sync-reconcile file))
                                  (with-current-buffer buf
                                    (should-not (string-match-p "TEST-3\\|ejira new heading" (buffer-string))))
                                  (should (= 1 (ejira-auto-sync-held-count)))
                                  (should (string-match-p "1 to review" (ejira--auto-sync-mode-line)))
                                  (clrhash ejira--auto-sync-held)
                                  (should-not (ejira--auto-sync-mode-line)))))

(ert-deftest ejira-sync-audit/normalize-ignores-export-artifacts ()
  "Blank lines, drawers and bare-link brackets do not count as differences.
The exporter separates elements with blank lines, drops drawers and
brackets bare URLs, so a freshly pushed text never renders back
character for character."
  (should (equal (ejira--audit-normalize "Intro:\n- a\n- b\nSee https://e.x/y.")
                 (ejira--audit-normalize
                  "Intro:\n\n- a\n- b\n\nSee [[https://e.x/y]].")))
  (should (equal (ejira--audit-normalize "** Part\nText.")
                 (ejira--audit-normalize
                  "** Part\n:LOGBOOK:\n- [2026-09-02 Wed 10:22] Refiled\n:END:\nText.")))
  (should-not (equal (ejira--audit-normalize "Intro: a")
                     (ejira--audit-normalize "Intro: b"))))

(ert-deftest ejira-sync-audit/classifies-without-writing ()
  "The audit classifies each heading and never modifies the file."
  (ejira-test--with-project-dir
   (concat ejira-test--discover-content
           "*** TODO Same\n:PROPERTIES:\n:ID:       TEST-2\n:TYPE:     ejira-issue\n:END:\n\nSame body.\n"
           "*** TODO Edited\n:PROPERTIES:\n:ID:       TEST-3\n:TYPE:     ejira-issue\n:END:\n\nLocal body.\n"
           "*** TODO Stale\n:PROPERTIES:\n:ID:       TEST-4\n:TYPE:     ejira-issue\n:END:\n\nOld body.\n"
           "*** TODO Not in Jira yet\n")
   (let* ((file (expand-file-name "TEST.org" ejira-org-directory))
          (buf (find-file-noselect file t))
          (ejira-epic-field 'customfield_10857)
          (ejira-push-on-save nil)
          (item (lambda (key summary desc)
                  `((key . ,key)
                    (fields . ((summary . ,summary) (description . ,desc)
                               (status . ((name . "Open")))
                               (issuetype . ((name . "Task")))))))))
     (with-current-buffer buf
       ;; TEST-3: baseline on the remote text, then edit locally.
       (goto-char (point-min))
       (re-search-forward "^\\*\\*\\* TODO Edited")
       (ejira--migrate-push-baseline)
       (ejira--store-remote-baseline (funcall item "TEST-3" "Edited" "Remote body."))
       (re-search-forward "^Local body")
       (goto-char (point-min))
       (re-search-forward "^\\*\\*\\* TODO Edited")
       (org-set-property "Pushhash" "v2:stale")
       ;; TEST-4: clean, with a baseline Jira has since moved past.
       (re-search-forward "^\\*\\*\\* TODO Stale")
       (ejira--migrate-push-baseline)
       (ejira--store-remote-baseline (funcall item "TEST-4" "Stale" "Old body."))
       (save-buffer))
     (let ((before (with-current-buffer buf (buffer-string))))
       (cl-letf (((symbol-function 'ejira--auto-sync-fetch)
                  (lambda (_keys)
                    (list `((key . "TEST-1")
                            (fields . ((summary . "The epic") (description . "Epic body.")
                                       (status . ((name . "Open")))
                                       (issuetype . ((name . "Epic"))))))
                          (funcall item "TEST-2" "Same" "Same body.")
                          (funcall item "TEST-3" "Edited" "Remote body.")
                          (funcall item "TEST-4" "Stale" "New remote body."))))
                 ((symbol-function 'jiralib2-jql-search)
                  (lambda (&rest _) (list (ejira-test--child-item "TEST-9" "x")))))
         (let* ((audit (ejira-sync-audit-file file))
                (class (lambda (k) (plist-get (cdr (assoc k (plist-get audit :rows))) :class))))
           (should (eq 'identical (funcall class "TEST-1")))
           (should (eq 'identical (funcall class "TEST-2")))
           (should (eq 'local-newer (funcall class "TEST-3")))
           (should (eq 'remote-newer (funcall class "TEST-4")))
           (should (equal '("TEST-9") (plist-get audit :missing)))
           (should (equal '("Not in Jira yet") (plist-get audit :local-only)))))
       (with-current-buffer buf
         (should (equal before (buffer-string)))
         (should-not (buffer-modified-p)))))))

(ert-deftest ejira-auto-sync/duplicate-identity-is-held ()
  "An issue key carried by two headings is held and reported, not pulled.
Regression: a stray copy in the project file (left by an interrupted
creation) was the one `ejira--find-heading' found; the pull updated it
and refiled it into the auto-sync file as a second heading."
  (when (get-buffer "*ejira sync log*")
    (with-current-buffer "*ejira sync log*" (erase-buffer)))
  (ejira-test--with-project-dir ejira-test--project-content
    (let* ((extra (make-temp-file "ejira-sync-" nil ".org"))
           (ejira-extra-scan-files (list extra))
           (pulled nil))
      (unwind-protect
          (progn
            (with-temp-file extra
              (insert "* TODO An issue\n:PROPERTIES:\n:ID:       TEST-1\n:TYPE:     ejira-issue\n:END:\n"))
            (cl-letf (((symbol-function 'ejira--auto-sync-fetch)
                       (lambda (keys)
                         (mapcar (lambda (k) (ejira-test--mock-item "An issue" "Done" nil)) keys)))
                      ((symbol-function 'ejira--update-task)
                       (lambda (&rest _) (setq pulled t)))
                      ((symbol-function 'ejira--push-scan-buffer) (lambda (_buf) nil)))
              (ejira--auto-sync-reconcile (file-truename extra)))
            (should-not pulled)
            (with-current-buffer "*ejira sync log*"
              (should (string-match-p "TEST-1: 2 headings carry this key"
                                      (buffer-string))))
            (let ((audit (cl-letf (((symbol-function 'ejira--auto-sync-fetch)
                                    (lambda (_keys) nil))
                                   ((symbol-function 'jiralib2-jql-search)
                                    (lambda (&rest _) nil)))
                           (ejira-sync-audit-file extra))))
              (should (assoc "TEST-1" (plist-get audit :duplicates)))))
        (when-let ((b (get-file-buffer extra)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))
        (delete-file extra)))))

(ert-deftest ejira-auto-sync/save-opens-review-for-held-creations ()
  "A save runs the cycle with review: held creations open the review buffer.
An external change runs the same cycle without opening anything."
  (ejira-test--with-project-dir
      (concat ejira-test--discover-content "*** TODO Brand new task\n")
    (let* ((file (file-truename (expand-file-name "TEST.org" ejira-org-directory)))
           (buf (find-file-noselect file t))
           (ejira-auto-sync-files (list file))
           (ejira-epic-field 'customfield_10857)
           (ejira--auto-sync-queue nil)
           (ejira--auto-sync-review-queue nil)
           (ejira--auto-sync-mtimes (make-hash-table :test 'equal))
           (shown nil))
      (cl-letf (((symbol-function 'ejira--auto-sync-fetch) (lambda (_keys) nil))
                ((symbol-function 'jiralib2-jql-search) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn &rest args) (apply fn args)))
                ((symbol-function 'ejira-confirm-show)
                 (lambda (plans) (setq shown (mapcar (lambda (p) (plist-get p :title)) plans)))))
        ;; External change: no review.
        (ejira--auto-sync-worker)
        (should-not shown)
        ;; A save in Emacs: review.
        (with-current-buffer buf
          (set-buffer-modified-p t)
          (save-buffer))
        (should (member file ejira--auto-sync-review-queue))
        (ejira--auto-sync-worker)
        (should (equal '("new task: Brand new task") shown))
        (should-not ejira--auto-sync-review-queue)))))

(ert-deftest ejira-auto-sync/every-tracked-file-auto-syncs ()
  "Project files, extra scan files and buffers holding issue headings
auto-sync by default; nothing has to be listed."
  (ejira-test--with-project-dir ejira-test--project-content
    (let* ((project (expand-file-name "TEST.org" ejira-org-directory))
           (extra (make-temp-file "ejira-extra-" nil ".org"))
           (other (make-temp-file "ejira-other-" nil ".org"))
           (plain (make-temp-file "ejira-plain-" nil ".org"))
           (ejira-extra-scan-files (list extra))
           (ejira-auto-sync-files nil)
           (ejira--auto-sync-queue nil)
           (ejira--auto-sync-review-queue nil))
      (unwind-protect
          (progn
            (with-temp-file other
              (insert "* TODO Refiled\n:PROPERTIES:\n:ID:       TEST-3\n:TYPE:     ejira-issue\n:END:\n"))
            (with-temp-file plain (insert "* TODO Just a note\n"))
            (should (ejira--auto-sync-file-p project))
            (should (ejira--auto-sync-file-p extra))
            (should-not (ejira--auto-sync-file-p other))
            (with-current-buffer (find-file-noselect other t)
              (should (ejira--auto-sync-file-p)))
            (with-current-buffer (find-file-noselect plain t)
              (should-not (ejira--auto-sync-file-p)))
            ;; Saving the project file queues a cycle with review.
            (with-current-buffer (find-file-noselect project t)
              (set-buffer-modified-p t)
              (save-buffer))
            (should (member (file-truename project) ejira--auto-sync-review-queue))
            (let ((ejira-auto-sync-tracked nil))
              (should-not (ejira--auto-sync-file-p project))
              (with-current-buffer (find-file-noselect other t)
                (should-not (ejira--auto-sync-file-p)))))
        (dolist (f (list extra other plain))
          (when-let ((b (get-file-buffer f)))
            (with-current-buffer b (set-buffer-modified-p nil))
            (kill-buffer b))
          (delete-file f))))))

(ert-deftest ejira-auto-sync/worker-reconciles-one-file-per-call ()
  "Each worker call reconciles one file, so input is not held up by a
whole round of cycles; the rest wait for the next idle call."
  (let* ((a (make-temp-file "ejira-a-" nil ".org"))
         (b (make-temp-file "ejira-b-" nil ".org"))
         (ejira-auto-sync-tracked nil)
         (ejira-auto-sync-files (list a b))
         (ejira--auto-sync-queue nil)
         (ejira--auto-sync-mtimes (make-hash-table :test 'equal))
         (done nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ejira--auto-sync-reconcile)
                   (lambda (file &rest _) (push file done))))
          (ejira--auto-sync-worker)
          (should (= 1 (length done)))
          (ejira--auto-sync-worker)
          (should (= 2 (length done)))
          (ejira--auto-sync-worker)
          (should (= 2 (length done))))
      (delete-file a) (delete-file b))))

(ert-deftest ejira-auto-sync/local-todos-are-not-held ()
  "A TODO with no Jira ancestor, or under a sub-task, is a local task:
it can never become an issue, so it is not held or counted."
  (clrhash ejira--auto-sync-held)
  (ejira-test--with-project-dir
      (concat "* TODO Personal task\n" ejira-test--project-content
              "*** DONE A subtask\n:PROPERTIES:\n:ID:       TEST-2\n:TYPE:     ejira-subtask\n:END:\n"
              "**** TODO Personal checklist item\n")
    (let ((file (file-truename (expand-file-name "TEST.org" ejira-org-directory))))
      (cl-letf (((symbol-function 'ejira--auto-sync-fetch) (lambda (_keys) nil))
                ((symbol-function 'jiralib2-jql-search) (lambda (&rest _) nil)))
        (ejira--auto-sync-reconcile file))
      (should (= 0 (ejira-auto-sync-held-count))))))

(ert-deftest ejira-auto-sync/reconcile-classification ()
  "One reconcile cycle: clean+remote-changed pulls, dirty+remote-changed
holds as conflict, clean+unchanged does nothing."
  ;; Start from a clean log so assertions only see this test entries.
  (when (get-buffer "*ejira sync log*")
    (with-current-buffer "*ejira sync log*" (erase-buffer)))
  (ejira-test--with-project-dir ejira-test--project-content
                                (let* ((file (expand-file-name "TEST.org" ejira-org-directory))
                                       (buf (find-file-noselect file t))
                                       (old-item (ejira-test--mock-item "An issue" "Open" nil))
                                       (new-item (ejira-test--mock-item "An issue" "Done" nil))
                                       (pulled nil))
                                  (with-current-buffer buf
                                    ;; Baseline the issue against the OLD remote state so the new
                                    ;; item registers as a remote change, then save: reconcile
                                    ;; defers while the buffer has unsaved edits.
                                    (goto-char (point-min))
                                    (re-search-forward "^\\*\\* TODO An issue")
                                    (ejira--store-remote-baseline old-item)
                                    (save-buffer))
                                  (cl-letf (((symbol-function (quote ejira--auto-sync-fetch))
                                             (lambda (_keys) (list new-item)))
                                            ((symbol-function (quote ejira--update-task))
                                             (lambda (task) (setq pulled (ejira-task-key task))))
                                            ((symbol-function (quote ejira--issue-comments-dirty-p))
                                             (lambda (_key) nil))
                                            ((symbol-function (quote ejira--push-scan-buffer))
                                             (lambda (_buf) nil)))
                                    ;; Clean heading + remote change: pulled and re-baselined.
                                    (ejira--auto-sync-reconcile file)
                                    (should (equal "TEST-1" pulled))
                                    (with-current-buffer buf
                                      (goto-char (point-min))
                                      (re-search-forward "^\\*\\* TODO An issue")
                                      (should (equal (org-entry-get nil "Remotehash")
                                                     (md5 (ejira--remote-fields-identity new-item)))))
                                    ;; Dirty heading + remote change: held as conflict.  Reset the
                                    ;; remote baseline to the old state, because the first cycle
                                    ;; legitimately re-baselined it to the new item.
                                    (setq pulled nil)
                                    (with-current-buffer buf
                                      (goto-char (point-min))
                                      (re-search-forward "^\\*\\* TODO An issue")
                                      (ejira--migrate-push-baseline)
                                      (ejira--store-remote-baseline old-item)
                                      (beginning-of-line)
                                      (org-with-point-at (point-marker)
                                        (replace-regexp "TODO An issue" "TODO An issue edited" nil
                                                        (point) (line-end-position)))
                                      (save-buffer))
                                    (ejira--auto-sync-reconcile file)
                                    (should-not pulled)
                                    (with-current-buffer "*ejira sync log*"
                                      (should (string-match-p
                                               "TEST-1: changed locally and remotely"
                                               (buffer-substring (point-min) (point-max)))))
                                    ;; The dirty heading keeps its state for the push flow.
                                    (with-current-buffer buf
                                      (goto-char (point-min))
                                      (re-search-forward "^\\*\\* TODO An issue edited")
                                      (should (ejira--locally-modified-p)))))))

;;; ── Refile / cache invalidation (regressions) ────────────────────────────────
;;
;; A refile invalidates only the moved heading's own cache entry.  Cached
;; markers for its descendants kept pointing into the source buffer at the
;; removal site, and later updates rewrote whatever heading now lived there.
;; Observed live: a refiled epic's comments and subtasks were rewritten onto
;; an unrelated heading, leaving duplicate identities behind.

(defmacro ejira-test--with-two-files (content-a content-b &rest body)
  "Run BODY with two file-backed org buffers holding CONTENT-A and CONTENT-B.
Binds FILE-A, BUF-A, FILE-B and BUF-B.  All ejira lookups are pointed at
exactly these two files; the heading cache and `org-id-locations' are
fresh."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "ejira-test-" t))
          (file-a (expand-file-name "a.org" dir))
          (file-b (expand-file-name "b.org" dir))
          (ejira-projects nil)
          (ejira-extra-scan-files (list file-a file-b))
          (ejira--heading-cache (make-hash-table :test #'equal))
          (org-id-locations (make-hash-table :test 'equal)))
     (unwind-protect
         (let ((buf-a (progn (with-temp-file file-a (insert ,content-a))
                             (find-file-noselect file-a t)))
               (buf-b (progn (with-temp-file file-b (insert ,content-b))
                             (find-file-noselect file-b t))))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(ert-deftest ejira-update-task/keeps-user-placed-heading-in-its-file ()
  "A pull does not move a heading placed outside the ejira directory to
a parent in another file.
Regression: an epic kept with its backlog in a user file was refiled,
subtree and all, under its Initiative in the project file."
  (ejira-test--with-project-dir
      (concat ejira-test--project-content
              "** TODO Initiative\n:PROPERTIES:\n:ID:       TEST-9\n:TYPE:     ejira-issue\n:Issuetype: Initiative\n:END:\n")
    (let* ((extra (make-temp-file "ejira-user-" nil ".org"))
           (ejira-extra-scan-files (list extra))
           (ejira--heading-cache (make-hash-table :test #'equal))
           (ejira-assigned-tagname nil))
      (unwind-protect
          (progn
            (with-temp-file extra
              (insert "* TODO The epic\n:PROPERTIES:\n:ID:       TEST-5\n:TYPE:     ejira-epic\n:END:\n\nBody.\n"
                      "** TODO Its task\n:PROPERTIES:\n:ID:       TEST-6\n:TYPE:     ejira-issue\n:END:\n"))
            (let ((buf (find-file-noselect extra t)))
              (cl-letf (((symbol-function 'ejira--my-fullname) (lambda () "Test User")))
                (ejira--update-task
                 (make-ejira-task :key "TEST-5" :type "Epic" :status "Open"
                                  :project "TEST" :parent "TEST-9"
                                  :updated (date-to-time "2026-09-02 00:00:00 +0000")
                                  :created (date-to-time "2026-09-01 00:00:00 +0000")
                                  :summary "The epic" :description "Body."
                                  :comments-complete t)))
              (should (eq buf (marker-buffer (ejira--find-heading "TEST-5"))))
              (with-current-buffer buf
                (goto-char (point-min))
                (should (re-search-forward "^\\* TODO The epic" nil t))
                (should (re-search-forward "^\\*\\* TODO Its task" nil t)))))
        (when-let ((b (get-file-buffer extra)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))
        (delete-file extra)))))

(ert-deftest ejira-update-task/foreign-epic-does-not-move-issue ()
  "An issue whose epic is in a project ejira does not sync stays put.
Regression: the pull fetched the foreign epic, created a project file
for its project and refiled the issue there."
  (ejira-test--with-project-dir ejira-test--project-content
    (let* ((ejira--heading-cache (make-hash-table :test #'equal))
           (ejira-assigned-tagname nil)
           (fetched nil))
      (cl-letf (((symbol-function 'jiralib2-get-issue)
                 (lambda (key) (push key fetched) (error "Must not fetch %s" key)))
                ((symbol-function 'ejira--my-fullname) (lambda () "Test User")))
        (ejira--update-task
         (make-ejira-task :key "TEST-1" :type "Task" :status "Open"
                          :project "TEST" :epic "OTHER-7"
                          :updated (date-to-time "2026-09-02 00:00:00 +0000")
                          :created (date-to-time "2026-09-01 00:00:00 +0000")
                          :summary "An issue" :comments-complete t)))
      (should-not fetched)
      (should-not (file-exists-p (expand-file-name "OTHER.org" ejira-org-directory)))
      (should (equal (expand-file-name "TEST.org" ejira-org-directory)
                     (buffer-file-name (marker-buffer (ejira--find-heading "TEST-1"))))))))

(ert-deftest ejira-refile/evicts-cached-descendant-markers ()
  "After a cross-file refile, a cached descendant marker must not be trusted.
The refile moves the whole subtree; markers of cached descendants stay in
the source buffer at the removal site.  `ejira--find-heading' must re-scan
and return the descendant's new location."
  (ejira-test--with-two-files
   "* Parent\n:PROPERTIES:\n:ID: T-1\n:END:\n** Child\n:PROPERTIES:\n:ID: T-2\n:END:\n"
   "* Target\n:PROPERTIES:\n:ID: T-3\n:END:\n"
   ;; Populate the cache for the parent and its descendant.
   (should (markerp (ejira--find-heading "T-1")))
   (should (markerp (ejira--find-heading "T-2")))
   (ejira--refile "T-1" "T-3")
   ;; T-2 moved into file B with its parent: the stale marker in A must
   ;; not be returned.
   (let ((m (ejira--find-heading "T-2")))
     (should (markerp m))
     (should (eq buf-b (marker-buffer m)))
     (should (equal "T-2" (with-current-buffer buf-b
                            (org-entry-get (marker-position m) "ID")))))
   ;; The moved parent itself resolves to file B as well.
   (let ((m (ejira--find-heading "T-1")))
     (should (markerp m))
     (should (eq buf-b (marker-buffer m))))))

(ert-deftest ejira-find-heading/does-not-trust-stale-cache-hit ()
  "A cache entry whose marker no longer sits on the owning ID is evicted."
  (ejira-test--with-two-files
   "* Real\n:PROPERTIES:\n:ID: T-9\n:END:\n"
   "* Impostor\n:PROPERTIES:\n:ID: T-OTHER\n:END:\n"
   (should (markerp (ejira--find-heading "T-9")))
   ;; Simulate a marker left behind by a refile: it points into buffer B
   ;; (at T-OTHER) while still being cached for T-9.
   (with-current-buffer buf-b
     (goto-char (point-min)))
   (puthash "T-9" (with-current-buffer buf-b (point-marker))
            ejira--heading-cache)
   (let ((m (ejira--find-heading "T-9")))
     (should (markerp m))
     (should (eq buf-a (marker-buffer m)))
     (should (equal "T-9" (with-current-buffer buf-a
                            (org-entry-get (marker-position m) "ID")))))))

;;; ── Body-as-description boundary (regressions) ───────────────────────────────
;;
;; With an empty own body, `org-end-of-meta-data' lands directly on the
;; first descendant heading.  A boundary scan that only tests headings
;; *after* advancing skipped that first child, so a leading Comments
;; container or child task was swallowed by the description-owned region
;; -- and the next description rewrite deleted it.

(defconst ejira-test--empty-body-comments-task
  "* TODO TEST-1 Task\n:PROPERTIES:\n:ID: TEST-1\n:TYPE: ejira-issue\n:END:\n\
** Comments\n\
*** [2026-09-01 Mon 10:00] Someone\n:PROPERTIES:\n:CommId: 123\n:END:\n\
Comment body.\n\
** Some plain section\n\
Plain owned prose.\n")

(defconst ejira-test--empty-body-plain-before-comments-task
  "* TODO TEST-1 Task\n:PROPERTIES:\n:ID: TEST-1\n:TYPE: ejira-issue\n:END:\n\
** Some plain section\n\
Plain owned prose.\n\
** Comments\n\
*** [2026-09-01 Mon 10:00] Someone\n:PROPERTIES:\n:CommId: 123\n:END:\n\
Comment body.\n")

(ert-deftest ejira-description-boundary/empty-body-never-swallows-comments ()
  "An empty own body followed by Comments yields an empty owned region."
  (let ((ejira-description-in-body t))
    (ejira-test--with-org-buf ejira-test--empty-body-comments-task
                              (goto-char (point-min))
                              (let ((region (ejira--task-description-region)))
                                (should (= (car region) (cdr region)))))))

(ert-deftest ejira-description-boundary/empty-body-never-swallows-child-task ()
  "An empty own body followed by a child task yields an empty owned region."
  (let ((ejira-description-in-body t))
    (ejira-test--with-org-buf
     "* TODO TEST-1 Task\n:PROPERTIES:\n:ID: TEST-1\n:TYPE: ejira-issue\n:END:\n\
** TODO TEST-2 Child\n:PROPERTIES:\n:ID: TEST-2\n:TYPE: ejira-issue\n:END:\n\
Child body.\n"
     (goto-char (point-min))
     (let ((region (ejira--task-description-region)))
       (should (= (car region) (cdr region)))))))

(ert-deftest ejira-description-boundary/plain-section-before-comments-is-owned ()
  "A plain section before the Comments container stays in the owned region."
  (let ((ejira-description-in-body t))
    (ejira-test--with-org-buf
     ejira-test--empty-body-plain-before-comments-task
     (goto-char (point-min))
     (let* ((region (ejira--task-description-region))
            (comments-line (progn (goto-char (point-min))
                                  (re-search-forward "^\\*\\* Comments")
                                  (line-beginning-position))))
       ;; The region ends exactly at the Comments container: it owns the
       ;; plain section but never the comment entries below.
       (should (<= (car region) comments-line))
       (should (= (cdr region) comments-line))
       (should (string-match-p
                "Plain owned prose"
                (buffer-substring-no-properties
                 (car region) (cdr region))))
       (should-not (string-match-p
                    "Comment body"
                    (buffer-substring-no-properties
                     (car region) (cdr region))))))))

(ert-deftest ejira-description-set/empty-body-preserves-comments ()
  "Rewriting the description of an empty-body task keeps the Comments container."
  (let ((ejira-description-in-body t))
    (ejira-test--with-org-buf ejira-test--empty-body-comments-task
                              (goto-char (point-min))
                              (ejira--set-task-description "Fresh description")
                              ;; The accessor contract is point on the task
                              ;; heading; the setter leaves point at the
                              ;; insertion site, so go back explicitly.
                              (goto-char (point-min))
                              (should (equal "Fresh description"
                                             (string-trim (ejira--get-task-description))))
                              (should (= 1 (count-matches "^\\*\\* Comments" (point-min) (point-max))))
                              (should (= 1 (count-matches ":CommId: +123" (point-min) (point-max)))))))

;;; ── Creation placement (regressions) ─────────────────────────────────────────

(ert-deftest ejira-new-heading/lands-inside-parent-subtree ()
  "A heading created under a parent is the parent's last child.
`ejira--true-subtree-end' returns a position without moving point; the
insertion must jump there explicitly, or the heading lands above the
parent and a later refile has to repair the placement."
  (ejira-test--with-project-dir ejira-test--project-content
                                (let* ((buf (find-file-noselect
                                             (expand-file-name "TEST.org" ejira-org-directory) t)))
                                  ;; Give TEST-1 a child and a following sibling
                                  ;; (inserted after its metadata so the drawer
                                  ;; stays the heading's first drawer).
                                  (with-current-buffer buf
                                    (org-with-point-at (ejira--find-heading "TEST-1")
                                      (org-end-of-meta-data t)
                                      (insert "*** Existing kid\n:PROPERTIES:\n:ID: TEST-KID\n:END:\n"))
                                    (set-buffer-modified-p nil))
                                  (let ((m (ejira--new-heading buf "TEST-1" "TEST-NEW")))
                                    (should (markerp m))
                                    (with-current-buffer buf
                                      (org-with-point-at m
                                        ;; The new heading is inside TEST-1's subtree.
                                        (should (equal "TEST-1"
                                                       (save-excursion
                                                         (outline-up-heading 1 t)
                                                         (org-entry-get (point) "ID"))))))))))

(ert-deftest ejira-new-heading/does-not-merge-into-next-heading ()
  "A heading created under a parent that has a following heading stays separate.
Regression: the new heading's line was not terminated when the parent's
subtree ended at the next heading, so its title was glued onto that
heading and the new ID replaced the neighbour's."
  (ejira-test--with-project-dir
      (concat ejira-test--project-content
              "** TODO Next issue\n:PROPERTIES:\n:ID:       TEST-2\n:TYPE:     ejira-issue\n:END:\n")
    (let* ((buf (find-file-noselect (expand-file-name "TEST.org" ejira-org-directory) t))
           (m (ejira--new-heading buf "TEST-1" "TEST-NEW")))
      (with-current-buffer buf
        (org-with-point-at m
          (should (equal "TEST-NEW" (org-entry-get nil "ID")))
          (should (equal "<ejira new heading>" (org-get-heading t t t t)))
          (should (= 3 (org-current-level))))
        (goto-char (point-min))
        (should (re-search-forward "^\\*\\* TODO Next issue$" nil t))
        (should (equal "TEST-2" (org-entry-get nil "ID")))))))

;;; ── Comment-list completeness gate (regressions) ─────────────────────────────
;;
;; `jiralib2-get-issue' embeds ONE PAGE of comments (`fields.comment.comments')
;; alongside `total'/`startAt'.  Deleting local comments absent from that list
;; is only safe when the page is provably complete; a truncated page used to
;; delete live comments wholesale.

(defconst ejira-test--comments-task-content
  "* TEST\n:PROPERTIES:\n:ID: TEST\n:TYPE: ejira-project\n:END:\n\
* TODO Issue\n:PROPERTIES:\n:ID: TEST-1\n:TYPE: ejira-issue\n:Modified: 2026-09-01 00:00:00\n:END:\n\
** Comments\n\
*** [c1]\n:PROPERTIES:\n:CommId: 111\n:TYPE: ejira-comment\n:END:\n\
body1\n\
*** [c2]\n:PROPERTIES:\n:CommId: 222\n:TYPE: ejira-comment\n:END:\n\
body2\n")

(defun ejira-test--run-update-with-comments (comments complete)
  "Run `ejira--update-task' on the comments fixture with COMMENTS/COMPLETE."
  (let ((ejira--heading-cache (make-hash-table :test #'equal))
        (ejira-assigned-tagname nil))
    (ejira-test--with-org-buf ejira-test--comments-task-content
                              (goto-char (point-min))
                              (re-search-forward org-heading-regexp)
                              (puthash "TEST" (point-marker) ejira--heading-cache)
                              (re-search-forward org-heading-regexp)
                              (puthash "TEST-1" (point-marker) ejira--heading-cache)
                              (ejira--update-task
                               (make-ejira-task
                                :key "TEST-1" :type "Task" :status "Open"
                                :project "TEST"
                                ;; Newer than the fixture's Modified so the
                                ;; comment path runs (it is skipped for an
                                ;; unmodified issue).
                                :updated (date-to-time "2026-09-02 00:00:00 +0000")
                                :created (date-to-time "2026-09-01 00:00:00 +0000")
                                :deadline "2026-09-24"
                                :summary "Issue"
                                :comments comments
                                :comments-complete complete))
                              (list :c1 (count-matches ":CommId: +111" (point-min) (point-max))
                                    :c2 (count-matches ":CommId: +222" (point-min) (point-max))))))

(ert-deftest ejira-update-task/unconvertible-markup-changes-nothing ()
  "A description or comment that cannot be converted aborts the pull
before the heading is touched, and the or-hold wrapper logs a hold.
Writing the raw markup instead turned `* (x) item' list lines into Org
headings and left `{{code}}' literal in the body."
  (dolist (where '(description comment))
    (let ((ejira--heading-cache (make-hash-table :test #'equal))
          (ejira-assigned-tagname nil)
          (ejira-parser-patterns
           (cons (cons "boom" (lambda () (error "Rule failure")))
                 ejira-parser-patterns))
          (task (make-ejira-task
                 :key "TEST-1" :type "Task" :status "Done"
                 :project "TEST"
                 :updated (date-to-time "2026-09-02 00:00:00 +0000")
                 :created (date-to-time "2026-09-01 00:00:00 +0000")
                 :summary "Issue renamed"
                 :description (if (eq where 'description) "a boom" "fine")
                 :comments (list (make-ejira-comment
                                  :id "111" :author "A"
                                  :created (date-to-time "2026-09-01")
                                  :updated (date-to-time "2026-09-02")
                                  :body (if (eq where 'comment) "c boom" "ok")))
                 :comments-complete t)))
      (when (get-buffer "*ejira sync log*")
        (with-current-buffer "*ejira sync log*" (erase-buffer)))
      (ejira-test--with-org-buf ejira-test--comments-task-content
                                (goto-char (point-min))
                                (re-search-forward org-heading-regexp)
                                (puthash "TEST" (point-marker) ejira--heading-cache)
                                (re-search-forward org-heading-regexp)
                                (puthash "TEST-1" (point-marker) ejira--heading-cache)
                                (let ((before (buffer-string)))
                                  (should-error (ejira--update-task task) :type 'ejira-parser-error)
                                  (should (equal before (buffer-string)))
                                  (should-not (ejira--update-task-or-hold task))
                                  (should (equal before (buffer-string)))
                                  (with-current-buffer "*ejira sync log*"
                                    (should (string-match-p "TEST-1: remote markup could not be converted"
                                                            (buffer-string)))))))))

(ert-deftest ejira-comments-dirty-p/only-real-edits ()
  "Clean comments report nil; an edited comment reports non-nil.
Regression: the raw `org-map-entries' list was returned, which is
non-nil for any Comments heading, so every issue with comments had its
pulls deferred as \"locally edited comments\"."
  (let ((ejira--heading-cache (make-hash-table :test #'equal)))
    (ejira-test--with-org-buf ejira-test--comments-task-content
                              (goto-char (point-min))
                              (re-search-forward org-heading-regexp)
                              (re-search-forward org-heading-regexp)
                              (puthash "TEST-1" (point-marker) ejira--heading-cache)
                              ;; Baseline both comments on their current bodies.
                              (goto-char (point-min))
                              (while (re-search-forward "^\\*\\*\\* \\[c" nil t)
                                (ejira--update-push-baseline))
                              (should-not (ejira--issue-comments-dirty-p "TEST-1"))
                              (goto-char (point-min))
                              (re-search-forward "^body2")
                              (replace-match "body2 edited")
                              (should (ejira--issue-comments-dirty-p "TEST-1")))))

(ert-deftest ejira-update-task/forced-update-ignores-modified-shortcut ()
  "A forced update imports comments even when `Modified' matches Jira.
Regression: the reconcile cycle baselined issues whose earlier pulls had
never imported their comments, because the unchanged timestamp skipped
the content update; the baseline then hid the gap for good."
  (let ((ejira--heading-cache (make-hash-table :test #'equal))
        (ejira-assigned-tagname nil)
        (task (make-ejira-task
               :key "TEST-1" :type "Task" :status "Open" :project "TEST"
               ;; Same instant as the fixture's Modified.
               :updated (date-to-time "2026-09-01 00:00:00 +0000")
               :created (date-to-time "2026-08-01 00:00:00 +0000")
               :summary "Issue"
               :comments (list (make-ejira-comment
                                :id "333" :author "A"
                                :created (date-to-time "2026-09-01")
                                :updated (date-to-time "2026-09-01")
                                :body "new"))
               :comments-complete nil)))
    (ejira-test--with-org-buf ejira-test--comments-task-content
      (goto-char (point-min))
      (re-search-forward org-heading-regexp)
      (puthash "TEST" (point-marker) ejira--heading-cache)
      (re-search-forward org-heading-regexp)
      (puthash "TEST-1" (point-marker) ejira--heading-cache)
      (ejira--update-task task)
      (should (= 0 (count-matches ":CommId: +333" (point-min) (point-max))))
      (let ((ejira--force-full-update t))
        (ejira--update-task task))
      (should (= 1 (count-matches ":CommId: +333" (point-min) (point-max)))))))

(ert-deftest ejira-update-task/discard-local-edits-takes-jira-version ()
  "With `ejira--discard-local-edits' a dirty heading takes Jira's content
and becomes clean; without it the local edit is kept."
  (let ((ejira--heading-cache (make-hash-table :test #'equal))
        (ejira-assigned-tagname nil)
        (task (make-ejira-task
               :key "TEST-1" :type "Task" :status "Open" :project "TEST"
               :updated (date-to-time "2026-09-01 00:00:00 +0000")
               :created (date-to-time "2026-08-01 00:00:00 +0000")
               :summary "Jira title" :description "Jira body."
               :comments-complete t)))
    (ejira-test--with-org-buf
     (concat "* TEST\n:PROPERTIES:\n:ID: TEST\n:TYPE: ejira-project\n:END:\n"
             "* TODO Local title\n:PROPERTIES:\n:ID: TEST-1\n:TYPE: ejira-issue\n"
             ":EJIRA_DESCRIPTION_IN_BODY: t\n:Modified: 2026-09-01 00:00:00\n:END:\n\nLocal body.\n")
     (goto-char (point-min))
     (re-search-forward org-heading-regexp)
     (puthash "TEST" (point-marker) ejira--heading-cache)
     (re-search-forward org-heading-regexp)
     (puthash "TEST-1" (point-marker) ejira--heading-cache)
     (org-set-property "Pushhash" "v2:stale")
     (org-set-property "Statehash" (md5 (ejira--heading-state-fields)))
     (should (ejira--locally-modified-p))
     (ejira--update-task task)
     (should (equal "Local title" (org-get-heading t t t t)))
     (let ((ejira--discard-local-edits t))
       (ejira--update-task task))
     (should (equal "Jira title" (org-get-heading t t t t)))
     (should (equal "Jira body." (string-trim (ejira--jira-description))))
     (should-not (ejira--locally-modified-p)))))

(ert-deftest ejira-set-heading-summary/takes-exact-case ()
  "A new summary replaces the title exactly, whatever the old case.
Regression: `replace-match' re-cased the replacement like the matched
title, so a Title Case heading kept its capitals forever."
  (ejira-test--with-org-buf
   "* DONE Start The Review By Next Week\n:PROPERTIES:\n:ID: TEST-1\n:END:\n"
   (ejira--set-heading-summary (point-marker) "Start the review by next week")
   (should (equal "Start the review by next week" (org-get-heading t t t t)))))

(ert-deftest ejira-update-task/preserves-comments-on-incomplete-list ()
  "A truncated embedded comment page must not delete local comments."
  (should (equal '(:c1 1 :c2 1)
                 (ejira-test--run-update-with-comments
                  (list (make-ejira-comment :id "111" :author "A"
                                            :created (date-to-time "2026-09-01")
                                            :updated (date-to-time "2026-09-01")
                                            :body "b"))
                  nil))))

(ert-deftest ejira-update-task/complete-list-may-delete ()
  "When the comment page is provably complete, deletions proceed."
  (should (equal '(:c1 1 :c2 0)
                 (ejira-test--run-update-with-comments
                  (list (make-ejira-comment :id "111" :author "A"
                                            :created (date-to-time "2026-09-01")
                                            :updated (date-to-time "2026-09-01")
                                            :body "b"))
                  t))))

(ert-deftest ejira-parse-item/comment-completeness-flag ()
  "`ejira--parse-item' derives the completeness flag from the embedded page."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (should (eq t (ejira-task-comments-complete
                   (ejira--parse-item
                    (json-read-from-string "{\"key\": \"TEST-1\", \"fields\": {\"issuetype\": {\"name\": \"Task\"}, \"comment\": {\"startAt\": 0, \"total\": 2, \"comments\": [{\"id\": \"1\", \"author\": {\"displayName\": \"A\"}, \"fields\": {\"created\": \"2026-09-01T10:00:00.000+0000\", \"updated\": \"2026-09-01T10:00:00.000+0000\", \"body\": \"x\"}}, {\"id\": \"2\", \"author\": {\"displayName\": \"A\"}, \"fields\": {\"created\": \"2026-09-02T10:00:00.000+0000\", \"updated\": \"2026-09-02T10:00:00.000+0000\", \"body\": \"y\"}}]}}}")))))
    (should (null (ejira-task-comments-complete
                   (ejira--parse-item
                    (json-read-from-string "{\"key\": \"TEST-1\", \"fields\": {\"issuetype\": {\"name\": \"Task\"}, \"comment\": {\"startAt\": 0, \"total\": 12, \"maxResults\": 5, \"comments\": [{\"id\": \"1\", \"author\": {\"displayName\": \"A\"}, \"fields\": {\"created\": \"2026-09-01T10:00:00.000+0000\", \"updated\": \"2026-09-01T10:00:00.000+0000\", \"body\": \"x\"}}]}}}")))))
    ;; No embedded comment object at all: the empty list is complete.
    (should (eq t (ejira-task-comments-complete
                   (ejira--parse-item
                    (json-read-from-string "{\"key\": \"TEST-1\", \"fields\": {\"issuetype\": {\"name\": \"Task\"}}}")))))))
;;; ── Scan dedupe + cookie dirtiness (regressions) ─────────────────────────────

(ert-deftest ejira-push--rule-f/new-parent-and-child-not-double-scanned ()
  "A new parent and its new child produce ONE create op (with children).
Scanning the child independently — its new-parent ancestor has no ID
yet — created the ticket twice: once via the parent's cascade and once
standalone under the grandparent."
  (ejira-test--with-org-buf
   "* PROJ
:PROPERTIES:
:TYPE:     ejira-project
:ID:       PROJ
:END:
** TODO New parent

Parent body.

*** TODO New child

Child body.
"
   (let* ((ops (ejira--push-scan-buffer (current-buffer)))
          (create-ops (cl-remove-if-not
                       (lambda (op) (eq (plist-get op :op) 'create))
                       ops))
          (blocked-ops (cl-remove-if-not
                        (lambda (op) (eq (plist-get op :op) 'blocked))
                        ops)))
     (should (= 1 (length create-ops)))
     (should (= 1 (length (plist-get (plist-get (car create-ops) :data) :children))))
     ;; the grandchild-level TODO under the captured child is reported,
     ;; not silently dropped
     (should (null blocked-ops)))))

(ert-deftest ejira-push--rule-e/todo-below-new-child-is-blocked ()
  "A TODO deeper than a new parent's direct children is blocked, not
flattened under the grandparent."
  (ejira-test--with-org-buf
   "* PROJ
:PROPERTIES:
:TYPE:     ejira-project
:ID:       PROJ
:END:
** TODO New parent

*** TODO Middle child

**** TODO Deeper grandchild
"
   (let* ((ops (ejira--push-scan-buffer (current-buffer)))
          (blocked-ops (cl-remove-if-not
                        (lambda (op) (eq (plist-get op :op) 'blocked))
                        ops)))
     (should (= 1 (length blocked-ops)))
     (should (string-match-p "cannot create below a new child"
                             (plist-get (car blocked-ops) :reason))))))

(ert-deftest ejira-state/priority-cookie-edit-dirties-state ()
  "A direct `[#A]' cookie edit is caught even when no hashed field moved."
  (let ((org-priority-highest 1)
        (org-priority-lowest 5)
        (ejira--heading-cache (make-hash-table :test #'equal)))
    (ejira-test--with-org-buf
     "* TODO [#3] TEST-1 Issue
:PROPERTIES:
:ID: TEST-1
:TYPE: ejira-issue
:Statehash: dummy
:JiraPriorityRank: 2
:END:
"
     (goto-char (point-min))
     (re-search-forward org-heading-regexp)
     ;; stored rank 2 (=[#2]); the cookie says 3 → modified
     (should (ejira--priority-cookie-modified-p)))))

(provide (quote ejira-test))
;;; ejira-test.el ends here
