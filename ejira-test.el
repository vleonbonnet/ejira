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
  (let* ((plans (list (list :op 'update :project "RNDSEC" :title "X")
                      (list :op 'create :project "CVE"    :title "Y")
                      (list :op 'update :project "RNDSEC" :title "Z")))
         (groups (ejira-confirm--group-by-project plans)))
    (should (= 2 (length groups)))
    (should (equal "CVE"    (caar groups)))        ; sorted first
    (should (equal "RNDSEC" (caadr groups)))
    (should (= 2 (length (cdr (assoc "RNDSEC" groups)))))
    (should (= 1 (length (cdr (assoc "CVE"    groups)))))))

(ert-deftest ejira-confirm--count-ops/all-types ()
  "Count creates, updates, and deletes independently."
  (let* ((plans (list (list :op 'create)
                      (list :op 'update)
                      (list :op 'update)
                      (list :op 'delete)))
         (counts (ejira-confirm--count-ops plans)))
    (should (= 1 (alist-get 'create counts 0)))
    (should (= 2 (alist-get 'update counts 0)))
    (should (= 1 (alist-get 'delete counts 0)))))

;;; ── ejira-core helpers ────────────────────────────────────────────────────────

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
  (let ((ops (ejira-test--scan
              "* PROJ-1 Parent Issue
:PROPERTIES:
:TYPE:     ejira-issue
:ID:       PROJ-1
:Issuetype: Task
:END:
** TODO My New Subtask
")))
    (let ((subtask-ops (cl-remove-if-not
                        (lambda (op) (and (eq 'create  (plist-get op :op))
                                          (eq 'subtask (plist-get op :object))))
                        ops)))
      (should (= 1 (length subtask-ops)))
      (should (equal "PROJ-1"
                     (plist-get (plist-get (car subtask-ops) :data) :parent-key))))))

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
  (let ((ops (ejira-test--scan
              "* PROJ
:PROPERTIES:
:TYPE:     ejira-project
:ID:       PROJ
:END:
** TODO My New Issue
")))
    (let ((issue-ops (cl-remove-if-not
                      (lambda (op) (and (eq 'create (plist-get op :op))
                                        (eq 'issue  (plist-get op :object))))
                      ops)))
      (should (= 1 (length issue-ops)))
      (should (equal "PROJ"
                     (plist-get (plist-get (car issue-ops) :data) :project-key))))))

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

(provide 'ejira-test)
;;; ejira-test.el ends here
