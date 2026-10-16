;;; ejira-migrate-test.el --- Migration to the current schema -*- lexical-binding: t; -*-

;;; Commentary:

;; Each test writes an Org file in the old schema, migrates it, and
;; checks the text and the baselines the current code reads from it.

;;; Code:

(require 'ert)
(require 'ejira-migrate)

(defmacro ejira-migrate-test--with-file (content &rest body)
  "Run BODY with `file' holding CONTENT and `migrated' its migrated text.
`report' holds the migration report; `buf' an Org buffer of `migrated'."
  (declare (indent 1))
  `(let ((file (make-temp-file "ejira-migrate-" nil ".org" ,content)))
     (unwind-protect
         (let* ((result (ejira-migrate-file file))
                (migrated (car result))
                (report (cdr result)))
           (ignore report)
           (with-temp-buffer
             (insert migrated)
             (let ((org-mode-hook nil)) (org-mode))
             (goto-char (point-min))
             (let ((buf (current-buffer)))
               (ignore buf)
               ,@body)))
       (delete-file file))))

(defun ejira-migrate-test--old-hashes (content key)
  "Return (PUSHHASH . STATEHASH) of issue KEY in CONTENT, as v2 wrote them.
The description is read from the `Description' child: the old layout."
  (with-temp-buffer
    (insert content)
    (let ((org-mode-hook nil)) (org-mode))
    (goto-char (point-min))
    (re-search-forward (concat "^:ID:[ \t]+" key "$"))
    (org-back-to-heading t)
    (let ((desc (or (when-let* ((d (ejira--find-child-heading "Description")))
                      (ejira--get-heading-body d))
                    "")))
      (cons (concat "v2:" (md5 (ejira-migrate--content-fields desc)))
            (md5 (ejira--heading-state-fields))))))

(defun ejira-migrate-test--issue (key title drawer body)
  "An old-schema issue heading KEY with TITLE, extra DRAWER lines and BODY."
  (format "** TODO %s\n:PROPERTIES:\n:ID:       %s\n:TYPE:     ejira-issue\n%s:END:\n%s"
          title key drawer body))

(defun ejira-migrate-test--goto (key)
  "Move to the heading of KEY."
  (goto-char (point-min))
  (re-search-forward (concat "^:ID:[ \t]+" key "$"))
  (org-back-to-heading t))

(defconst ejira-migrate-test--project
  "* Project\n:PROPERTIES:\n:ID:       TEST\n:TYPE:     ejira-project\n:END:\n")

(defun ejira-migrate-test--clean (key title body)
  "Old-schema CONTENT holding clean issue KEY with TITLE and BODY.
Its `Pushhash' and `Statehash' match it, as a sync left them."
  (let* ((raw (concat ejira-migrate-test--project
                      (ejira-migrate-test--issue key title "" body)))
         (hashes (ejira-migrate-test--old-hashes raw key)))
    (concat ejira-migrate-test--project
            (ejira-migrate-test--issue
             key title
             (format ":Pushhash: %s\n:Statehash: %s\n:Remotehash: abc\n:Remoteupdated: 2026-09-21T10:00:00.000+0000\n"
                     (car hashes) (cdr hashes))
             body))))

(ert-deftest ejira-migrate/folds-description-and-stays-clean ()
  "A legacy description becomes the task body, its subheadings one star
up; the issue keeps no local change, and the remote baseline is renamed."
  (ejira-migrate-test--with-file
      (ejira-migrate-test--clean
       "TEST-1" "An issue"
       "*** Description\n:PROPERTIES:\n:ID:       D-1\n:END:\n\nIntro.\n\n***** Part\n\nDetail.\n\n*** Comments\n")
    (should (= 1 (plist-get report :folded)))
    (should (equal '(("D-1" . "TEST-1")) (plist-get report :description-ids)))
    (should (= 1 (plist-get report :clean)))
    (should-not (string-match-p "Description\\|Pushhash\\|Statehash\\|Remoteupdated" migrated))
    (ejira-migrate-test--goto "TEST-1")
    (should (equal "abc" (org-entry-get nil "RemoteHash")))
    (should (equal "Intro.\n\n**** Part\n\nDetail." (string-trim (ejira--jira-description))))
    (should (= 2 (length (ejira--local-baseline))))
    (should-not (ejira--locally-modified-p))
    (should (ejira--find-child-heading "Comments"))))

(ert-deftest ejira-migrate/keeps-pending-edits-pending ()
  "An issue edited locally before the migration is still edited after it."
  (let ((content (ejira-migrate-test--clean "TEST-1" "An issue"
                                            "*** Description\n\nRemote text.\n")))
    (ejira-migrate-test--with-file
        (replace-regexp-in-string "^\\*\\* TODO An issue$" "** TODO An issue, edited" content)
      (should (= 1 (plist-get report :dirty-before)))
      (should (= 1 (plist-get report :dirty)))
      (ejira-migrate-test--goto "TEST-1")
      (should (ejira--content-modified-p))
      (should-not (ejira--state-modified-p)))))

(ert-deftest ejira-migrate/drops-stale-copies-of-the-description ()
  "A section next to the description holding only a copy of it, wrapped
differently, is removed; the task is then folded."
  (ejira-migrate-test--with-file
      (ejira-migrate-test--clean
       "TEST-1" "An issue"
       (concat "*** Description\n\n***** How can we help?\n\nHello! My team has a question.\n\n"
               "*** How can we help?\nHello!\nMy team has a question.\n\n*** Comments\n"))
    (should (= 1 (plist-get report :dropped)))
    (should (= 1 (plist-get report :folded)))
    ;; Only the description's own section is left.
    (should (= 2 (length (split-string migrated "How can we help\\?"))))
    (ejira-migrate-test--goto "TEST-1")
    (should-not (ejira--locally-modified-p))))

(ert-deftest ejira-migrate/keeps-local-notes-local-as-a-projection ()
  "A task whose own prose or sections are local notes next to its
description becomes a projection: the description moves to its
`JIRA_DESCRIPTION' child and the notes stay out of Jira."
  (ejira-migrate-test--with-file
      (ejira-migrate-test--clean
       "TEST-1" "An issue"
       "\nMy local notes.\n\n*** Description\n\nRemote text.\n\n*** Impact\n\nLocal analysis.\n")
    (should (equal '("TEST-1") (plist-get report :projections)))
    (should (= 0 (plist-get report :folded)))
    (ejira-migrate-test--goto "TEST-1")
    (should (ejira--jira-projection-p))
    (should (equal "Remote text." (string-trim (ejira--jira-description))))
    (should (string-match-p "My local notes\\." (ejira--get-heading-own-body)))
    (should-not (ejira--locally-modified-p))))

(ert-deftest ejira-migrate/converts-single-hash-baselines ()
  "A clean issue with the single legacy hash gets both parts; one with
pending edits blocks the migration."
  (let* ((raw (concat ejira-migrate-test--project
                      (ejira-migrate-test--issue "TEST-1" "An issue" "" "\nBody.\n")))
         (legacy (with-temp-buffer
                   (insert raw)
                   (let ((org-mode-hook nil)) (org-mode))
                   (ejira-migrate-test--goto "TEST-1")
                   (md5 (ejira-migrate--single-hash-content "")))))
    ;; The old layout: no Description child, so an empty description.
    (ejira-migrate-test--with-file
        (concat ejira-migrate-test--project
                (ejira-migrate-test--issue "TEST-1" "An issue"
                                           (format ":Pushhash: %s\n" legacy) ""))
      (should-not (plist-get report :stuck))
      (ejira-migrate-test--goto "TEST-1")
      (should (= 2 (length (ejira--local-baseline))))
      (should-not (ejira--locally-modified-p)))
    (ejira-migrate-test--with-file
        (concat ejira-migrate-test--project
                (ejira-migrate-test--issue "TEST-1" "An issue" ":Pushhash: stale\n" ""))
      (should (equal '("TEST-1") (plist-get report :stuck))))))

(ert-deftest ejira-migrate/comments-keep-their-hash ()
  "A comment's `Pushhash' becomes its `LocalHash', unchanged."
  (ejira-migrate-test--with-file
      (concat (ejira-migrate-test--clean "TEST-1" "An issue" "*** Description\n\nText.\n")
              "*** Comments\n**** A comment\n:PROPERTIES:\n:TYPE:     ejira-comment\n:CommId:   7\n:Pushhash: abc123\n:END:\n\nBody.\n")
    (should (= 1 (plist-get report :comments)))
    (re-search-forward "^\\*\\*\\*\\* A comment")
    (should (equal "abc123" (org-entry-get nil "LocalHash")))))

(ert-deftest ejira-migrate/body-layout-changes-only-properties ()
  "A file already in the body layout only changes its property lines."
  (let* ((content (ejira-migrate-test--clean "TEST-1" "An issue" "\nThe body.\n"))
         (content (replace-regexp-in-string
                   ":TYPE:     ejira-issue\n"
                   ":TYPE:     ejira-issue\n:EJIRA_DESCRIPTION_IN_BODY: t\n" content)))
    ;; The opt-in changes the description, so the fixture hashes it again.
    (setq content
          (with-temp-buffer
            (insert content)
            (let ((org-mode-hook nil)) (org-mode))
            (ejira-migrate-test--goto "TEST-1")
            (org-set-property "Pushhash" (concat "v2:" (md5 (ejira-migrate--content-fields
                                                             (ejira--get-task-description)))))
            (buffer-string)))
    (ejira-migrate-test--with-file content
      (should (= 0 (plist-get report :folded)))
      (should (= 1 (plist-get report :clean)))
      (should (string-match-p "\n\nThe body\\.\n" migrated))
      (should-not (string-match-p "EJIRA_DESCRIPTION_IN_BODY" migrated)))))

(provide 'ejira-migrate-test)
;;; ejira-migrate-test.el ends here
