;;; ejira-migrate-test.el --- Pure migration regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'ejira-migrate)

(ert-deftest ejira-migrate-parse-keeps-siblings-and-skipped-levels ()
  (let* ((lines '("preamble" "* First" "body" "*** Nested" "nested body"
                  "* Second" "second body"))
         (parsed (ejira-migrate--parse lines 0))
         (blocks (car parsed)))
    (should-not (cdr parsed))
    (should (= 3 (length blocks)))
    (should (equal "First" (plist-get (nth 1 blocks) :title)))
    (should (equal "Second" (plist-get (nth 2 blocks) :title)))
    (should (equal "Nested"
                   (plist-get (car (plist-get (nth 1 blocks) :children))
                              :title)))
    (let* ((rendered (ejira-migrate--render-block
                      (list :level 0 :children blocks)))
           (again (car (ejira-migrate--parse rendered 0))))
      (should (= 3 (length again)))
      (dolist (text '("preamble" "body" "nested body" "second body"))
        (should (member text rendered))))))

(ert-deftest ejira-migrate-set-props-preserves-drawer-and-input ()
  (let* ((original '(":PROPERTIES:" ":ID: SEC-1" ":Pushhash: old" ":END:"))
         (result (ejira-migrate--set-props
                  original '(("Pushhash" . "new") ("Statehash" . "state")))))
    (should (equal original
                   '(":PROPERTIES:" ":ID: SEC-1" ":Pushhash: old" ":END:")))
    (should (equal "new" (cdr (assoc "Pushhash"
                                     (ejira-migrate--drawer-props result)))))
    (should (equal "state" (cdr (assoc "Statehash"
                                       (ejira-migrate--drawer-props result)))))
    (should (equal ":END:" (car (last result))))))

(ert-deftest ejira-migrate-fold-retains-body-and-unrelated-children ()
  (let* ((lines '("* TODO Task" ":PROPERTIES:" ":ID: SEC-1" ":END:"
                  "local prose" "** Description" "remote prose"
                  "** Notes" "keep me"))
         (block (car (car (ejira-migrate--parse lines 0))))
         (folded (ejira-migrate--fold block))
         (rendered (ejira-migrate--render-block (car folded))))
    (should (ejira-migrate--task-p block))
    (should (equal 'diff (plist-get (cdr folded) :action)))
    (dolist (text '("local prose" "remote prose" "** Notes" "keep me"))
      (should (member text rendered)))
    (should-not (member "** Description" rendered))))

(provide 'ejira-migrate-test)
