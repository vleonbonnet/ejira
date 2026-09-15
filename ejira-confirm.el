;;; ejira-confirm.el --- Push review via org-sync-confirm -*- lexical-binding: t -*-

;; This file is part of ejira.

;;; Commentary:

;; Turns the push plans built by ejira-push.el into an
;; `org-sync-confirm' tree (project > issue > plan > field) and runs
;; the confirmed plans' :send thunks.  Every plan is an item the user
;; can untick; an issue with an update plan is that plan's item, with
;; the issue's other plans (new subtasks, comments, transitions, ...)
;; as its children.
;;
;;   Ejira Push — 3 changes: 1 modified, 2 new
;;   ══════════════════════════════════════════
;;   [x] Assign new issues to me
;;
;;   ▼ TEST  1 modified, 2 new
;;     ▼ [x] ✎ Fix the login page  summary, description, 1 new
;;         summary:      old title → new title
;;         ▶ description:  -1 +2 lines, +80 chars
;;         ▶ payload:      6 lines, sent as is
;;         ▶ [x] + subtask: TODO Write the tests  new
;;
;; C-c C-c pushes the ticked plans, C-c C-k aborts.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-sync-confirm)

(defvar ejira--pushing nil
  "Bound to t while ejira-push is executing a batch; inhibits re-scan on save.
Defined in ejira-push.el; declared here so the executor can bind it.")

(declare-function ejira--find-heading "ejira-core" (id))

;;; String helpers

(defun ejira-confirm--normalize (s)
  "Strip CR and trim S; nil becomes \"\"."
  (string-trim (replace-regexp-in-string "\r" "" (or s ""))))

(defun ejira-confirm-field-changes (fields)
  "Filter FIELDS to entries whose old and new values differ.
FIELDS is a list of (NAME OLD NEW) string triples."
  (cl-remove-if (lambda (f)
                  (equal (ejira-confirm--normalize (nth 1 f))
                         (ejira-confirm--normalize (nth 2 f))))
                fields))

(defun ejira-confirm--present (s)
  "Return S normalized, or nil when it is empty."
  (let ((clean (ejira-confirm--normalize s)))
    (unless (string-empty-p clean) clean)))

;;; Grouping

(defun ejira-confirm--group-by-project-issue (plans)
  "Return alist (project . alist(issue-or-nil . plans)) sorted by project."
  (let (groups)
    (dolist (plan plans)
      (let* ((proj  (or (plist-get plan :project) "?"))
             (issue (plist-get plan :parent-issue))
             (proj-cell (or (assoc proj groups)
                            (let ((c (cons proj nil)))
                              (push c groups) c)))
             (issue-cell (or (assoc issue (cdr proj-cell))
                             (let ((c (cons issue nil)))
                               (setcdr proj-cell
                                       (append (cdr proj-cell) (list c)))
                               c))))
        (setcdr issue-cell (append (cdr issue-cell) (list plan)))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

;;; Items

(defun ejira-confirm--issue-title (key)
  "Return the org heading title for Jira KEY, or nil if not found."
  (when (and (stringp key) (fboundp 'ejira--find-heading))
    (condition-case nil
        (when-let ((m (ejira--find-heading key)))
          (org-with-point-at m
            (org-no-properties (org-get-heading t t t t))))
      (error nil))))

(defun ejira-confirm--change-fields (changes)
  "Convert CHANGES, a list of (NAME OLD NEW), into review fields."
  (mapcar (lambda (c)
            (list :name (nth 0 c)
                  :old (ejira-confirm--present (nth 1 c))
                  :new (ejira-confirm--present (nth 2 c))))
          changes))

(defun ejira-confirm--value-fields (fields preview)
  "Convert FIELDS, a list of (NAME VALUE), into new-value review fields.
Without FIELDS, PREVIEW becomes a single body field."
  (or (delq nil
            (mapcar (lambda (f)
                      (when-let ((v (ejira-confirm--present (nth 1 f))))
                        (list :name (nth 0 f) :new v)))
                    fields))
      (when-let ((body (ejira-confirm--present preview)))
        (list (list :name "body" :new body)))))

(defun ejira-confirm--create-label (plan)
  "Return the header label of create PLAN: \"subtask: TODO Title\"."
  (let* ((object (plist-get plan :object))
         (label (or (plist-get plan :label)
                    (pcase object ('subtask "subtask") ('comment "comment") (_ "issue"))))
         (fields (plist-get plan :fields))
         (state (nth 1 (cl-assoc "state" fields :test #'string=)))
         (title (or (nth 1 (cl-assoc "title" fields :test #'string=))
                    (replace-regexp-in-string "\\`new [a-z]+: \\|\\`new [a-z]+ on " ""
                                              (or (plist-get plan :title) "")))))
    (concat label ": "
            (if (ejira-confirm--present state) (concat state " ") "")
            (org-sync-confirm--truncate title 80))))

(defun ejira-confirm--child-item (child)
  "Return the review item for CHILD, a cascaded subtask of a new issue."
  (list :label (concat "subtask: "
                       (if (ejira-confirm--present (plist-get child :state))
                           (concat (plist-get child :state) " ")
                         "")
                       (or (plist-get child :title) ""))
        :kind 'new
        :fixed t
        :summary "created with the parent"
        :fields (ejira-confirm--value-fields
                 (list (list "title" (plist-get child :title))
                       (list "state" (plist-get child :state))
                       (list "description" (plist-get child :body)))
                 nil)))

(defun ejira-confirm--update-label (plan)
  "Return the header label of update PLAN."
  (pcase (plist-get plan :object)
    ('status "transition")
    ('issuetype "issue type")
    ('epic "epic link")
    ('comment (plist-get plan :title))
    (_ (plist-get plan :title))))

(defun ejira-confirm--plan-item (plan)
  "Return the review item for PLAN."
  (let ((base (list :payload (plist-get plan :payload)
                    :execute (plist-get plan :send)
                    :data plan)))
    (append
     (pcase (plist-get plan :op)
       ('create
        (list :label (ejira-confirm--create-label plan)
              :kind 'new
              :fields (ejira-confirm--value-fields (plist-get plan :fields)
                                                   (plist-get plan :preview))
              :children (mapcar #'ejira-confirm--child-item (plist-get plan :children))
              :toggles (when-let ((cell (plist-get plan :assign-self)))
                         (list (list :key 'assign-self :label "assign to me" :cell cell)))))
       ('delete
        (list :label (plist-get plan :title)
              :kind 'deleted
              :warning "permanent"
              :fields (when-let ((body (ejira-confirm--present (plist-get plan :preview))))
                        (list (list :name "body" :old body)))))
       (_
        (list :label (ejira-confirm--update-label plan)
              :kind 'modified
              :fields (ejira-confirm--change-fields (plist-get plan :changes)))))
     base)))

(defun ejira-confirm--issue-node (issue-key plans)
  "Return the review item for ISSUE-KEY grouping PLANS.
The issue's own update plan, when present, is the node itself."
  (let* ((update (cl-find-if (lambda (p) (and (eq (plist-get p :op) 'update)
                                              (eq (plist-get p :object) 'issue)))
                             plans))
         (others (mapcar #'ejira-confirm--plan-item (cl-remove update plans)))
         (title (ejira-confirm--issue-title issue-key))
         (label (if (ejira-confirm--present title) title issue-key)))
    (if update
        (let ((item (ejira-confirm--plan-item update)))
          (plist-put item :label label)
          (plist-put item :children others))
      (list :label label :children others))))

(defun ejira-confirm--items (plans)
  "Build the review tree for PLANS."
  (mapcar (lambda (group)
            (list :label (car group)
                  :children
                  (cl-mapcan (lambda (issue-group)
                               (if (car issue-group)
                                   (list (ejira-confirm--issue-node (car issue-group)
                                                                    (cdr issue-group)))
                                 (mapcar #'ejira-confirm--plan-item (cdr issue-group))))
                             (cdr group))))
          (ejira-confirm--group-by-project-issue plans)))

;;; Entry point

(defun ejira-confirm--execute (items)
  "Run the :send thunks of ITEMS with re-scan on save inhibited."
  (let ((ejira--pushing t))
    (org-sync-confirm-execute-items items 'ejira)))

(defun ejira-confirm-show (plans)
  "Display the review buffer for pending push PLANS."
  (org-sync-confirm-show
   (list :title "Ejira Push"
         :buffer "*ejira-confirm*"
         :confirm-label "Push"
         :warning-type 'ejira
         :toggles (list (list :key 'assign-self :label "Assign new issues to me"))
         :items (ejira-confirm--items plans)
         :execute #'ejira-confirm--execute
         :on-cancel (lambda () (message "ejira: push cancelled")))))

(provide 'ejira-confirm)
;;; ejira-confirm.el ends here
