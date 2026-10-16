;;; ejira-migrate.el --- Migrate Org files to ejira's current schema -*- lexical-binding: t; -*-

;;; Commentary:

;; `ejira-migrate-files' rewrites Org files written by earlier ejira
;; versions into the current schema, in one pass per file:
;;
;; - Descriptions live in the task body.  A legacy `Description' child
;;   heading is folded into its task's body (one star less for its
;;   subheadings), and the `EJIRA_DESCRIPTION_IN_BODY' opt-in goes.
;; - The local baseline is one `LocalHash' property: an issue's content
;;   and state hashes, a comment's body hash.  It replaces `Pushhash'
;;   and `Statehash', and the legacy single-hash form of `Pushhash'.
;; - The remote baseline is `RemoteHash', dated by `Modified';
;;   `Remotehash' is renamed and `Remoteupdated' removed.
;;
;; Whatever was clean stays clean, and whatever had local edits keeps
;; them pending.  A legacy task whose own body or other sections were
;; local notes next to its `Description' child would send them as its
;; description in the body layout: it becomes a projection instead, its
;; `Description' child its `JIRA_DESCRIPTION' (see
;; `ejira-migrate--convert-at-point').  Sections holding only a stale
;; copy of the description are removed first.
;;
;; The fold is built as text; the baselines are read from the original
;; text with the formulas it was written with, and recorded in the new
;; text with the current code.  Nothing is written without APPLY.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ejira-push)

;;; Folding a legacy description

(defun ejira-migrate--heading-line-p (line)
  "Return non-nil when LINE is an Org heading."
  (string-match-p "^\\*+ " line))

(defun ejira-migrate--blank-p (line)
  "Return non-nil when LINE holds only whitespace."
  (string-empty-p (string-trim line)))

(defun ejira-migrate--trim (lines)
  "Drop the leading and trailing blank lines of LINES."
  (let ((lines (seq-drop-while #'ejira-migrate--blank-p lines)))
    (nreverse (seq-drop-while #'ejira-migrate--blank-p (reverse lines)))))

(defun ejira-migrate--lines (beg end)
  "Return the lines between BEG and END, without their newlines."
  (split-string (buffer-substring-no-properties beg end) "\n"))

(defun ejira-migrate--comparable (text)
  "Return TEXT reduced to what a copy of it keeps: no stars, drawers or wrapping."
  (string-trim
   (replace-regexp-in-string
    "[ \t\n]+" " "
    (replace-regexp-in-string
     "^\\*+ " ""
     (replace-regexp-in-string "^[ \t]*:[A-Za-z_-]+:.*$" "" text)))))

(defun ejira-migrate--subtree-text (marker)
  "Return the subtree at MARKER as text."
  (org-with-point-at marker
    (buffer-substring-no-properties (point) (save-excursion (org-end-of-subtree t t) (point)))))

(defun ejira-migrate--children ()
  "Return markers on the direct children of the heading at point."
  (save-excursion
    (let (children)
      (when (org-goto-first-child)
        (push (point-marker) children)
        (while (org-get-next-sibling)
          (push (point-marker) children)))
      (nreverse children))))

(defun ejira-migrate--local-section-p (marker)
  "Return non-nil when the child at MARKER is a local section of its task.
Not a task, nor one of the headings ejira owns."
  (org-with-point-at marker
    (not (or (ejira--task-heading-p)
             (member (org-get-heading t t t t)
                     (list "Description" ejira-comments-heading-name
                           ejira-jira-description-heading-name))))))

(defun ejira-migrate--convert-at-point ()
  "Bring the legacy task at point to the current layout.
Return a plist: :action, `folded', `projection' or `none'; :desc-id,
the `ID' of a removed `Description' heading; :dropped, the number of
stale description copies removed.

A section next to the `Description' child holding nothing but a copy
of the description (wrapping aside) is a leftover of an older pull: it
is removed.  When the task then has neither own prose nor another
local section, its `Description' child is folded into its body (its
subheadings lose one star, one blank line around the body).
Otherwise those are local notes, which the body layout would send as
the description: the `Description' child becomes the task's
`JIRA_DESCRIPTION' projection, and the notes stay local."
  (let* ((desc (ejira--find-child-heading "Description"))
         (desc-text (and desc (ejira-migrate--comparable
                               (ejira-migrate--subtree-text desc))))
         (dropped 0))
    ;; Stale copies, last first so the earlier positions hold.
    (when (and desc-text (not (string-empty-p desc-text)))
      (dolist (m (reverse (ejira-migrate--children)))
        (when (and (ejira-migrate--local-section-p m)
                   (let ((copy (ejira-migrate--comparable (ejira-migrate--subtree-text m))))
                     (and (not (string-empty-p copy))
                          (string-search copy desc-text))))
          (org-with-point-at m
            (delete-region (point) (save-excursion (org-end-of-subtree t t) (point))))
          (cl-incf dropped))))
    (let ((own (ejira-migrate--trim
                (ejira-migrate--lines (save-excursion (ejira--body-start))
                                      (save-excursion
                                        (org-back-to-heading t)
                                        (if (org-goto-first-child)
                                            (point)
                                          (org-end-of-subtree t t)
                                          (point))))))
          (local (cl-some #'ejira-migrate--local-section-p (ejira-migrate--children))))
      (cond
       ((or own local)
        (if desc
            (org-with-point-at desc (org-edit-headline ejira-jira-description-heading-name))
          ;; No description: an empty projection keeps the notes local.
          (save-excursion
            (let ((level (1+ (org-current-level))))
              (if (org-goto-first-child)
                  (beginning-of-line)
                (org-end-of-subtree t t))
              (unless (bolp) (insert "\n"))
              (insert (make-string level ?*) " " ejira-jira-description-heading-name "\n"))))
        (list :action 'projection :dropped dropped))
       (desc (list :action 'folded :desc-id (ejira-migrate--fold-description desc)
                   :dropped dropped))
       (t (list :action 'none :dropped dropped))))))

(defun ejira-migrate--fold-description (desc)
  "Fold the `Description' child at DESC into the body of the task at point.
The task has no own prose.  Return the `ID' of the removed heading."
  (let* ((own-beg (save-excursion (ejira--body-start)))
         (own-end (save-excursion
                    (org-back-to-heading t)
                    (org-goto-first-child)
                    (point)))
         (desc-id (org-entry-get desc "ID"))
         (desc-beg (marker-position desc))
         (desc-end (org-with-point-at desc (org-end-of-subtree t t) (point)))
         (body (ejira-migrate--trim
                (mapcar (lambda (l)
                          (if (ejira-migrate--heading-line-p l) (substring l 1) l))
                        (ejira-migrate--lines
                         (org-with-point-at desc (ejira--body-start))
                         desc-end)))))
    ;; The description subtree lies after the own body: remove it
    ;; first, so the body's positions still hold.
    (delete-region desc-beg desc-end)
    (delete-region own-beg own-end)
    (goto-char own-beg)
    (insert (if body (concat "\n" (string-join body "\n") "\n\n") "\n"))
    desc-id))

;;; Baselines as the old schema wrote them

(defconst ejira-migrate--issue-types
  '("ejira-issue" "ejira-story" "ejira-subtask" "ejira-epic")
  "Heading TYPE values of Jira issues.")

(defun ejira-migrate--issue-heading-p ()
  "Return the key of the issue heading at point, or nil."
  (let ((id (org-entry-get nil "ID")))
    (and id
         (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" id)
         (member (org-entry-get nil "TYPE") ejira-migrate--issue-types)
         id)))

(defun ejira-migrate--content-fields (description)
  "Content fields of the issue at point, with DESCRIPTION as its description.
The formula of `ejira--heading-content-fields'."
  (concat (ejira--push-normalize (ejira--jira-summary))
          "\0"
          (ejira--push-normalize description)
          "\0"
          (ejira--push-normalize
           (or (org-entry-get nil ejira-priority-id-property) ""))
          "\0"
          (ejira--push-normalize
           (or (when-let* ((d (org-get-deadline-time (point-marker))))
                 (format-time-string "%Y-%m-%d" d))
               ""))))

(defun ejira-migrate--single-hash-content (description)
  "The content the legacy single `Pushhash' hashed, for the issue at point.
DESCRIPTION is its description as that layout held it."
  (concat (ejira--push-normalize (ejira--jira-summary))
          "\0"
          (ejira--push-normalize description)
          "\0"
          (ejira--push-normalize (or (org-entry-get nil "Assignee") ""))
          "\0"
          (ejira--push-normalize
           (or (save-excursion
                 (org-back-to-heading t)
                 (when (looking-at org-priority-regexp)
                   (match-string 2)))
               ""))
          (or (when-let* ((priority-id (org-entry-get nil ejira-priority-id-property)))
                (concat "\0" (ejira--push-normalize priority-id)))
              "")
          "\0"
          (ejira--push-normalize
           (or (when-let* ((d (org-get-deadline-time (point-marker))))
                 (format-time-string "%Y-%m-%d" d))
               ""))
          "\0"
          (ejira--push-normalize (or (org-get-todo-state) ""))))

(defun ejira-migrate--old-issue ()
  "Read the issue heading at point as the old schema held it.
Return a plist: :layout (`body', `legacy' or `projection'), and
:content and :state, the hashes its `LocalHash' takes: a hash string,
`fresh' (recompute from the migrated heading: it had no pending edit),
or nil (no baseline).  :content is `equivalent' for a clean legacy
task, whose description moves: recomputed too.  :stuck is non-nil
for a legacy single-hash issue with pending edits, which cannot be
split into content and state.  :was-dirty is non-nil when the issue
had pending local edits."
  (let* ((layout (cond ((ejira--jira-projection-p) 'projection)
                       ((equal "t" (org-entry-get nil "EJIRA_DESCRIPTION_IN_BODY" t)) 'body)
                       (t 'legacy)))
         (old-desc (or (pcase layout
                         ('legacy (when-let* ((d (ejira--find-child-heading "Description")))
                                    (ejira--get-heading-body d)))
                         (_ (ejira--jira-description)))
                       ""))
         (pushhash (org-entry-get nil "Pushhash"))
         (statehash (org-entry-get nil "Statehash"))
         (fields (ejira-migrate--content-fields old-desc)))
    (append
     (list :layout layout)
     (cond
      ((null pushhash) (list :content nil :state nil))
      ((and statehash (string-prefix-p "v2:" pushhash))
       (let ((clean (equal pushhash (concat "v2:" (md5 fields)))))
         (list :content (if (and clean (eq layout 'legacy)) 'equivalent
                          (substring pushhash 3))
               :state statehash
               :was-dirty (or (not clean)
                              (not (equal statehash (md5 (ejira--heading-state-fields))))
                              (ejira--priority-cookie-modified-p)))))
      ((equal pushhash (md5 (ejira-migrate--single-hash-content old-desc)))
       (list :content (if (eq layout 'legacy) 'equivalent 'fresh)
             :state 'fresh))
      (t (list :stuck t :was-dirty t))))))

;;; Migration

(defun ejira-migrate--org-buffer (text)
  "Return a new Org buffer holding TEXT, outside any file and hook."
  (let ((buf (generate-new-buffer " *ejira-migrate*")))
    (with-current-buffer buf
      (insert text)
      (let ((org-mode-hook nil))
        (delay-mode-hooks (org-mode)))
      (setq-local org-startup-indented nil))
    buf))

(defun ejira-migrate--map-headings (fn)
  "Call FN with point on each heading of the current buffer, in order."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-heading-regexp nil t)
     (save-excursion
       (beginning-of-line)
       (funcall fn))
     (goto-char (line-end-position)))))

(defun ejira-migrate-file (file)
  "Return the migration of FILE: (TEXT . REPORT), nothing written.
TEXT is the migrated content; REPORT a plist describing it, including
:stuck, issues whose pending edits block the migration."
  (let* ((old (make-hash-table :test 'equal))
         (report (list :file file :issues 0 :folded 0 :clean 0 :dirty 0 :dirty-before 0
                       :no-baseline 0 :comments 0 :dropped 0 :projections nil
                       :stuck nil
                       :description-ids nil :duplicate-ids nil))
         (legacy nil)
         (ejira--pre-scanning t)
         text)
    ;; Read the old baselines, then fold the legacy descriptions, from
    ;; the last task up so that every fold leaves the positions above it.
    (let ((buf (ejira-migrate--org-buffer
                (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (unwind-protect
          (with-current-buffer buf
            (ejira-migrate--map-headings
             (lambda ()
               (when-let* ((key (ejira-migrate--issue-heading-p)))
                 (if (gethash key old)
                     (push key (plist-get report :duplicate-ids))
                   (let ((info (ejira-migrate--old-issue)))
                     (puthash key info old)
                     (when (plist-get info :was-dirty)
                       (cl-incf (plist-get report :dirty-before)))
                     (when (plist-get info :stuck)
                       (push key (plist-get report :stuck)))
                     (when (eq (plist-get info :layout) 'legacy)
                       (push (point-marker) legacy)))))))
            ;; `legacy' holds the markers last first.
            (dolist (m legacy)
              (goto-char m)
              (let* ((key (org-entry-get nil "ID"))
                     (done (ejira-migrate--convert-at-point)))
                (cl-incf (plist-get report :dropped) (plist-get done :dropped))
                (pcase (plist-get done :action)
                  ('folded
                   (cl-incf (plist-get report :folded))
                   (when-let* ((desc-id (plist-get done :desc-id)))
                     (push (cons desc-id key) (plist-get report :description-ids))))
                  ('projection
                   (push key (plist-get report :projections))))))
            (setq text (buffer-substring-no-properties (point-min) (point-max))))
        (kill-buffer buf)))
    ;; Drop the layout opt-in and the dated-baseline property, rename
    ;; the remote hash.
    (setq text (replace-regexp-in-string
                "^\\(?:#\\+PROPERTY:[ \t]+EJIRA_DESCRIPTION_IN_BODY\\|[ \t]*:EJIRA_DESCRIPTION_IN_BODY:\\|[ \t]*:Remoteupdated:\\).*\n"
                "" text))
    (setq text (replace-regexp-in-string "^\\([ \t]*\\):Remotehash:" "\\1:RemoteHash:" text t))
    ;; Record the local baselines with the current code.
    (let ((buf (ejira-migrate--org-buffer text)))
      (unwind-protect
          (with-current-buffer buf
            (ejira-migrate--map-headings
             (lambda ()
               (cond
                ((ejira--comment-heading-p)
                 (when-let* ((hash (org-entry-get nil "Pushhash")))
                   (cl-incf (plist-get report :comments))
                   (org-set-property ejira-local-hash-property hash)
                   (org-delete-property "Pushhash")))
                ((when-let* ((key (ejira-migrate--issue-heading-p))
                             (info (gethash key old)))
                   (cl-incf (plist-get report :issues))
                   (ejira-migrate--record-local-baseline info report)
                   t))
                ;; Any other heading has no baseline to keep.
                (t (org-delete-property "Pushhash")
                   (org-delete-property "Statehash")))))
            (cons (buffer-substring-no-properties (point-min) (point-max))
                  report))
        (kill-buffer buf)))))

(defun ejira-migrate--record-local-baseline (info report)
  "Write the `LocalHash' of the issue at point from its old baselines INFO.
See `ejira-migrate--old-issue' for INFO; REPORT is updated."
  (let ((content (plist-get info :content))
        (state (plist-get info :state)))
    ;; A converted task's description is its old one: folded only when
    ;; the body held nothing else, kept apart as a projection otherwise.
    ;; Its text moved, so its hash is recomputed.
    (when (memq content '(fresh equivalent))
      (setq content (md5 (ejira--heading-content-fields))))
    (when (eq state 'fresh)
      (setq state (md5 (ejira--heading-state-fields))))
    (org-delete-property "Pushhash")
    (org-delete-property "Statehash")
    (cond
     ((plist-get info :stuck))
     ((and content state)
      (org-set-property ejira-local-hash-property (concat content " " state))
      (if (ejira--locally-modified-p)
          (cl-incf (plist-get report :dirty))
        (cl-incf (plist-get report :clean))))
     (t (cl-incf (plist-get report :no-baseline))))))

(defun ejira-migrate-files (files &optional apply)
  "Migrate FILES to the current schema; write them with APPLY.
See the commentary of ejira-migrate.el.  Without APPLY, nothing is
written.  Refuses to write when an issue cannot be migrated (see
:stuck in the reports) or carries a duplicate ID within its file.
Return the list of reports."
  (let* ((results (mapcar (lambda (f) (cons f (ejira-migrate-file f))) files))
         (reports (mapcar (lambda (r) (cddr r)) results))
         (blocking (cl-remove-if-not (lambda (r) (or (plist-get r :stuck)
                                                     (plist-get r :duplicate-ids)))
                                     reports)))
    (when (and apply blocking)
      (user-error "ejira-migrate: not written, issues need a hand first: %S"
                  (mapcar (lambda (r) (list (plist-get r :file)
                                            :stuck (plist-get r :stuck)
                                            :duplicates (plist-get r :duplicate-ids)))
                          blocking)))
    (when apply
      (dolist (r results)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (cadr r) nil (car r) nil 'silent))))
    reports))

(provide 'ejira-migrate)
;;; ejira-migrate.el ends here
