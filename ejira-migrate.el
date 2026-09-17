;;; ejira-migrate.el --- One-shot Description-fold migration -*- lexical-binding: t; -*-

;; Folds legacy `Description' child headings into task bodies (body mode)
;; with PURE LINE-BASED construction: the file is parsed into a block
;; tree, the new content is built as text, and the file is written once.
;; No buffer mutation of the real file.
;;
;; Baselines: each task's v2 Pushhash is computed in a throwaway buffer
;; rendering the REMOTE-EQUIVALENT body (the old description content as
;; the task's body), so a fold that matches the remote state baselines
;; clean, while union folds (own prose + description) stay dirty for
;; their reviewed first push.
;;
;; Set `ejira-migrate--apply' to t to write; nil = dry run.

(defvar ejira-migrate--apply nil)
(defvar ejira-migrate--file "/Users/val/Documents/projects/pr-bot/secateur/todo.org")
(defvar ejira-migrate--result nil)

(defun ejira-migrate--heading-level (line)
  "Return the outline LEVEL of LINE, or nil when it is not a heading."
  (when (string-match "^\\*+ " line)
    (- (match-end 0) (match-beginning 0) 1)))

(defun ejira-migrate--heading-title (line)
  "Return the title text of heading LINE (stars, keyword, cookie kept)."
  (when (string-match "^\\*+ " line)
    (substring line (match-end 0))))

(defun ejira-migrate--blank-p (line)
  (string-empty-p (string-trim line)))

(defun ejira-migrate--dedent (lines)
  "Remove one leading star from every heading line in LINES."
  (mapcar (lambda (l)
            (if (and (ejira-migrate--heading-level l)
                     (string-prefix-p "*" l))
                (substring l 1)
              l))
          lines))

(defun ejira-migrate--trim-head (lines)
  "Drop leading blank lines from LINES."
  (while (and lines (ejira-migrate--blank-p (car lines)))
    (setq lines (cdr lines)))
  lines)

(defun ejira-migrate--trim-tail (lines)
  "Drop trailing blank lines from LINES."
  (let ((n (length lines)))
    (while (and (> n 0) (ejira-migrate--blank-p (nth (1- n) lines)))
      (cl-decf n))
    (seq-take lines n)))

(defun ejira-migrate--meta-end (lines)
  "Index of the first line of LINES that is not planning/drawer metadata."
  (let ((i 0) (n (length lines)))
    (while (and (< i n) (string-match-p "^[A-Z]+:" (nth i lines)))
      (cl-incf i))
    (while (and (< i n) (string-match-p "^:[A-Za-z_]+:" (nth i lines)))
      (while (and (< i n) (not (string-match-p "^:END:" (nth i lines))))
        (cl-incf i))
      (when (< i n) (cl-incf i)))
    i))

(defun ejira-migrate--parse (lines level)
  "Parse LINES into blocks at LEVEL.
Returns (BLOCKS . REST); REST begins at the first line that is a
heading shallower than LEVEL (or is nil).  Lines before the first
LEVEL heading become a single (:level 0) verbatim block."
  (let ((blocks nil))
    (while (and lines
                (let ((l (ejira-migrate--heading-level (car lines))))
                  (or (null l) (>= l level))))
      (let ((l (ejira-migrate--heading-level (car lines))))
        (cond
         ((null l)
          (let ((take nil))
            (while (and lines (null (ejira-migrate--heading-level (car lines))))
              (push (car lines) take)
              (setq lines (cdr lines)))
            (push (list :level 0 :lines (nreverse take)) blocks)))
         (t
          (let* ((heading-line (car lines))
                 (own nil)
                 (rest (cdr lines)))
            (while (and rest (null (ejira-migrate--heading-level (car rest))))
              (push (car rest) own)
              (setq rest (cdr rest)))
            (setq own (nreverse own))
            (let* ((meta-end (ejira-migrate--meta-end own))
                   (meta (seq-take own meta-end))
                   (prose (seq-drop own meta-end))
                   (parsed (ejira-migrate--parse rest (1+ level))))
              (push (list :level l
                          :heading-line heading-line
                          :title (string-trim
                                  (or (ejira-migrate--heading-title
                                       heading-line)
                                      ""))
                          :todo-keyword (and (string-match
                                              "^\\*+ \\([A-Z]+\\) "
                                              heading-line)
                                             (match-string 1 heading-line))
                          :meta meta
                          :own prose
                          :children (car parsed))
                    blocks)
              (setq lines (cdr parsed)))))))
  (cons (nreverse blocks) lines)))

(defun ejira-migrate--render-block (b)
  "Flatten block B back into lines."
  (if (= 0 (plist-get b :level))
      (plist-get b :lines)
    (let ((body (append
                 (plist-get b :own)
                 (apply #'append
                        (mapcar #'ejira-migrate--render-block
                                (plist-get b :children))))))
      (append (list (plist-get b :heading-line))
              (plist-get b :meta)
              (if (seq-some (lambda (l) (not (ejira-migrate--blank-p l)))
                            body)
                  (cons ""
                        (append (ejira-migrate--trim-head
                                 (ejira-migrate--trim-tail body))
                                (list "")))
                (list ""))))))

(defun ejira-migrate--drawer-props (meta-lines)
  "Return the alist of drawer properties in META-LINES."
  (let ((props nil))
    (dolist (l meta-lines)
      (when (string-match "^:\\([A-Za-z_]+\\):[ \t]*\\(.*?\\)[ \t]*$" l)
        (push (cons (match-string 1 l) (match-string 2 l)) props)))
    (nreverse props)))

(defun ejira-migrate--task-p (b)
  "Return non-nil when block B is a synchronized task heading."
  (let* ((props (ejira-migrate--drawer-props (plist-get b :meta)))
         (type (cdr (assoc "TYPE" props)))
         (id (cdr (assoc "ID" props))))
    (and id (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" id)
         (or (plist-get b :todo-keyword)
             (and type (member type ejira-pushable-types)
                  (not (equal type "ejira-comment")))))))

(defun ejira-migrate--projection-p (b)
  "Return non-nil when task block B uses a Jira content projection."
  (let* ((props (ejira-migrate--drawer-props (plist-get b :meta)))
         (desc-child (cl-find-if
                      (lambda (c)
                        (and (> (plist-get c :level) 0)
                             (equal (plist-get c :title)
                                    "JIRA_DESCRIPTION")))
                      (plist-get b :children))))
    (or (cdr (assoc "JIRA_TITLE" props)) desc-child)))

(defun ejira-migrate--fold (b)
  "Fold task block B's Description child into its body.
Returns (NEW-BLOCK . FOLD-INFO-OR-NIL)."
  (let* ((props (ejira-migrate--drawer-props (plist-get b :meta)))
         (key (cdr (assoc "ID" props)))
         (title (plist-get b :title))
         (children (plist-get b :children))
         (desc-block (cl-find-if
                      (lambda (c)
                        (and (> (plist-get c :level) 0)
                             (equal (plist-get c :title) "Description")))
                      children)))
    (if (not desc-block)
        (cons b nil)
      (let* ((dprops (ejira-migrate--drawer-props
                      (plist-get desc-block :meta)))
             (desc-id (cdr (assoc "ID" dprops)))
             (own (ejira-migrate--trim-head
                   (ejira-migrate--trim-tail (plist-get b :own))))
             (desc-content-lines
              (ejira-migrate--dedent
               (append (plist-get desc-block :own)
                       (apply #'append
                              (mapcar #'ejira-migrate--render-block
                                      (plist-get desc-block :children))))))
             (desc-content (ejira-migrate--trim-head
                            (ejira-migrate--trim-tail desc-content-lines)))
             (desc-text (string-join desc-content "\n"))
             (own-text (string-join own "\n"))
             (folded
              (cond
               ((= (length desc-text) 0) own)
               ((string-equal desc-text own-text) own)
               ((= (length own-text) 0) desc-content)
               (t (append own (cons "" desc-content)))))
             (new-block (list :level (plist-get b :level)
                              :heading-line (plist-get b :heading-line)
                              :title title
                              :todo-keyword (plist-get b :todo-keyword)
                              :meta (plist-get b :meta)
                              :own folded
                              :children
                              (cl-remove-if
                               (lambda (c) (eq c desc-block))
                               children)))
             (remote-equiv-body
              (if (or (= (length desc-text) 0)
                      (string-equal desc-text own-text))
                  own
                desc-content)))
        (cons new-block
              (list :key key :title title :desc-id desc-id
                    :action (cond ((string-equal desc-text own-text)
                                   'dup)
                                  ((= (length own-text) 0) 'desc-only)
                                  (t 'diff))
                    :baseline-heading (plist-get b :heading-line)
                    :baseline-meta (plist-get b :meta)
                    :baseline-body remote-equiv-body)))))))

(defun ejira-migrate--content-identity ()
  "Return the canonical content identity for the heading at point.
The description component comes from the body accessor; the caller
binds `ejira-description-in-body'."
  (concat
   (ejira--push-normalize (ejira--jira-summary))
   "\0"
   (ejira--push-normalize (or (ejira--jira-description) ""))
   "\0"
   (ejira--push-normalize
    (or (org-entry-get nil ejira-priority-id-property) ""))
   "\0"
   (ejira--push-normalize
    (or (when-let ((d (org-get-deadline-time (point-marker))))
          (format-time-string "%Y-%m-%d" d))
        ""))))

(defun ejira-migrate--baselines (fold-info)
  "Compute the v2 Pushhash and Statehash for FOLD-INFO."
  (let ((ejira-description-in-body t))
    (with-temp-buffer
      (org-mode)
      (insert (plist-get fold-info :baseline-heading) "\n")
      (dolist (l (plist-get fold-info :baseline-meta))
        (insert l "\n"))
      (insert "\n")
      (dolist (l (plist-get fold-info :baseline-body))
        (insert l "\n"))
      (goto-char (point-min))
      (org-back-to-heading t)
      (list :pushhash
            (concat ejira-pushhash-v2-prefix
                    (md5 (ejira-migrate--content-identity)))
            :statehash (md5 (ejira--heading-state-fields))))))

(defun ejira-migrate--set-props (meta-lines prop-alist)
  "Set drawer properties in META-LINES from PROP-ALIST."
  (let ((lines (copy-sequence meta-lines))
        (drawer-start (cl-position-if
                       (lambda (l) (string-match-p "^:PROPERTIES:" l))
                       lines)))
    (dolist (p prop-alist)
      (let ((pat (concat "^:" (regexp-quote (car p)) ":[ \t]*.*$"))
            (found nil))
        (setq lines
              (mapcar (lambda (l)
                        (if (and (string-match-p "^:[A-Za-z_]+:" l)
                                 (string-match pat l))
                            (progn (setq found t)
                                   (format ":%s:     %s" (car p) (cdr p)))
                          l))
                      lines))
        (unless found
          (when drawer-start
            (setq lines
                  (append (seq-take lines (1+ drawer-start))
                          (list (format ":%s:     %s" (car p) (cdr p)))
                          (seq-drop lines (1+ drawer-start))))))))
    lines))

(defvar ejira-migrate--counts nil)
(defvar ejira-migrate--journal nil)

(defun ejira-migrate--transform (blocks)
  "Fold Description children of task blocks in BLOCKS."
  (let ((out nil))
    (dolist (b blocks)
      (cond
       ((= 0 (plist-get b :level))
        (push b out))
       ((and (ejira-migrate--task-p b)
             (not (ejira-migrate--projection-p b)))
        (let* ((folded (ejira-migrate--fold b))
               (new-block (car folded))
               (info (cdr folded)))
          (when info
            (cl-incf (plist-get ejira-migrate--counts :folded))
            (cl-incf (plist-get ejira-migrate--counts
                                (plist-get info :action)))
            (when (plist-get info :desc-id)
              (push (format "- [[id:%s][Description section of %s]]"
                            (plist-get info :desc-id)
                            (plist-get info :title))
                    ejira-migrate--journal))
            (let* ((hashes (ejira-migrate--baselines info))
                   (meta-with-hash
                    (ejira-migrate--set-props
                     (plist-get new-block :meta)
                     (list (cons "Pushhash"
                                 (plist-get hashes :pushhash))
                           (cons "Statehash"
                                 (plist-get hashes :statehash))))))
              (plist-put new-block :meta meta-with-hash)))
          (push new-block out)))
       (t
        (plist-put b :children
                   (mapcar (lambda (c)
                             (car (ejira-migrate--transform (list c))))
                           (plist-get b :children)))
        (push b out))))
    (nreverse out)))

(defun ejira-migrate--run ()
  (let* ((orig-lines (split-string
                      (with-temp-buffer
                        (insert-file-contents ejira-migrate--file)
                        (buffer-string))
                      "\n") )
         (parsed (ejira-migrate--parse orig-lines 0))
         (ejira-migrate--counts
          (list :folded 0 :dup 0 :desc-only 0 :diff 0 :none 0
                :projections 0 :errors 0))
         (ejira-migrate--journal nil)
         (new-blocks (mapcar
                      (lambda (b)
                        (car (ejira-migrate--transform (list b))))
                      (car parsed))))
    (when ejira-migrate--apply
      (let ((new-text (string-join
                       (cons "#+PROPERTY: EJIRA_DESCRIPTION_IN_BODY t"
                             (ejira-migrate--render-block
                              (list :level 0 :lines nil :children new-blocks)))
                       "\n")))
        (with-temp-file ejira-migrate--file
          (insert new-text "\n"))
        (with-temp-file
            (concat (file-name-directory ejira-migrate--file)
                    "ejira-migration/migration-journal-2026-09-17.org")
          (insert
           "#+TITLE: ejira description-fold migration journal\n"
           (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M"))
           (format "- folded: %d (dup %d, desc-only %d, diff %d); projections kept: %d\n\n"
                   (plist-get ejira-migrate--counts :folded)
                   (plist-get ejira-migrate--counts :dup)
                   (plist-get ejira-migrate--counts :desc-only)
                   (plist-get ejira-migrate--counts :diff)
                   (plist-get ejira-migrate--counts :projections))
           "Removed Description section headings (IDs preserved for link recovery):\n"
           (string-join ejira-migrate--journal "\n")))))
    (setq ejira-migrate--result
          (append ejira-migrate--counts
                  (list :applied ejira-migrate--apply
                        :journal (length ejira-migrate--journal))))))

(provide 'ejira-migrate)
