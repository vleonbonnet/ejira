;;; ejira-confirm.el --- Push confirmation buffer -*- lexical-binding: t -*-

;; This file is part of ejira.

;;; Commentary:

;; Provides a dedicated major mode for reviewing pending push changes
;; before sending them to the Jira API.
;;
;; Tree layout: project > issue-node > field / subitem > field
;;
;;   Ejira Push — 3 items: 1 new, 1 modified
;;   ══════════════════════════════════════════
;;   ▼ RNDSEC  1 new, 1 modified
;;     ▼ ✎ RNDSEC-123  summary, 1 new subtask
;;       ▶ summary:     old title → new title
;;       ▶ + subtask: TODO  My new task
;;             title:        My new task
;;           ▶ description:  "Body text…"
;;
;; Overlay visibility: project/issue nodes start expanded (▼); field and
;; subitem nodes start collapsed (▶).  Short single-line fields are shown
;; inline with no toggle.  TAB toggles any collapsible section.
;;
;; C-c C-c sends all items, C-c C-k aborts.

;;; Code:

(require 'cl-lib)

(defvar ejira--pushing nil
  "Bound to t while ejira-push is executing a batch; inhibits re-scan on save.
Defined in ejira-push.el; declared here so ejira-confirm-execute can bind it.")

;;; ── Faces ────────────────────────────────────────────────────────────────────

(defface ejira-confirm-title
  '((t :weight bold))
  "Face for the confirmation buffer title."
  :group 'ejira)

(defface ejira-confirm-project
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for project section headers."
  :group 'ejira)

(defface ejira-confirm-item
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for item (issue/comment) names."
  :group 'ejira)

(defface ejira-confirm-field-name
  '((t :inherit font-lock-variable-name-face))
  "Face for field labels."
  :group 'ejira)

(defface ejira-confirm-old-value
  '((t :foreground "#cc4444"))
  "Face for old (replaced) values."
  :group 'ejira)

(defface ejira-confirm-new-value
  '((t :foreground "#44aa44"))
  "Face for new values."
  :group 'ejira)

(defface ejira-confirm-summary
  '((t :inherit font-lock-comment-face))
  "Face for summary/dim text."
  :group 'ejira)

(defface ejira-confirm-key
  '((t :weight bold :inherit font-lock-builtin-face))
  "Face for keybinding hints."
  :group 'ejira)

(defface ejira-confirm-new
  '((t :foreground "#44aa44" :weight bold))
  "Face for new item markers (+)."
  :group 'ejira)

(defface ejira-confirm-deleted
  '((t :foreground "#cc4444" :weight bold))
  "Face for deleted item markers (✗)."
  :group 'ejira)

(defface ejira-confirm-warning
  '((t :foreground "#cc8800" :weight bold))
  "Face for deletion warning text."
  :group 'ejira)

;;; ── Buffer-local state ───────────────────────────────────────────────────────

(defvar-local ejira-confirm--plans nil)

(defvar-local ejira-confirm--nodes nil
  "List of plists (:marker MARKER :overlay OVERLAY :level LEVEL).
LEVEL is one of: project item field.")


;;; ── String helpers ───────────────────────────────────────────────────────────

(defun ejira-confirm--normalize (s)
  "Strip CR and trim S; nil becomes \"\"."
  (string-trim (replace-regexp-in-string "\r" "" (or s ""))))

(defun ejira-confirm--for-display (s)
  "Return S normalized for display (CR stripped, trimmed). Raw markup preserved."
  (ejira-confirm--normalize s))

(defun ejira-confirm--truncate (s)
  "Flatten and truncate S to at most 55 chars for one-line display."
  (let ((flat (string-trim (replace-regexp-in-string "[\n\r]+" " " (or s "")))))
    (if (length> flat 55)
        (concat (substring flat 0 55) "…")
      flat)))


;;; ── Diff helpers ─────────────────────────────────────────────────────────────

(defun ejira-confirm-field-changes (fields)
  "Filter FIELDS to entries whose old and new values differ.
FIELDS is a list of (NAME OLD NEW) string triples."
  (cl-remove-if (lambda (f)
                  (equal (ejira-confirm--normalize (nth 1 f))
                         (ejira-confirm--normalize (nth 2 f))))
                fields))

;;; ── Grouping ─────────────────────────────────────────────────────────────────

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

;;; ── Summary helpers ──────────────────────────────────────────────────────────

(defun ejira-confirm--op-summary (plans &optional prefix)
  "One-line count summary for PLANS, optionally prefixed with PREFIX."
  (let* ((n-create (cl-count-if (lambda (p) (eq (plist-get p :op) 'create)) plans))
         (n-update (cl-count-if (lambda (p) (eq (plist-get p :op) 'update)) plans))
         (n-delete (cl-count-if (lambda (p) (eq (plist-get p :op) 'delete)) plans))
         (parts nil))
    (when (> n-create 0) (push (format "%d new"      n-create) parts))
    (when (> n-update 0) (push (format "%d modified" n-update) parts))
    (when (> n-delete 0) (push (format "%d deleted"  n-delete) parts))
    (if parts
        (concat (or prefix "  ") (string-join (nreverse parts) ", "))
      "")))

(defun ejira-confirm--issue-summary (plans)
  "One-line summary for an issue node header given its PLANS."
  (let* ((update  (cl-find-if (lambda (p) (and (eq (plist-get p :op) 'update)
                                               (eq (plist-get p :object) 'issue)))
                              plans))
         (creates (cl-remove-if-not (lambda (p) (eq (plist-get p :op) 'create)) plans))
         (deletes (cl-remove-if-not (lambda (p) (eq (plist-get p :op) 'delete)) plans))
         (parts nil))
    (when update
      (when-let ((fields (mapcar #'car (plist-get update :changes))))
        (push (string-join fields ", ") parts)))
    (let ((n-sub (cl-count-if (lambda (p) (eq (plist-get p :object) 'subtask)) creates))
          (n-cmt (cl-count-if (lambda (p) (eq (plist-get p :object) 'comment)) creates))
          (n-iss (cl-count-if (lambda (p) (eq (plist-get p :object) 'issue))   creates)))
      (when (> n-sub 0) (push (format "%d new subtask%s" n-sub (if (= n-sub 1) "" "s")) parts))
      (when (> n-cmt 0) (push (format "%d new comment%s" n-cmt (if (= n-cmt 1) "" "s")) parts))
      (when (> n-iss 0) (push (format "%d new issue%s"   n-iss (if (= n-iss 1) "" "s")) parts)))
    (when (> (length deletes) 0)
      (push (format "%d deleted" (length deletes)) parts))
    (if parts (concat "  " (string-join (nreverse parts) ", ")) "")))

;;; ── Node registry ────────────────────────────────────────────────────────────
;;
;; Each collapsible section registers itself via ejira-confirm--register-node.
;; Visibility rules:
;;   project / issue-node → start VISIBLE  (▼, start-hidden=nil)
;;   subitem / top-level-item → start HIDDEN  (▶, start-hidden=t)
;;   multi-line fields → start HIDDEN  (▶, start-hidden=t)
;;   single-line fields → rendered inline, no overlay

(defun ejira-confirm--register-node (header-start detail-start level start-hidden)
  "Register a collapsible overlay from DETAIL-START to point.
HEADER-START is the beginning of the header line; LEVEL is a symbol.
When START-HIDDEN is non-nil the body starts invisible."
  (let ((ov (make-overlay detail-start (point))))
    (overlay-put ov 'invisible start-hidden)
    (overlay-put ov 'ejira-confirm-node t)
    (push (list :marker (copy-marker header-start) :overlay ov :level level)
          ejira-confirm--nodes)))

;;; ── Core field primitives ────────────────────────────────────────────────────
;;
;; insert-diff-field: expandable OLD → NEW diff, always collapsible.
;; insert-new-field: single-line values shown inline; multi-line get overlay.

(defun ejira-confirm--insert-diff-field (name old new indent)
  "Insert an expandable OLD → NEW diff row for field NAME at INDENT spaces."
  (let* ((pad      (make-string indent ?\s))
         (bpad     (make-string (+ indent 4) ?\s))
         (old-disp (ejira-confirm--for-display old))
         (new-disp (ejira-confirm--for-display new))
         (header-start (point)))
    (insert pad
            (propertize "▶" 'ejira-confirm-arrow t)
            " "
            (propertize (format "%-14s" (concat name ":")) 'face 'ejira-confirm-field-name)
            (propertize (ejira-confirm--truncate old-disp) 'face 'ejira-confirm-old-value)
            " → "
            (propertize (ejira-confirm--truncate new-disp) 'face 'ejira-confirm-new-value)
            "\n")
    (let ((detail-start (point)))
      (insert (propertize (concat bpad "── old ──\n") 'face 'ejira-confirm-summary))
      (dolist (line (split-string old-disp "\n"))
        (insert bpad (propertize line 'face 'ejira-confirm-old-value) "\n"))
      (insert (propertize (concat bpad "── new ──\n") 'face 'ejira-confirm-summary))
      (dolist (line (split-string new-disp "\n"))
        (insert bpad (propertize line 'face 'ejira-confirm-new-value) "\n"))
      (ejira-confirm--register-node header-start detail-start 'field t))))

(defun ejira-confirm--insert-new-field (name value indent)
  "Insert a new-value field for NAME at INDENT spaces.
Single-line values are shown inline (no toggle); multi-line get a collapsible overlay."
  (let* ((pad   (make-string indent ?\s))
         (clean (ejira-confirm--for-display value))
         (lines (split-string clean "\n")))
    (if (= (length lines) 1)
        ;; Single-line: show inline, no expand toggle
        (insert pad
                "  "
                (propertize (format "%-14s" (concat name ":")) 'face 'ejira-confirm-field-name)
                (propertize clean 'face 'ejira-confirm-new-value)
                "\n")
      ;; Multi-line: collapsible overlay, starts collapsed
      (let ((bpad (make-string (+ indent 4) ?\s))
            (header-start (point)))
        (insert pad
                (propertize "▶" 'ejira-confirm-arrow t)
                " "
                (propertize (format "%-14s" (concat name ":")) 'face 'ejira-confirm-field-name)
                (propertize (ejira-confirm--truncate clean) 'face 'ejira-confirm-new-value)
                "\n")
        (let ((detail-start (point)))
          (dolist (line lines)
            (insert bpad (propertize line 'face 'ejira-confirm-new-value) "\n"))
          (ejira-confirm--register-node header-start detail-start 'field t))))))

;;; ── Subitem (create/delete inside an issue node) ─────────────────────────────
;;
;; Subitems start collapsed (▶).  Header shows Jira state before the title.
;; Expanding reveals all fields (title, state, description, …).

(defun ejira-confirm--insert-subitem (plan)
  "Insert a create/delete plan inside an issue node at indent=4."
  (let* ((op      (plist-get plan :op))
         (object  (plist-get plan :object))
         (title   (plist-get plan :title))
         (fields  (plist-get plan :fields))
         (preview (plist-get plan :preview)))
    (pcase op
      ('create
       (let* ((label      (pcase object ('subtask "subtask") ('comment "comment") (_ "new")))
              (state-entry (cl-assoc "state" fields :test #'string=))
              (state-val   (when state-entry (nth 1 state-entry)))
              (clean       (replace-regexp-in-string "^new [a-z]+: " "" (or title "")))
              ;; Truncate title so the whole visible header fits within ~100 chars.
              ;; Fixed prefix: "    ▶ + LABEL: " = 4+1+1+1+1+1+(length label)+2 = 11+(length label)
              ;; Plus state and one space separator if state present.
              (prefix-len  (+ 11 (length label)
                               (if (and state-val (> (length state-val) 0))
                                   (1+ (length state-val))
                                 0)))
              (title-max   (max 20 (- 100 prefix-len)))
              (title-disp  (let ((flat (string-trim
                                        (replace-regexp-in-string "[\n\r]+" " " clean))))
                             (if (> (length flat) title-max)
                                 (concat (substring flat 0 title-max) "…")
                               flat)))
              (header-start (point)))
         (insert "    "
                 (propertize "▶" 'ejira-confirm-arrow t)
                 " "
                 (propertize "+" 'face 'ejira-confirm-new)
                 " "
                 (propertize label 'face 'ejira-confirm-item)
                 ": "
                 (if (and state-val (> (length state-val) 0))
                     (concat (propertize state-val 'face 'ejira-confirm-summary) " ")
                   "")
                 (propertize title-disp 'face 'ejira-confirm-item)
                 "\n")
         (let ((body-start (point)))
           (cond
            (fields
             (dolist (f fields)
               (let ((fname (nth 0 f)) (fval (nth 1 f)))
                 (when (and fval (> (length fval) 0))
                   (ejira-confirm--insert-new-field fname fval 8)))))
            ((and preview (> (length preview) 0))
             (ejira-confirm--insert-new-field "body" preview 8)))
           (ejira-confirm--register-node header-start body-start 'item t))))
      ('delete
       (insert "    "
               (propertize "✗" 'face 'ejira-confirm-deleted)
               " "
               (propertize (or title "") 'face 'ejira-confirm-deleted)
               "  "
               (propertize "⚠ permanent" 'face 'ejira-confirm-warning)
               "\n")
       (when (and preview (> (length preview) 0))
         (let ((header-start (point)))
           (ejira-confirm--insert-new-field "body" preview 8)))))))

;;; ── Issue node ───────────────────────────────────────────────────────────────
;;
;; Issue nodes start expanded (▼).  They contain diff-fields for any updates
;; and subitem entries for creates/deletes — all of which start collapsed.

(defun ejira-confirm--issue-title (key)
  "Return the org heading title for Jira KEY, or nil if not found."
  (when (and (stringp key) (fboundp 'ejira--find-heading))
    (condition-case nil
        (when-let ((m (ejira--find-heading key)))
          (org-with-point-at m
            (org-no-properties (org-get-heading t t t t))))
      (error nil))))

(defun ejira-confirm--insert-issue-node (issue-key plans)
  "Insert a collapsible issue node for ISSUE-KEY containing PLANS."
  (let* ((summary     (ejira-confirm--issue-summary plans))
         (update-plan (cl-find-if (lambda (p) (and (eq (plist-get p :op) 'update)
                                                   (eq (plist-get p :object) 'issue)))
                                  plans))
         (sub-plans   (cl-remove update-plan plans))
         (title       (ejira-confirm--issue-title issue-key))
         (header-start (point)))
    (insert "  "
            (propertize "▼" 'ejira-confirm-arrow t)
            " "
            (if update-plan "✎ " "")
            (propertize issue-key 'face 'ejira-confirm-item)
            (if (and title (not (string-empty-p title)))
                (concat " " (propertize title 'face 'ejira-confirm-summary))
              "")
            (propertize summary 'face 'ejira-confirm-summary)
            "\n")
    (let ((body-start (point)))
      (when update-plan
        (dolist (change (plist-get update-plan :changes))
          (ejira-confirm--insert-diff-field
           (nth 0 change) (nth 1 change) (nth 2 change) 4)))
      (dolist (plan sub-plans)
        (ejira-confirm--insert-subitem plan))
      (ejira-confirm--register-node header-start body-start 'item nil))))

;;; ── Top-level item (new issue with no parent) ────────────────────────────────

(defun ejira-confirm--insert-item (plan)
  "Insert a top-level plan item (no parent issue)."
  (let* ((op       (plist-get plan :op))
         (title    (plist-get plan :title))
         (fields   (plist-get plan :fields))
         (preview  (plist-get plan :preview))
         (children (plist-get plan :children))
         (header-start (point)))
    (pcase op
      ('create
       (let* ((state-entry (cl-assoc "state" fields :test #'string=))
              (state-val   (when state-entry (nth 1 state-entry))))
         (insert "  "
                 (propertize "▶" 'ejira-confirm-arrow t)
                 " "
                 (propertize "+" 'face 'ejira-confirm-new)
                 "  "
                 (if (and state-val (> (length state-val) 0))
                     (concat (propertize state-val 'face 'ejira-confirm-summary) " ")
                   "")
                 (propertize title 'face 'ejira-confirm-item)
                 "\n")
         (let ((body-start (point)))
           (cond
            (fields
             (dolist (f fields)
               (let ((fname (nth 0 f)) (fval (nth 1 f)))
                 (when (and fval (> (length fval) 0))
                   (ejira-confirm--insert-new-field fname fval 6)))))
            ((and preview (> (length preview) 0))
             (ejira-confirm--insert-new-field "body" preview 6)))
           (dolist (child children)
             (let* ((child-title (plist-get child :title))
                    (child-state (plist-get child :state))
                    (child-body  (plist-get child :body))
                    (child-plan  (list :op 'create
                                       :object 'subtask
                                       :title (format "new subtask: %s" child-title)
                                       :fields (delq nil
                                                     (list (list "title" child-title)
                                                           (when (and child-state
                                                                      (> (length child-state) 0))
                                                             (list "state" child-state))
                                                           (when (and child-body
                                                                      (> (length child-body) 0))
                                                             (list "description" child-body)))))))
               (ejira-confirm--insert-subitem child-plan)))
           (ejira-confirm--register-node header-start body-start 'item t))))
      ('update
       ;; Standalone update not inside an issue node (fallback)
       (let* ((changes     (plist-get plan :changes))
              (field-names (mapcar #'car changes)))
         (insert "  "
                 (propertize "▼" 'ejira-confirm-arrow t)
                 " ✎ "
                 (propertize title 'face 'ejira-confirm-item)
                 (propertize (concat "  " (string-join field-names ", "))
                             'face 'ejira-confirm-summary)
                 "\n")
         (let ((body-start (point)))
           (dolist (change changes)
             (ejira-confirm--insert-diff-field
              (nth 0 change) (nth 1 change) (nth 2 change) 4))
           (ejira-confirm--register-node header-start body-start 'item nil))))
      ('delete
       (insert "  "
               (propertize "✗" 'face 'ejira-confirm-deleted)
               " "
               (propertize title 'face 'ejira-confirm-deleted)
               "  "
               (propertize "⚠ permanent" 'face 'ejira-confirm-warning)
               "\n")
       (when (and preview (> (length preview) 0))
         (let ((body-start (point)))
           (ejira-confirm--insert-new-field "body" preview 6)))))))

;;; ── Project section ──────────────────────────────────────────────────────────

(defun ejira-confirm--insert-project (project-key issue-groups)
  "Insert a collapsible project section (starts expanded).
ISSUE-GROUPS is an alist (issue-key-or-nil . plans-list)."
  (let* ((all-plans  (mapcan (lambda (g) (copy-sequence (cdr g))) issue-groups))
         (summary    (ejira-confirm--op-summary all-plans))
         (header-start (point)))
    (insert (propertize "▼" 'ejira-confirm-arrow t)
            " "
            (propertize project-key 'face 'ejira-confirm-project)
            (propertize summary 'face 'ejira-confirm-summary)
            "\n")
    (let ((body-start (point)))
      (dolist (group issue-groups)
        (let ((issue-key (car group))
              (plans     (cdr group)))
          (if issue-key
              (ejira-confirm--insert-issue-node issue-key plans)
            (dolist (plan plans)
              (ejira-confirm--insert-item plan)))))
      (insert "\n")
      (ejira-confirm--register-node header-start body-start 'project nil))))

;;; ── Header / footer ──────────────────────────────────────────────────────────

(defun ejira-confirm--insert-header (n counts)
  "Insert buffer header for N items with op COUNTS alist."
  (let* ((n-create (alist-get 'create counts 0))
         (n-update (alist-get 'update counts 0))
         (n-delete (alist-get 'delete counts 0))
         (parts nil))
    (when (> n-create 0) (push (format "%d new"      n-create) parts))
    (when (> n-update 0) (push (format "%d modified" n-update) parts))
    (when (> n-delete 0) (push (format "%d deleted"  n-delete) parts))
    (let ((line (format "Ejira Push — %d item%s%s"
                        n
                        (if (= n 1) "" "s")
                        (if parts (concat ": " (string-join (nreverse parts) ", ")) ""))))
      ;; Apply the same face to both title and separator so they share identical
      ;; font metrics and string-width matches visual width exactly.
      (insert (propertize line 'face 'ejira-confirm-title)
              "\n"
              (propertize (make-string (string-width line) ?═) 'face 'ejira-confirm-title)
              "\n\n"))))

(defun ejira-confirm--insert-footer ()
  "Insert the keybinding footer."
  (insert "\n"
          (propertize "C-c C-c" 'face 'ejira-confirm-key) "  Push    "
          (propertize "C-c C-k" 'face 'ejira-confirm-key) "  Cancel    "
          (propertize "TAB"     'face 'ejira-confirm-key) "  Toggle\n"))

;;; ── Entry point ──────────────────────────────────────────────────────────────

(defun ejira-confirm-show (plans)
  "Display confirmation buffer for pending push PLANS."
  (let* ((grouped (ejira-confirm--group-by-project-issue plans))
         (counts  (let ((c nil))
                    (dolist (p plans c)
                      (let* ((op (plist-get p :op))
                             (cell (assq op c)))
                        (if cell (setcdr cell (1+ (cdr cell)))
                          (push (cons op 1) c))))))
         (buf (get-buffer-create "*ejira-confirm*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ejira-confirm-mode)
        (ejira-confirm--insert-header (length plans) counts)
        (dolist (group grouped)
          (ejira-confirm--insert-project (car group) (cdr group)))
        (ejira-confirm--insert-footer))
      (goto-char (point-min))
      (setq ejira-confirm--plans plans))
    (pop-to-buffer buf)))

;;; ── Interaction ──────────────────────────────────────────────────────────────

(defun ejira-confirm-execute ()
  "Confirm and execute all pending push operations."
  (interactive)
  (let ((plans ejira-confirm--plans))
    (quit-window t)
    ;; Bind ejira--pushing for the entire batch so that any org saves triggered
    ;; by state restoration or property writes do not re-invoke ejira--push-on-save.
    (let ((ejira--pushing t))
      (dolist (plan plans)
        (condition-case err
            (funcall (plist-get plan :send))
          (error (message "ejira: push failed for %s: %s"
                          (plist-get plan :title)
                          (error-message-string err))))))))

(defun ejira-confirm-cancel ()
  "Cancel the push and close the confirmation buffer."
  (interactive)
  (quit-window t)
  (message "ejira: push cancelled"))

(defun ejira-confirm-toggle-section ()
  "Toggle expand/collapse of the node at point."
  (interactive)
  (when-let ((node (ejira-confirm--node-at-point)))
    (let* ((ov     (plist-get node :overlay))
           (marker (plist-get node :marker))
           (hidden (overlay-get ov 'invisible))
           (inhibit-read-only t))
      (overlay-put ov 'invisible (not hidden))
      (save-excursion
        (goto-char marker)
        ;; hidden=t → was collapsed (▶), now expanding → swap to ▼
        ;; hidden=nil → was expanded (▼), now collapsing → swap to ▶
        (when (search-forward (if hidden "▶" "▼") (line-end-position) t)
          (replace-match (if hidden "▼" "▶") t t))))))

(defun ejira-confirm-next-section ()
  "Move point to the next node heading."
  (interactive)
  (let ((pos (point)) found)
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (> m pos) (or (not found) (< m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun ejira-confirm-prev-section ()
  "Move point to the previous node heading."
  (interactive)
  (let ((pos (point)) found)
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (< m pos) (or (not found) (> m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun ejira-confirm--node-at-point ()
  "Return the innermost node whose header line contains point."
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position))
        (best nil) (best-pos -1))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (>= m line-beg) (<= m line-end) (> m best-pos))
          (setq best node best-pos m))))
    best))

;;; ── Mode ─────────────────────────────────────────────────────────────────────

(defvar ejira-confirm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "C-c C-c" #'ejira-confirm-execute)
    (keymap-set map "C-c C-k" #'ejira-confirm-cancel)
    (keymap-set map "TAB"     #'ejira-confirm-toggle-section)
    (keymap-set map "<tab>"   #'ejira-confirm-toggle-section)
    (keymap-set map "n"       #'ejira-confirm-next-section)
    (keymap-set map "p"       #'ejira-confirm-prev-section)
    (keymap-set map "q"       #'ejira-confirm-cancel)
    map))

(define-derived-mode ejira-confirm-mode special-mode "Ejira-Confirm"
  "Major mode for confirming ejira push changes.\n\n\\{ejira-confirm-mode-map}"
  (setq-local revert-buffer-function #'ignore)
  (setq truncate-lines t))

(provide 'ejira-confirm)
;;; ejira-confirm.el ends here
