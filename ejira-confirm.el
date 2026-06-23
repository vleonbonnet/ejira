;;; ejira-confirm.el --- Push confirmation buffer -*- lexical-binding: t -*-

;; This file is part of ejira.

;;; Commentary:

;; Provides a dedicated major mode for reviewing pending push changes
;; before sending them to the Jira API.  Modeled on orgist-confirm.el.
;;
;; Three-level tree: project > item > field
;;
;;   Ejira Push — 3 item(s): 1 new, 1 modified, 1 deleted
;;   ═══════════════════════════════════════════════════════
;;   ▼ RNDSEC  2 new, 1 modified
;;     + new subtask: My heading title
;;     ▶ ✎ RNDSEC-123  summary, assignee
;;       ── old ──
;;       ...
;;     ✗ delete comment on RNDSEC-456  ⚠ permanent
;;       "Comment body preview…"
;;
;; C-c C-c sends all items, C-c C-k aborts.  TAB toggles sections.

;;; Code:

(require 'cl-lib)

;;; Faces

(defface ejira-confirm-title
  '((t :weight bold :height 1.2))
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
  "Face for summary text."
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

;;; Buffer-local state

(defvar-local ejira-confirm--plans nil)

(defvar-local ejira-confirm--nodes nil
  "List of plists (:marker MARKER :overlay OVERLAY :level LEVEL).
LEVEL is one of: project item field.")

;;; Diff helpers

(defun ejira-confirm--normalize (s)
  "Normalize string S for comparison: nil to empty, strip CR, trim."
  (string-trim (replace-regexp-in-string "\r" "" (or s ""))))

(defun ejira-confirm-field-changes (fields)
  "Filter FIELDS down to entries whose old and new values differ.
FIELDS is a list of (NAME OLD NEW) string triples."
  (cl-remove-if (lambda (f)
                  (equal (ejira-confirm--normalize (nth 1 f))
                         (ejira-confirm--normalize (nth 2 f))))
                fields))

;;; Grouping helpers

(defun ejira-confirm--group-by-project (plans)
  "Return alist (project-key . plans-list) sorted alphabetically."
  (let (groups)
    (dolist (plan plans)
      (let* ((proj (or (plist-get plan :project) "?"))
             (cell (assoc proj groups)))
        (if cell (setcdr cell (append (cdr cell) (list plan)))
          (push (cons proj (list plan)) groups))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

(defun ejira-confirm--count-ops (plans)
  "Return alist of (op-symbol . count) for PLANS."
  (let ((counts '()))
    (dolist (plan plans)
      (let* ((op (plist-get plan :op))
             (cell (assq op counts)))
        (if cell (setcdr cell (1+ (cdr cell)))
          (push (cons op 1) counts))))
    counts))

;;; Entry point

(defun ejira-confirm-show (plans)
  "Display confirmation buffer for pending push PLANS."
  (let* ((grouped (ejira-confirm--group-by-project plans))
         (counts (ejira-confirm--count-ops plans))
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

;;; Rendering

(defun ejira-confirm--insert-header (n counts)
  "Insert buffer header for N items with op COUNTS alist."
  (let* ((parts nil)
         (n-create (alist-get 'create counts 0))
         (n-update (alist-get 'update counts 0))
         (n-delete (alist-get 'delete counts 0)))
    (when (> n-create 0) (push (format "%d new" n-create) parts))
    (when (> n-update 0) (push (format "%d modified" n-update) parts))
    (when (> n-delete 0) (push (format "%d deleted" n-delete) parts))
    (let ((line (format "Ejira Push — %d item%s%s"
                        n
                        (if (= n 1) "" "s")
                        (if parts
                            (concat ": " (string-join (nreverse parts) ", "))
                          ""))))
      (insert (propertize line 'face 'ejira-confirm-title)
              "\n"
              (make-string (length line) ?═)
              "\n\n"))))

(defun ejira-confirm--project-op-summary (plans)
  "Return a summary string of op counts for PLANS within one project."
  (let* ((counts (ejira-confirm--count-ops plans))
         (parts nil)
         (n-create (alist-get 'create counts 0))
         (n-update (alist-get 'update counts 0))
         (n-delete (alist-get 'delete counts 0)))
    (when (> n-create 0) (push (format "%d new" n-create) parts))
    (when (> n-update 0) (push (format "%d modified" n-update) parts))
    (when (> n-delete 0) (push (format "%d deleted" n-delete) parts))
    (if parts
        (concat "  " (string-join (nreverse parts) ", "))
      "")))

(defun ejira-confirm--insert-project (project-key plans)
  "Insert a collapsible project section for PROJECT-KEY with PLANS."
  (let* ((summary (ejira-confirm--project-op-summary plans))
         (header-start (point)))
    (insert (propertize "▼" 'ejira-confirm-arrow t)
            " "
            (propertize project-key 'face 'ejira-confirm-project)
            (propertize summary 'face 'ejira-confirm-summary)
            "\n")
    (let ((body-start (point)))
      (dolist (plan plans)
        (ejira-confirm--insert-item plan))
      (insert "\n")
      (let ((ov (make-overlay body-start (point))))
        (overlay-put ov 'invisible nil)
        (overlay-put ov 'ejira-confirm-node t)
        (push (list :marker (copy-marker header-start) :overlay ov :level 'project)
              ejira-confirm--nodes)))))

(defun ejira-confirm--truncate (s)
  "Flatten and truncate S for one-line display."
  (let ((flat (string-trim (replace-regexp-in-string "[\n\r]+" " " (or s "")))))
    (if (length> flat 55)
        (concat "\"" (substring flat 0 55) "…\"")
      (format "%S" flat))))

(defun ejira-confirm--insert-item (plan)
  "Insert a rendered item line for PLAN.  Dispatches on :op."
  (let ((op (plist-get plan :op))
        (title (plist-get plan :title)))
    (pcase op
      ('create
       (let* ((preview (plist-get plan :preview))
              (header-start (point)))
         (insert "  "
                 (propertize "+" 'face 'ejira-confirm-new)
                 "  "
                 (propertize title 'face 'ejira-confirm-item)
                 "\n")
         (when (and preview (> (length preview) 0))
           (let ((detail-start (point)))
             (insert "       "
                     (propertize (ejira-confirm--truncate preview)
                                 'face 'ejira-confirm-summary)
                     "\n")
             (let ((ov (make-overlay detail-start (point))))
               (overlay-put ov 'invisible t)
               (overlay-put ov 'ejira-confirm-node t)
               (push (list :marker (copy-marker header-start) :overlay ov :level 'item)
                     ejira-confirm--nodes))))))

      ('update
       (let* ((changes (plist-get plan :changes))
              (field-names (mapcar #'car changes))
              (header-start (point)))
         (insert "  "
                 (propertize "▶" 'ejira-confirm-arrow t)
                 " ✎ "
                 (propertize title 'face 'ejira-confirm-item)
                 (propertize (concat "  " (string-join field-names ", "))
                             'face 'ejira-confirm-summary)
                 "\n")
         (let ((detail-start (point)))
           (dolist (change changes)
             (ejira-confirm--insert-field
              (nth 0 change) (nth 1 change) (nth 2 change)))
           (let ((ov (make-overlay detail-start (point))))
             (overlay-put ov 'invisible t)
             (overlay-put ov 'ejira-confirm-node t)
             (push (list :marker (copy-marker header-start) :overlay ov :level 'item)
                   ejira-confirm--nodes)))))

      ('delete
       (let ((preview (plist-get plan :preview)))
         (insert "  "
                 (propertize "✗" 'face 'ejira-confirm-deleted)
                 " "
                 (propertize title 'face 'ejira-confirm-deleted)
                 "  "
                 (propertize "⚠ permanent" 'face 'ejira-confirm-warning)
                 "\n")
         (when (and preview (> (length preview) 0))
           (insert "    "
                   (propertize (ejira-confirm--truncate preview)
                               'face 'ejira-confirm-old-value)
                   "\n")))))))

(defun ejira-confirm--insert-field (name old new)
  "Insert an expandable change line for field NAME showing OLD → NEW."
  (let ((header-start (point)))
    (insert "    "
            (propertize "▶" 'ejira-confirm-arrow t)
            " "
            (propertize (format "%-14s" (concat name ":"))
                        'face 'ejira-confirm-field-name)
            (propertize (ejira-confirm--truncate old)
                        'face 'ejira-confirm-old-value)
            " → "
            (propertize (ejira-confirm--truncate new)
                        'face 'ejira-confirm-new-value)
            "\n")
    (let ((detail-start (point)))
      (insert (propertize "        ── old ──\n" 'face 'ejira-confirm-summary))
      (dolist (line (split-string (ejira-confirm--normalize old) "\n"))
        (insert "        " (propertize line 'face 'ejira-confirm-old-value) "\n"))
      (insert (propertize "        ── new ──\n" 'face 'ejira-confirm-summary))
      (dolist (line (split-string (ejira-confirm--normalize new) "\n"))
        (insert "        " (propertize line 'face 'ejira-confirm-new-value) "\n"))
      (let ((ov (make-overlay detail-start (point))))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'ejira-confirm-node t)
        (push (list :marker (copy-marker header-start) :overlay ov :level 'field)
              ejira-confirm--nodes)))))

(defun ejira-confirm--insert-footer ()
  "Insert the keybinding footer."
  (insert "\n"
          (propertize "C-c C-c" 'face 'ejira-confirm-key)
          "  Push    "
          (propertize "C-c C-k" 'face 'ejira-confirm-key)
          "  Cancel    "
          (propertize "TAB" 'face 'ejira-confirm-key)
          "  Toggle\n"))

;;; Interaction

(defun ejira-confirm-execute ()
  "Confirm and execute all pending push operations."
  (interactive)
  (let ((plans ejira-confirm--plans))
    (quit-window t)
    (dolist (plan plans)
      (condition-case err
          (funcall (plist-get plan :send))
        (error (message "ejira: push failed for %s: %s"
                        (plist-get plan :title)
                        (error-message-string err)))))))

(defun ejira-confirm-cancel ()
  "Cancel the push and close the confirmation buffer."
  (interactive)
  (quit-window t)
  (message "ejira: push cancelled"))

(defun ejira-confirm-toggle-section ()
  "Toggle expand/collapse of the node at point."
  (interactive)
  (when-let ((node (ejira-confirm--node-at-point)))
    (let* ((ov (plist-get node :overlay))
           (marker (plist-get node :marker))
           (hidden (overlay-get ov 'invisible))
           (inhibit-read-only t))
      (overlay-put ov 'invisible (not hidden))
      (save-excursion
        (goto-char marker)
        (when (search-forward (if hidden "▶" "▼") (line-end-position) t)
          (replace-match (if hidden "▼" "▶") t t))))))

(defun ejira-confirm-next-section ()
  "Move point to the next node heading."
  (interactive)
  (let ((pos (point)) (found nil))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (> m pos) (or (not found) (< m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun ejira-confirm-prev-section ()
  "Move point to the previous node heading."
  (interactive)
  (let ((pos (point)) (found nil))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (< m pos) (or (not found) (> m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun ejira-confirm--node-at-point ()
  "Return the node plist whose header line contains point, or nil.
When both an item and a field node share the line region, prefer the one
whose marker is closest to point (the field header)."
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position))
        (best nil) (best-pos -1))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (>= m line-beg) (<= m line-end) (> m best-pos))
          (setq best node best-pos m))))
    best))

;;; Mode definition

(defvar ejira-confirm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "C-c C-c" #'ejira-confirm-execute)
    (keymap-set map "C-c C-k" #'ejira-confirm-cancel)
    (keymap-set map "TAB" #'ejira-confirm-toggle-section)
    (keymap-set map "<tab>" #'ejira-confirm-toggle-section)
    (keymap-set map "n" #'ejira-confirm-next-section)
    (keymap-set map "p" #'ejira-confirm-prev-section)
    (keymap-set map "q" #'ejira-confirm-cancel)
    map)
  "Keymap for `ejira-confirm-mode'.")

(define-derived-mode ejira-confirm-mode special-mode "Ejira-Confirm"
  "Major mode for confirming ejira push changes.

\\{ejira-confirm-mode-map}"
  (setq-local revert-buffer-function #'ignore)
  (setq truncate-lines t))

(provide 'ejira-confirm)

;;; ejira-confirm.el ends here
