;;; ejira-confirm.el --- Push confirmation buffer -*- lexical-binding: t -*-

;; This file is part of ejira.

;;; Commentary:

;; Provides a dedicated major mode for reviewing pending push changes
;; before sending them to the Jira API.  Modeled on orgist-confirm.el.
;;
;;   Ejira Push — SECBUG-908 (2 changes)
;;   ════════════════════════
;;   ▶ summary:      "old title" → "new title"
;;   ▶ description:  "old text…" → "new text…"
;;
;; TAB on a field line expands the full old/new text blocks.
;; C-c C-c sends the request(s), C-c C-k aborts without sending.

;;; Code:

(require 'cl-lib)

;;; Faces

(defface ejira-confirm-title
  '((t :weight bold :height 1.2))
  "Face for the confirmation buffer title."
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

;;; Buffer-local state

(defvar-local ejira-confirm--execute-fn nil
  "Function called on confirm to send the pending requests.")

(defvar-local ejira-confirm--nodes nil
  "List of collapsible nodes.
Each node is a plist (:marker MARKER :overlay OVERLAY).")

;;; Diff computation

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

;;; Entry point

(defun ejira-confirm-show (title changes execute-fn)
  "Display confirmation buffer for pending push CHANGES.
TITLE identifies the item being pushed (e.g. an issue key).
CHANGES is a list of (FIELD-NAME OLD NEW) string triples.
EXECUTE-FN is called with no arguments when the user confirms.
It is NOT called on cancel."
  (let ((buf (get-buffer-create "*ejira-confirm*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Set mode BEFORE rendering so kill-all-local-variables
        ;; doesn't wipe out buffer-local state set during rendering.
        (ejira-confirm-mode)
        (ejira-confirm--insert-header title (length changes))
        (dolist (change changes)
          (ejira-confirm--insert-field
           (nth 0 change) (nth 1 change) (nth 2 change)))
        (ejira-confirm--insert-footer))
      (goto-char (point-min))
      (setq ejira-confirm--execute-fn execute-fn))
    (pop-to-buffer buf)))

;;; Rendering

(defun ejira-confirm--insert-header (title count)
  "Insert the buffer header for TITLE showing COUNT changes."
  (let ((line (format "Ejira Push — %s (%d change%s)"
                      title count (if (= count 1) "" "s"))))
    (insert (propertize line 'face 'ejira-confirm-title)
            "\n"
            (make-string (length line) ?═)
            "\n\n")))

(defun ejira-confirm--truncate (s)
  "Flatten and truncate S for one-line display."
  (let ((flat (string-trim (replace-regexp-in-string "[\n\r]+" " " (or s "")))))
    (if (length> flat 60)
        (concat "\"" (substring flat 0 60) "…\"")
      (format "%S" flat))))

(defun ejira-confirm--insert-field (name old new)
  "Insert an expandable change line for field NAME showing OLD → NEW."
  (let ((header-start (point)))
    (insert (propertize "▶" 'ejira-confirm-arrow t)
            " "
            (propertize (format "%-14s" (concat name ":"))
                        'face 'ejira-confirm-field-name)
            (propertize (ejira-confirm--truncate old)
                        'face 'ejira-confirm-old-value)
            " → "
            (propertize (ejira-confirm--truncate new)
                        'face 'ejira-confirm-new-value)
            "\n")
    ;; Full-text detail block (initially hidden)
    (let ((detail-start (point)))
      (insert (propertize "    ── old ──\n" 'face 'ejira-confirm-summary))
      (dolist (line (split-string (ejira-confirm--normalize old) "\n"))
        (insert "    " (propertize line 'face 'ejira-confirm-old-value) "\n"))
      (insert (propertize "    ── new ──\n" 'face 'ejira-confirm-summary))
      (dolist (line (split-string (ejira-confirm--normalize new) "\n"))
        (insert "    " (propertize line 'face 'ejira-confirm-new-value) "\n"))
      (let ((ov (make-overlay detail-start (point))))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'ejira-confirm-node t)
        (push (list :marker (copy-marker header-start) :overlay ov)
              ejira-confirm--nodes)))))

(defun ejira-confirm--insert-footer ()
  "Insert the keybinding footer."
  (insert "\n"
          (propertize "C-c C-c" 'face 'ejira-confirm-key)
          "  Push    "
          (propertize "C-c C-k" 'face 'ejira-confirm-key)
          "  Cancel    "
          (propertize "TAB" 'face 'ejira-confirm-key)
          "  Toggle details\n"))

;;; Interaction

(defun ejira-confirm-execute ()
  "Confirm and send the pending push requests."
  (interactive)
  (let ((fn ejira-confirm--execute-fn))
    (quit-window t)
    (funcall fn)))

(defun ejira-confirm-cancel ()
  "Cancel the push and close the confirmation buffer."
  (interactive)
  (quit-window t)
  (message "ejira: push cancelled"))

(defun ejira-confirm-toggle-section ()
  "Toggle expand/collapse of the field details at point."
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
  "Move point to the next field heading."
  (interactive)
  (let ((pos (point)) (found nil))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (> m pos) (or (not found) (< m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun ejira-confirm-prev-section ()
  "Move point to the previous field heading."
  (interactive)
  (let ((pos (point)) (found nil))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (< m pos) (or (not found) (> m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun ejira-confirm--node-at-point ()
  "Return the node plist for the heading at point, or nil."
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position))
        (best nil))
    (dolist (node ejira-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (>= m line-beg) (<= m line-end))
          (setq best node))))
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
