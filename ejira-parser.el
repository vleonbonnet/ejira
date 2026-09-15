;;; ejira-parser.el --- Parsing to and from JIRA markup.

;; Copyright (C) 2017 Henrik Nyman

;; Author: Henrik Nyman
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira
;; Version: 1.0
;; Package-Requires: ((org "8.3") (ox-jira) (language-detection) (s "1.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Two-directional parser for JIRA markup. For translating org-mode string to
;; JIRA-format, the ox-jira -module is used directly. Translation in the other
;; direction is regexp-based: protected regions (block macros, verbatim) are
;; replaced with unique tokens first and restored afterwards, so later rules
;; cannot rewrite literal content.

;;; Code:

(require 'org)
(require 'org-src)
(require 'ox-jira)
(require 'cl-lib)
(require 'language-detection)
(require 's)

(defvar ejira-parser-export-process-underscores t
  "If nil, the parser will not make underscores into anchors.")

(defvar ejira-parser-failure-function #'ejira-parser--report-failure
  "Function called with the original JIRA markup when conversion fails.
A failed conversion keeps the raw markup so no content is lost; this
hook only reports the event.  Set to nil to report nothing.")

(defvar ejira-parser--list-token nil
  "Random per-conversion token marking ordered-list placeholders.
Literal text can never contain it, so the numbering pass cannot be
confused by `########'-looking text in code blocks or prose.")

(defun ejira-parser--report-failure (s)
  "Report that S could not be converted from JIRA markup."
  (message "ejira-parser: conversion failed; kept raw JIRA markup: %.60s"
           (replace-regexp-in-string "\n" " " s)))

(defun ejira-parser--code-language (params)
  "Extract a code language from JIRA macro params string PARAMS.
PARAMS is the text after `{code:' up to the closing brace, e.g.
\"java\", \"language=go\" or \"title=x|language=python\".  A bare
token is accepted as a language, matching JIRA's macro shorthand.
The literal \"none\" written by the exporter means \"unknown\"."
  (when (and params (not (string-empty-p params)))
    (let (lang)
      (dolist (token (split-string params "[|,]"))
        (let ((kv (split-string token "=")))
          (cond ((and (nth 1 kv)
                      (member (downcase (nth 0 kv)) '("language" "lang")))
                 (setq lang (nth 1 kv)))
                ((and (null (nth 1 kv))
                      (not (string-empty-p (nth 0 kv))))
                 (setq lang (or lang (nth 0 kv)))))))
      (when (and lang (not (member lang '("" "none"))))
        (downcase lang)))))

(defun ejira-parser--escape-org-code (s)
  "Return S escaped for insertion into a verbatim Org block.
Lines beginning with `*' or `#+' get Org's comma escape so an
embedded block delimiter cannot terminate the generated block."
  (with-temp-buffer
    (insert s)
    (org-escape-code-in-region (point-min) (point-max))
    (buffer-string)))

(defun ejira-parser--indent (s)
  "Indent every non-empty line of S by two spaces.

Empty lines are left empty on purpose: a global `whitespace-cleanup'
save hook strips the indentation again, and the import/export
comparison would report the body as locally modified forever."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at-p "[ \t]*$")
        (insert "  "))
      (forward-line 1))
    (buffer-string)))

(defun ejira-parser--emphasis-boundary-fail-p ()
  "Return non-nil when the emphasis match at point has a word boundary.
JIRA applies `_italic_' and `+underline+' only when the delimiters are
not adjacent to alphanumeric characters; the check must not consume
the boundary character, or the next span's delimiter would be hidden
from the scan."
  (let ((before (char-before (match-beginning 0)))
        (after (char-after (match-end 0))))
    (or (and before (memq (char-syntax before) '(?w ?_)))
        (and after (memq (char-syntax after) '(?w ?_))))))

(defun ejira-parser--unwrap-paragraphs ()
  "Join wrapped prose lines in the current buffer into single lines.

JIRA hard-wraps prose at its UI width.  Org here prefers long lines,
and a global prose guard rejects accidentally wrapped paragraphs on
commit; `ox-jira' flattens a paragraph's internal newlines on export
anyway, so joining them at import keeps both directions consistent.
Lines ending with an explicit Org break (`\\\\') keep their break."
  ;; Org refuses to parse with a foreign `tab-width'; the user's global
  ;; setting (4) must not leak into the parse buffer.
  (let ((tab-width 8)
        (ranges nil))
    (delay-mode-hooks (org-mode))
    (org-element-map (org-element-parse-buffer) 'paragraph
      (lambda (p)
        (push (cons (org-element-property :begin p)
                    (org-element-property :contents-end p))
              ranges)))
    (dolist (range (sort ranges (lambda (a b) (> (car a) (car b)))))
      (save-excursion
        (goto-char (car range))
        (let* ((text (buffer-substring-no-properties (car range) (cdr range)))
               (joined text))
          ;; Replace only interior line breaks: never the region's own
          ;; trailing newline, blank lines, or a line ending in the
          ;; Org hard break `\\'.
          (while (string-match "\\([^\\\\\n]\\)\n\\([^ \t\n]\\)" joined)
            (setq joined (replace-match "\\1 \\2" t nil joined)))
          (unless (equal joined text)
            (delete-region (car range) (cdr range))
            (goto-char (car range))
            (insert joined)))))))

(defun ejira-parser--normalize-heading-spacing ()
  "Ensure one blank line before and after every Org heading line.
JIRA glues `h2.' headings to their paragraphs; the canonical Org form
shared with orgist and gdocs-mode separates headings from surrounding
content with exactly one blank line.  Only missing blanks are
inserted, so the pass is idempotent and never collapses intentional
blank runs.  Block contents are skipped: generated code and example
bodies are escaped or indented there and must keep their exact shape."
  (goto-char (point-min))
  (let ((block-depth 0)
        prev-kind)
    (while (not (eobp))
      (let ((kind (cond
                   ((looking-at "^[ \t]*#\\+BEGIN_") 'begin)
                   ((looking-at "^[ \t]*#\\+END_") 'end)
                   ((> block-depth 0) 'block)
                   ((looking-at "^\\*+ ") 'heading)
                   ((looking-at "^[ \t]*$") 'blank)
                   (t 'content))))
        ;; The line after a heading is separated by the same rule that
        ;; separates a heading from preceding content.  Insert at point
        ;; without saving it: point must stay on the current line, or
        ;; forward-line skips the inserted blank and the loop never
        ;; advances past consecutive headings.
        (when (and (eq kind 'heading)
                   (memq prev-kind '(content heading begin end)))
          (insert "\n"))
        (when (and (memq kind '(content begin end))
                   (eq prev-kind 'heading))
          (insert "\n"))
        (pcase kind
          ('begin (cl-incf block-depth))
          ('end (cl-decf block-depth)))
        (setq prev-kind kind))
    (forward-line 1))))

(defun ejira-parser--renumber-ordered-lists (token)
  "Renumber ordered-list placeholders containing TOKEN in the buffer.
Each placeholder line looks like \"<indent><TOKEN>-<level> item\".
Numbers restart per list: counters for deeper levels reset after a
shallower item, and all counters reset when a non-blank line starts a
new top-level construct.  Indented continuation lines do not reset."
  (let ((re (concat "^\\([ \t]*\\)" (regexp-quote token) "-\\([0-9]+\\)"))
        (counters nil))
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((looking-at re)
        (let* ((indent (match-string-no-properties 1))
               (level (string-to-number (match-string-no-properties 2)))
               (number (1+ (or (cdr (assq level counters)) 0))))
          ;; Deeper counters reset whenever a new item appears at this
          ;; level; parent counters stay.
          (setq counters (cl-remove-if (lambda (e) (> (car e) level)) counters))
          (push (cons level number) counters)
          (replace-match (format "%s%d." indent number) t t)))
       ((and (not (looking-at-p "[ \t]*$"))
             (looking-at-p "[^ \t]"))
        ;; A non-blank, non-indented, non-item line ends the list.
        (setq counters nil)))
      (forward-line 1))))

(defconst ejira-parser-patterns
  '(
    ;; Code block: reuse an explicit language when the macro names one,
    ;; otherwise sniff it from the body.
    ("^{code\\(?::\\([^}\n]*\\)\\)?}\n?\\(?2:[^\n]*\\(?:\n.*\\)*?\\)?\n?{code}"
     . (lambda ()
         (let* ((params (match-string 1))
                (body (match-string 2))
                (md (match-data))
                (lang (or (ejira-parser--code-language params)
                          (symbol-name (language-detection-string body))
                          "")))
           ;; Language-detection falls back to awk for plain text, in
           ;; which case it most likely is not code at all.
           (when (equal lang "awk")
             (setq lang ""))
           (when (equal lang "emacslisp")
             (setq lang "elisp"))
           (prog1
               (concat
                "#+BEGIN_SRC" (if (string-empty-p lang) "" " ") lang "\n"
                (ejira-parser--indent (ejira-parser--escape-org-code (or body "")))
                "\n#+END_SRC")

             ;; Auto-detecting language alters match data, restore it.
             (set-match-data md)))))

    ;; Noformat block: a literal container, same protection as code.
    ("^{noformat}\n?\\(?1:[^\n]*\\(?:\n.*\\)*?\\)?\n?{noformat}"
     . (lambda ()
         (let ((body (or (match-string 1) ""))
               (md (match-data)))
           (prog1
               (concat "#+BEGIN_EXAMPLE\n"
                       (ejira-parser--indent (ejira-parser--escape-org-code body))
                       "\n#+END_EXAMPLE")

             ;; Escaping alters match data, restore it.
             (set-match-data md)))))

    ;; Quote block: the body is itself JIRA markup, convert it.
    ("^{quote}\n?\\(?1:[^\n]*\\(?:\n.*\\)*?\\)?\n?{quote}"
     . (lambda ()
         (let* ((body (or (match-string 1) ""))
                (md (match-data))
                (converted (if (string-empty-p (string-trim body))
                               ""
                             (string-trim (ejira-parser-jira-to-org body)))))
           (prog1
               (concat "#+BEGIN_QUOTE\n"
                       (ejira-parser--indent converted)
                       "\n#+END_QUOTE")
             (set-match-data md)))))

    ;; Inline verbatim.  Runs before link and mention patterns so its
    ;; content is protected by a token and never converted.
    ("{{\\(?1:.*?\\)}}"
     . (lambda ()
         (let ((content (match-string 1)))
           (if (or (string-empty-p content)
                   (string-match-p "\\`[ \t]\\|[ \t]\\'" content))
               ;; Org emphasis cannot wrap surrounding whitespace; keep
               ;; the JIRA form, which is lossless and stable on export.
               (match-string 0)
             (concat "=" content "=")))))

    ;; JIRA checkbox emoticons, before the list rule can eat the marker.
    ;; ox-jira writes (/) for checked, (i) for partial, (x) for unchecked.
    ("^ ?\\([#*]+\\) \\(([/x!i?])\\|(on)\\|(off)\\|(\\*)\\) "
     . (lambda ()
         (let* ((prefixes (match-string 1))
                (level (- (length prefixes) 1))
                (indent (make-string (max 0 (* 4 level)) ? ))
                (emoticon (match-string 2)))
           (concat indent
                   (cond ((equal emoticon "(/)") "- [X] ")
                         ((equal emoticon "(i)") "- [-] ")
                         (t "- [ ] "))))))

    ;; Bullet- or numbered list.
    ;; For some reason JIRA sometimes inserts a space in front of the marker.
    ;; Numbered items become token placeholders, numbered later, after
    ;; literal regions are restored, by `ejira-parser--renumber-ordered-lists'.
    ("^ ?\\([#*]+\\) "
     . (lambda ()
         (let* ((prefixes (match-string 1))
                (level (- (length prefixes) 1))
                (indent (make-string (max 0 (* 4 level)) ? )))
           (concat indent
                   (if (s-ends-with? "#" prefixes)
                       (concat ejira-parser--list-token "-"
                               (number-to-string level))
                     "-")
                   " "))))

    ;; Heading
    ("^h\\([1-6]\\)\\. "
     . (lambda ()
         (concat
          ;; NOTE: Requires dynamic binding to be active.
          (make-string (+ (if (boundp 'jira-to-org--convert-level)
                              jira-to-org--convert-level
                            0)
                          (string-to-number (match-string 1))) ?*) " ")))

    ;; Link to a user
    ("\\[~\\([a-zA-Z_.0-9-]*\\)\\]"
     . (lambda ()
         (let* ((username (match-string 1))
                ;; Fall back to the username so an unknown user never
                ;; turns into a literal "nil" label.
                (name (or (alist-get username (ejira--get-users) nil nil 'equal)
                          username)))

           (format "[[%s/secure/ViewProfile.jspa?name=%s][%s]]"
                   jiralib2-url username name))))

    ;; Link with description
    ("\\[\\([^][\n]*?\\)|\\([^][\n]*?\\)\\]"
     . (lambda ()
         (let ((label (match-string 1))
               (url (match-string 2)))
           (format "[[%s][%s]]" url label))))

    ;; Link without description, as emitted by the exporter.
    ("\\[\\(https?://[^][\n]*?\\)\\]"
     . (lambda ()
         (format "[[%s]]" (match-string 1))))

    ;; Table
    ("\\(^||.*||\\)\\(\\(?:\n|.*|\\)*$\\)"
     . (lambda ()
         (let* ((header (match-string 1))
                (body (match-string 2))
                (md (match-data)))
           (with-temp-buffer
             (insert
              (concat
               (replace-regexp-in-string "||" "|" header)
               "\n|"
               (replace-regexp-in-string
                "" "-"
                (make-string (- (s-count-matches "||" header) 1) ?+)
                nil nil nil 1)
               "-|"
               ;; Org reads a row whose first cell starts with a dash as
               ;; a horizontal rule and discards it; a space keeps the
               ;; row a data row.
               (replace-regexp-in-string "\\(\\`\\|\n\\)\\(|-\\)" "\\1| -"
                                         (or body ""))))
             (org-table-align)

             (prog1
                 ;; Get rid of final newline that may be injected by org-table-align
                 (replace-regexp-in-string "\n$" "" (buffer-string))

               ;; org-table-align modifies match data, restore it.
               (set-match-data md))))))

    ;; Italic text.  Boundary characters are checked, not consumed, so
    ;; adjacent spans still match and word-internal underscores (a_b_c)
    ;; stay literal, as they are in JIRA.
    ("_\\(.*?\\)_"
     . (lambda ()
         (let ((content (match-string 1)))
           (if (or (ejira-parser--emphasis-boundary-fail-p)
                   (string-empty-p content)
                   (string-match-p "\\`[ \t]\\|[ \t]\\'" content))
               (match-string 0)
             (concat "/" content "/")))))

    ;; Underline text.  Same boundary and spacing checks as italic;
    ;; this keeps arithmetic like a+b or 2+3 literal.
    ("\\+\\(.+?\\)\\+"
     . (lambda ()
         (let ((content (match-string 1)))
           (if (or (ejira-parser--emphasis-boundary-fail-p)
                   (string-match-p "\\`[ \t]\\|[ \t]\\'" content))
               (match-string 0)
             (concat "_" content "_")))))

    ;; Horizontal rule.  JIRA uses four dashes, Org five or more.
    ("^----$"
     . (lambda () "-----"))

    )
  "Regular expression - replacement pairs used in parsing JIRA markup.")

(defun ejira-parser-org-to-jira (s &optional level)
  "Transform org-style string S into JIRA format.

When LEVEL is given it is the outline level of the heading that owns
S's body (the `Description' child, say).  Headings are then exported
at their JIRA levels relative to that container, so a body imported
at LEVEL round-trips to the same h1/h2/... markup; without it the
exporter renormalizes the body's own minimum heading level to h1.

`#+INCLUDE' keywords are stripped and Babel evaluation is disabled:
export must be a pure text conversion, not a way to splice local
files or run local code into a JIRA field."
  (let ((org-export-use-babel nil)
        (ox-jira-override-headline-offset
         (if (numberp level) (- level) ox-jira-override-headline-offset)))
    (if ejira-parser-export-process-underscores
        (org-export-string-as (ejira-parser--strip-include-keywords s) 'jira t)
      (org-export-string-as
       (concat "#+OPTIONS: ^:nil\n" (ejira-parser--strip-include-keywords s))
       'jira t))))

(defun ejira-parser--strip-include-keywords (s)
  "Remove #+INCLUDE keywords from S."
  (replace-regexp-in-string "^[ \t]*#\\+INCLUDE:.*\n?" "" s))

(defun random-alpha ()
  "Generate a random lowercase character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-identifier (&optional length)
  "Create a random string of length LENGTH containing only lowercase letters."
  (mapconcat (lambda (_) (random-alpha)) (make-list (or length 16) nil) ""))

(defun ejira-parser-jira-to-org (s &optional level)
  "Transform JIRA-style string S into org-style.
If LEVEL is given, shift all
headings to the right by that amount."
  (condition-case nil
      (let ((backslash-replacement (random-identifier 32))
            (percent-replacement (random-identifier 32))
            (ejira-parser--list-token (random-identifier 32)))
        (with-temp-buffer
          (let ((replacements ())
                (jira-to-org--convert-level (or level 0)))

            ;; Literal backslashes need to be handled separately, they mess up
            ;; other regexp patching. They get replaced with the identifier
            ;; first, and restored last.
            (insert (decode-coding-string
                     (replace-regexp-in-string
                      "\\\\" backslash-replacement
                      (replace-regexp-in-string
                       "%" percent-replacement s))
                     'utf-8))
            (cl-loop
             for (pattern . replacement) in ejira-parser-patterns do
             (goto-char (point-min))
             (while (re-search-forward pattern nil t)
               (let ((identifier (random-identifier 32))
                     (rep (funcall replacement)))
                 (replace-match identifier)

                 ;; Prepend to the list so that the replacements will be applied in
                 ;; reverse order.
                 (add-to-list 'replacements `(,identifier . ,rep)))))

            ;; Restore protected regions, then number ordered lists.  The
            ;; placeholders are unique tokens, so literal `########' text
            ;; inside restored code or prose cannot be renumbered.
            (mapc
             (lambda (r)
               (goto-char (point-min))
               (search-forward (car r))
               (replace-match (cdr r)))
             replacements)
            (ejira-parser--renumber-ordered-lists ejira-parser--list-token)
            ;; Normalize trailing whitespace across the whole result,
            ;; including code and quote bodies.  A global
            ;; `whitespace-cleanup' save hook strips it on the next save
            ;; anyway; emitting the cleaned form keeps imports stable and
            ;; a freshly pulled body compares equal forever.
            (delete-trailing-whitespace))

          ;; Restore backslashes before unescaping JIRA's \{ \} \[ \] forms:
          ;; the exporter writes them for literal braces and brackets, and
          ;; JIRA uses them for the same purpose.  Return this value -- the
          ;; restoration must not be computed and then discarded.
          (let ((restored
                 (replace-regexp-in-string
                  percent-replacement "%"
                  (replace-regexp-in-string
                   "\\\\\\([][{}]\\)" "\\1"
                   (replace-regexp-in-string
                    backslash-replacement "\\\\"
                    (buffer-string))))))
            (with-temp-buffer
              (insert restored)
              ;; Unwrap Jira's hard-wrapped prose only after the backslash
              ;; placeholders are real again, so the `\\' hard-break check
              ;; sees actual breaks.
              (ejira-parser--unwrap-paragraphs)
              (ejira-parser--normalize-heading-spacing)
              (buffer-string)))))
    (error
     (when ejira-parser-failure-function
       (funcall ejira-parser-failure-function s))
     s)))

(provide 'ejira-parser)
;;; ejira-parser.el ends here
