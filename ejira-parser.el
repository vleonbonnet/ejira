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
;; JIRA-format, ox-jira is used through the derived `ejira-jira' backend, which
;; only overrides what must round-trip (timestamps). Translation in the other
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
(require 'time-date)

(defvar ejira-parser-export-process-underscores t
  "If nil, the parser will not make underscores into anchors.")

;;; Timestamps
;;
;; JIRA wiki markup has no date element, so Org timestamps travel as
;; text in JIRA's own date format -- the one jira.mongodb.org-style
;; servers show for date fields: `08/Oct/2026', `23/Jun/2026 4:50 PM'.
;; Ranges join their ends with ` -- ', which JIRA renders as an en dash.
;; The import turns exactly this format back into inactive Org
;; timestamps (JIRA cannot tell active from inactive).  Timestamps JIRA
;; cannot express -- repeaters, warning delays, diary sexps -- travel as
;; their Org text, brackets escaped, and import back unchanged.

(defconst ejira-parser--jira-months
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
  "Month abbreviations of JIRA's date format, independent of the locale.")

(defconst ejira-parser--jira-date-re
  (concat "[0-3][0-9]/"
          (regexp-opt (append ejira-parser--jira-months nil))
          "/[0-9]\\{4\\}")
  "Regexp matching a JIRA date such as 08/Oct/2026.")

(defconst ejira-parser--jira-time-re
  "[01]?[0-9]:[0-5][0-9] [AP]M"
  "Regexp matching a JIRA time of day such as 4:50 PM.")

(defconst ejira-parser--jira-timestamp-re
  (let ((date ejira-parser--jira-date-re)
        (time ejira-parser--jira-time-re))
    (concat "\\b" date "\\(?: " time "\\)?"
            "\\(?: -- \\(?:" date "\\(?: " time "\\)?\\|" time "\\)\\)?"
            "\\b"))
  "Regexp matching a JIRA date, date and time, or range of them.")

(defun ejira-parser--jira-time (hour minute)
  "Format HOUR and MINUTE as a JIRA time of day: 4:50 PM."
  (format "%d:%02d %s"
          (let ((h (% hour 12))) (if (= h 0) 12 h))
          minute
          (if (< hour 12) "AM" "PM")))

(defun ejira-parser--jira-date (year month day &optional hour minute)
  "Format YEAR MONTH DAY, and HOUR MINUTE when given, as a JIRA date."
  (concat (format "%02d/%s/%04d" day (aref ejira-parser--jira-months (1- month)) year)
          (when hour (concat " " (ejira-parser--jira-time hour minute)))))

(defun ejira-parser--export-timestamp (timestamp _contents _info)
  "Transcode the Org TIMESTAMP object into JIRA's date format.
See the Timestamps section of ejira-parser.el."
  (let ((type (org-element-property :type timestamp)))
    (if (or (eq type 'diary)
            (org-element-property :repeater-type timestamp)
            (org-element-property :warning-type timestamp))
        ;; No JIRA equivalent: the Org text, with `[' escaped so JIRA
        ;; never reads it as a link; the import consumes the escape.
        (replace-regexp-in-string
         "\\[" "\\[" (org-element-property :raw-value timestamp) t t)
      (let* ((get (lambda (p) (org-element-property p timestamp)))
             (start (list (funcall get :year-start) (funcall get :month-start)
                          (funcall get :day-start)))
             (end (list (funcall get :year-end) (funcall get :month-end)
                        (funcall get :day-end)))
             (start-text (apply #'ejira-parser--jira-date
                                (append start
                                        (when (funcall get :hour-start)
                                          (list (funcall get :hour-start)
                                                (funcall get :minute-start)))))))
        (cond
         ((not (memq type '(active-range inactive-range)))
          start-text)
         ;; Same-day time range: 08/Oct/2026 10:00 AM -- 11:00 AM.
         ((and (equal start end) (funcall get :hour-end))
          (concat start-text " -- "
                  (ejira-parser--jira-time (funcall get :hour-end)
                                           (funcall get :minute-end))))
         (t
          (concat start-text " -- "
                  (apply #'ejira-parser--jira-date
                         (append end
                                 (when (funcall get :hour-end)
                                   (list (funcall get :hour-end)
                                         (funcall get :minute-end))))))))))))

(defun ejira-parser--parse-jira-date (s)
  "Parse the JIRA date S (08/Oct/2026) into (YEAR MONTH DAY), or nil.
Return nil for a date that does not exist, such as 31/Feb/2026."
  (when (string-match "\\`\\([0-9]+\\)/\\([A-Za-z]+\\)/\\([0-9]+\\)\\'" s)
    (let ((day (string-to-number (match-string 1 s)))
          (month (1+ (or (cl-position (match-string 2 s) ejira-parser--jira-months
                                      :test #'equal)
                         -1)))
          (year (string-to-number (match-string 3 s))))
      (when (and (>= month 1) (>= day 1)
                 (<= day (date-days-in-month year month)))
        (list year month day)))))

(defun ejira-parser--parse-jira-time (s)
  "Parse the JIRA time S (4:50 PM) into (HOUR MINUTE), or nil."
  (when (string-match "\\`\\([0-9]+\\):\\([0-9]+\\) \\([AP]M\\)\\'" s)
    (let ((hour (string-to-number (match-string 1 s)))
          (minute (string-to-number (match-string 2 s)))
          (pm (equal (match-string 3 s) "PM")))
      (when (<= 1 hour 12)
        (list (+ (% hour 12) (if pm 12 0)) minute)))))

(defun ejira-parser--org-date (date &optional time)
  "Return the Org date text for DATE (YEAR MONTH DAY) and TIME (HOUR MINUTE).
Without brackets: `2026-10-08 Thu' or `2026-10-08 Thu 16:50'."
  (pcase-let ((`(,year ,month ,day) date))
    (concat (format-time-string "%Y-%m-%d %a"
                                (encode-time (list 0 0 12 day month year nil nil nil)))
            (when time (apply #'format " %02d:%02d" time)))))

(defun ejira-parser--jira-timestamp-to-org (text)
  "Convert the JIRA date or range TEXT into an inactive Org timestamp.
Return TEXT unchanged when it names a date that does not exist."
  (let* ((halves (split-string text " -- "))
         (parse (lambda (half)
                  ;; (DATE TIME), either possibly nil.
                  (if (string-match (concat "\\`\\(" ejira-parser--jira-date-re "\\)"
                                            "\\(?: \\(.*\\)\\)?\\'")
                                    half)
                      (let ((time-text (match-string 2 half)))
                        (list (ejira-parser--parse-jira-date (match-string 1 half))
                              (and time-text (ejira-parser--parse-jira-time time-text))))
                    (list nil (ejira-parser--parse-jira-time half)))))
         (start (funcall parse (car halves)))
         (end (and (cdr halves) (funcall parse (cadr halves)))))
    (cond
     ((null (car start)) text)
     ((null end)
      (format "[%s]" (ejira-parser--org-date (car start) (cadr start))))
     ;; Same-day time range: [2026-10-08 Thu 10:00-11:00].
     ((and (null (car end)) (cadr start) (cadr end))
      (format "[%s-%02d:%02d]"
              (ejira-parser--org-date (car start) (cadr start))
              (car (cadr end)) (cadr (cadr end))))
     ((car end)
      (format "[%s]--[%s]"
              (ejira-parser--org-date (car start) (cadr start))
              (ejira-parser--org-date (car end) (cadr end))))
     (t text))))

(defun ejira-parser-inactivate-timestamps (s)
  "Return Org text S with its active timestamps made inactive.
Only the timestamps that travel as JIRA dates are changed (repeaters,
warning delays and diary sexps travel as their Org text and keep their
brackets).  JIRA cannot tell active from inactive, so a body compares
equal to its JIRA copy exactly when the two agree after this."
  (if (not (string-match-p "<[0-9]\\{4\\}-" s))
      s
    (with-temp-buffer
      (let ((tab-width 8))
        (insert s)
        (delay-mode-hooks (org-mode))
        (let (stamps)
          (org-element-map (org-element-parse-buffer) 'timestamp
            (lambda (ts)
              (when (and (memq (org-element-property :type ts) '(active active-range))
                         (not (org-element-property :repeater-type ts))
                         (not (org-element-property :warning-type ts)))
                (push ts stamps))))
          ;; Last first, so earlier positions stay valid.
          (dolist (ts stamps)
            (let* ((beg (org-element-property :begin ts))
                   (raw (org-element-property :raw-value ts))
                   (end (+ beg (length raw))))
              (goto-char beg)
              (delete-region beg end)
              (insert (replace-regexp-in-string
                       ">" "]" (replace-regexp-in-string "<" "[" raw t t) t t)))))
        (buffer-string)))))

(org-export-define-derived-backend 'ejira-jira 'jira
  :translate-alist '((timestamp . ejira-parser--export-timestamp)))

(defvar ejira-parser-failure-function #'ejira-parser--report-failure
  "Function called with the original JIRA markup when conversion fails.
A failed conversion keeps the raw markup so no content is lost; this
hook only reports the event.  Set to nil to report nothing.")

(defvar ejira-parser-signal-failures nil
  "When non-nil, a failed JIRA-to-Org conversion signals `ejira-parser-error'.
The default keeps the raw markup, which is right for comparisons and
previews.  Code that writes the result into an Org body binds this to
t: raw JIRA markup stored as Org is corruption, not a fallback -- a
list line such as `* (x) item' becomes an Org heading.")

(define-error 'ejira-parser-error "JIRA markup could not be converted to Org")

(defvar ejira-parser-browse-links-as-id nil
  "Whether to import issue browse links as `id:' links.
A JIRA link whose URL is `<jiralib2-url>/browse/KEY' becomes
`[[id:KEY][label]]', so an Org file that references issues by their
ejira heading IDs keeps doing so across a pull.  Enable it together
with an exporter that writes `id:' issue links back as browse URLs.
  nil    keep browse URLs;
  t      convert every issue key;
  known  convert only keys that have a local heading (in
         `org-id-locations'), so no dead `id:' link is created for an
         issue the Org files do not hold.")

(defun ejira-parser--issue-known-p (key)
  "Return non-nil when issue KEY has a heading known to `org-id'."
  (and (boundp 'org-id-locations)
       (hash-table-p org-id-locations)
       (gethash key org-id-locations)
       t))

(defvar ejira-parser-issue-known-function #'ejira-parser--issue-known-p
  "Predicate deciding whether an issue key has a local heading.
Used by `ejira-parser-browse-links-as-id' set to `known'.  ejira-core
replaces the default with its own lookup, which also finds headings
that a stale `org-id-locations' misses.")

(defun ejira-parser--browse-url-issue-id (url)
  "Return `id:KEY' when URL browses issue KEY on this server, else nil.
Governed by `ejira-parser-browse-links-as-id'.  Match data is
preserved: replacement functions run between the pattern search and
its `replace-match'."
  (when (and ejira-parser-browse-links-as-id
             (boundp 'jiralib2-url) jiralib2-url)
    (save-match-data
      (when (string-match (concat "\\`" (regexp-quote jiralib2-url)
                                  "/browse/\\([A-Z][A-Z0-9]+-[0-9]+\\)\\'")
                          url)
        (let ((key (match-string 1 url)))
          (when (or (not (eq ejira-parser-browse-links-as-id 'known))
                    (funcall ejira-parser-issue-known-function key))
            (concat "id:" key)))))))

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

    ;; JIRA date, date and time, or a range of them: an inactive Org
    ;; timestamp.  After inline verbatim, so dates in code spans stay
    ;; literal.  See the Timestamps section above.
    (ejira-parser--jira-timestamp-re
     . (lambda ()
         (ejira-parser--jira-timestamp-to-org (match-string 0))))

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

    ;; Link with description.  With `ejira-parser-browse-links-as-id',
    ;; a browse URL for an issue key on this server comes back as an
    ;; `id:' link, the form the org side uses for issue references.
    ("\\[\\([^][\n]*?\\)|\\([^][\n]*?\\)\\]"
     . (lambda ()
         (let ((label (match-string 1))
               (url (match-string 2)))
           (format "[[%s][%s]]"
                   (or (ejira-parser--browse-url-issue-id url) url)
                   label))))

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
  "Regular expression - replacement pairs used in parsing JIRA markup.
A pattern is a regexp string, or a variable whose value is one.")

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
        (org-export-string-as (ejira-parser--strip-include-keywords s) 'ejira-jira t)
      (org-export-string-as
       (concat "#+OPTIONS: ^:nil\n" (ejira-parser--strip-include-keywords s))
       'ejira-jira t))))

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
headings to the right by that amount.

On failure the raw markup is returned and `ejira-parser-failure-function'
is called, unless `ejira-parser-signal-failures' is non-nil, in which
case `ejira-parser-error' is signalled instead."
  (condition-case err
      (let ((backslash-replacement (random-identifier 32))
            (percent-replacement (random-identifier 32))
            (brace-open-replacement (random-identifier 32))
            (brace-close-replacement (random-identifier 32))
            (ejira-parser--list-token (random-identifier 32)))
        (with-temp-buffer
          (let ((replacements ())
                (jira-to-org--convert-level (or level 0)))

            ;; Escaped braces (\{ \}) are literal characters in JIRA, and
            ;; the exporter writes them inside inline verbatim so a value
            ;; like `a}' survives JIRA's first-`}}' span scan.  Tokenize
            ;; them first so no rule mistakes them for markup -- in
            ;; particular the verbatim rule's `}}' closer -- and restore
            ;; them as plain braces last.
            ;; Literal backslashes need to be handled separately, they mess up
            ;; other regexp patching. They get replaced with the identifier
            ;; first, and restored last.
            (insert (decode-coding-string
                     (replace-regexp-in-string
                      "\\\\" backslash-replacement
                      (replace-regexp-in-string
                       "%" percent-replacement
                       (replace-regexp-in-string
                        "\\\\}" brace-close-replacement
                        (replace-regexp-in-string
                         "\\\\{" brace-open-replacement s t t)
                        t t)))
                     'utf-8))
            (cl-loop
             for (pattern . replacement) in ejira-parser-patterns do
             (goto-char (point-min))
             (while (re-search-forward (if (symbolp pattern)
                                           (symbol-value pattern)
                                         pattern)
                                       nil t)
               (let ((identifier (random-identifier 32))
                     ;; A replacement function may search or match
                     ;; strings; the `replace-match' below needs this
                     ;; pattern's match data, whatever the function did.
                     (rep (save-match-data (funcall replacement))))
                 (replace-match identifier t t)

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
                  brace-open-replacement "{"
                  (replace-regexp-in-string
                   brace-close-replacement "}"
                   (replace-regexp-in-string
                    percent-replacement "%"
                    (replace-regexp-in-string
                     "\\\\\\([][{}]\\)" "\\1"
                     (replace-regexp-in-string
                      backslash-replacement "\\\\"
                      (buffer-string))))
                   t t)
                  t t)))
            (with-temp-buffer
              (insert restored)
              ;; Unwrap Jira's hard-wrapped prose only after the backslash
              ;; placeholders are real again, so the `\\' hard-break check
              ;; sees actual breaks.
              (ejira-parser--unwrap-paragraphs)
              (ejira-parser--normalize-heading-spacing)
              (buffer-string)))))
    (error
     (if ejira-parser-signal-failures
         (signal 'ejira-parser-error (list (error-message-string err)
                                           (substring s 0 (min 200 (length s)))))
       (when ejira-parser-failure-function
         (funcall ejira-parser-failure-function s))
       s))))

(provide 'ejira-parser)
;;; ejira-parser.el ends here
