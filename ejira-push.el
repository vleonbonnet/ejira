;;; ejira-push.el --- Push pipeline for ejira -*- lexical-binding: t -*-
;;; Commentary:
;; Owns all Jira write operations for ejira.  Scans org buffers for pending
;; operations, batch-fetches remote state, builds plan plists, and shows
;; ejira-confirm.  The only place jiralib2 write functions are called is
;; inside :send thunks.
;;; Code:
(require 'ejira-core)
(require 'ejira-confirm)

(defvar ejira-push-on-save t
  "When non-nil, saving any org buffer that contains ejira-managed headings
offers to push locally-edited issues and comments through a review buffer.")

(defvar ejira--assign-new-issues t
  "Default value for the assign-self cell on new issue plans.
Individual cells are mutable `(list t)' stored on each plan's
:assign-self property, toggled interactively in the confirm buffer.")

(defvar ejira--pushing nil
  "Bound to t while a push batch is executing (inhibits re-scan on save).")

;; Defined in ejira.el; loaded after this file.  Auto-sync files route
;; their saves to the reconciliation queue instead of the review buffer.
(defvar ejira-auto-sync-files)
(declare-function ejira--auto-sync-enqueue "ejira" (file &optional review))

(defvar ejira-state-resolution-alist
  '((5 . "Done")
    (6 . "Won't Fix"))
  "Alist mapping org-todo-keyword index to the Jira resolution name used when
a transition requires a resolution field (e.g. the 'Closed' workflow step).
Index values correspond to positions in org-todo-keywords-1 (1-based).")

(defun ejira--transition-to-org-state (key org-state todo-keywords)
  "Transition Jira issue KEY to the state mapped from ORG-STATE.
TODO-KEYWORDS is the buffer's org-todo-keywords-1 list used for index lookup.
Signals an error if no matching transition is available or the API call fails."
  (unless (and (stringp org-state) (> (length org-state) 0)
               todo-keywords (boundp 'ejira-todo-states-alist))
    (error "ejira: cannot transition %s — missing state or todo-states-alist" key))
  (let* ((pos (cl-position org-state todo-keywords :test #'string=))
         (idx (when pos (1+ pos))))
    (unless idx
      (error "ejira: org state %S not found in todo-keywords for %s" org-state key))
    (let* ((target-states
            (delq nil (mapcar (lambda (e)
                                (when (= (cdr e) idx) (car e)))
                              ejira-todo-states-alist)))
           ;; Fetch transitions with field metadata to detect required fields.
           (all-transitions
            (cdadr
             (jiralib2-session-call
              (format "/rest/api/2/issue/%s/transitions?expand=transitions.fields" key))))
           (actions (mapcar (lambda (trans)
                              (cons (cdr (assoc 'id trans))
                                    (cdr (assoc 'name trans))))
                            all-transitions))
           (action (cl-find-if (lambda (a) (member (cdr a) target-states))
                               actions)))
      (unless action
        (error "ejira: no Jira transition to %S available for %s (tried: %s)"
               org-state key (mapconcat #'identity target-states ", ")))
      (let* ((action-id (car action))
             (trans-detail (cl-find-if (lambda (tr)
                                         (equal (cdr (assoc 'id tr)) action-id))
                                       all-transitions))
             (fields (cdr (assoc 'fields trans-detail)))
             (res-field (cdr (assoc 'resolution fields)))
             (resolution-required (eq (cdr (assoc 'required res-field)) t))
             (resolution-name (when resolution-required
                                (cdr (assq idx ejira-state-resolution-alist)))))
        (ejira--jira-write (format "transition %s to %s" key (cdr action))
          (if resolution-name
              (jiralib2-session-call
               (format "/rest/api/2/issue/%s/transitions" key)
               :type "POST"
               :data (json-encode `((transition . ((id . ,action-id)))
                                    (fields . ((resolution . ((name . ,resolution-name))))))))
            (jiralib2-do-action key action-id)))))))

(defun ejira--nearest-task-ancestor ()
  "Return (TYPE ID ISSUETYPE) of the nearest task ancestor of the heading at point.
Plain (non-task) sections between a TODO and its task ancestor are
skipped, so a TODO nested under a descriptive heading still belongs to
the enclosing task's hierarchy.  Returns nil when there is no task or
project ancestor."
  (save-excursion
    (catch 'result
      (while (org-up-heading-safe)
        (let ((type (org-entry-get nil "TYPE"))
              (id (org-entry-get nil "ID")))
          (cond
           ((and id
                 (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" id)
                 (member type '("ejira-issue" "ejira-story"
                                "ejira-subtask" "ejira-epic")))
            (throw 'result (list type id (org-entry-get nil "Issuetype"))))
           ((equal type "ejira-project")
            (throw 'result (list type id nil)))))))))

(defun ejira--creation-target-id (marker)
  "Return a stable identity for the new-issue heading at MARKER.
The heading's Org ID, created when missing.  Creation is resolved by
this ID at send time instead of by MARKER: sends that run before it
rewrite other headings' bodies (a finalize pull replaces the parent's
or a sibling's description), and a marker at a heading start then
collapses into the rewritten region -- observed live: a cascade wrote
each child's issue key onto its parent heading.  `ejira--record-new-issue-key'
keeps the ID as ORIG_ID and rewrites links to it."
  (org-with-point-at marker
    (or (org-entry-get nil "ID")
        (let ((id (org-id-new)))
          (org-entry-put nil "ID" id)
          (when-let ((file (buffer-file-name (buffer-base-buffer))))
            (org-id-add-location id file))
          id))))

(defun ejira--resolve-heading (marker locators)
  "Return a marker on the heading LOCATORS identify, starting in MARKER's buffer.
LOCATORS is a list of (PROPERTY . VALUE); see `ejira--locate-heading'.
A send resolves its heading again when it runs: an earlier send, or a
reload of a buffer whose file changed on disk, may have moved it.  An
issue heading refiled into another file is found by its key.  Signal an
error when the heading is gone."
  (let ((buf (marker-buffer marker)))
    (or (and buf
             (with-current-buffer buf
               (org-with-wide-buffer
                (let ((loc (car locators)))
                  (when (and loc (cdr loc) (marker-position marker))
                    (goto-char marker)
                    (when (and (ignore-errors (org-back-to-heading t) t)
                               (equal (cdr loc) (org-entry-get (point) (car loc))))
                      (point-marker)))))))
        (and buf
             (with-current-buffer buf
               (when-let ((pos (ejira--locate-heading locators)))
                 (copy-marker pos))))
        (cl-some (lambda (loc)
                   (and (equal (car loc) "ID")
                        (cdr loc)
                        (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" (cdr loc))
                        (ejira--find-heading (cdr loc))))
                 locators)
        (error "ejira: the heading %s is no longer in %s"
               (mapconcat (lambda (l) (format "%s %s" (car l) (cdr l)))
                          (cl-remove-if-not #'cdr locators) " / ")
               (if buf (buffer-name buf) "its buffer")))))

(defun ejira--creation-target (marker id)
  "Return a marker on the heading of MARKER's buffer whose ID is ID.
Signal an error when no such heading exists any more."
  (ejira--resolve-heading marker (list (cons "ID" id))))

(defun ejira--commit-creation-journal (marker)
  "Record in its file that the heading at MARKER is being created in Jira.
The `Creating' property is written before the request: when Jira
accepts the request and the response is lost, the journal blocks a
blind duplicate on the next scan.  It only does that from the file.  A
journal left in the buffer is lost when the buffer is reloaded, so when
it cannot reach the file this signals an error and nothing is created.
Return a marker on the heading."
  (let* ((id (ejira--creation-target-id marker))
         (locators (list (cons "ID" id)))
         (patch `(("Creating" . ,(format-time-string "%Y-%m-%d %H:%M:%S")))))
    (unless (ejira--commit-edit (marker-buffer marker)
                                (lambda () (ejira--apply-heading-patch locators patch)))
      (error "ejira: the heading to create (ID %s) is no longer in %s; nothing was created"
             id (buffer-name (marker-buffer marker))))
    (ejira--resolve-heading marker locators)))

(defun ejira--rewrite-id-links (old new)
  "Rewrite `id:' links to OLD into links to NEW in the current buffer.
Return non-nil when a link was rewritten."
  (let ((n 0))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward (concat "\\[\\[id:" (regexp-quote old) "\\]") nil t)
       ;; The match includes the closing bracket; keep it.
       (replace-match (concat "[[id:" new "]") t t)
       (cl-incf n)))
    (> n 0)))

(defun ejira--record-new-issue-key (new-key marker)
  "Record newly created Jira issue key NEW-KEY on MARKER's heading.

The identity reaches the file before any follow-up step (assignment,
transition, cascade, the finalize pull), and replaces the creation
journal (`Creating') in the same write.  When a later step fails, or the
file changes on disk before the buffer is saved again, the heading must
already look created.  Otherwise a retry, or the next pull, duplicates
the ticket.  A pre-existing non-key Org ID (a plain UUID) is preserved in
`ORIG_ID' first, and `id:' links pointing at it are rewritten to the new
key so existing Org links keep resolving.

Return a marker on the heading.  Signal an error when the heading is no
longer in its file: the issue exists in Jira, and the next sync imports
it under its parent."
  (let* ((buf (marker-buffer marker))
         (old (ejira--creation-target-id marker))
         (orig (unless (string-match-p "\\`[A-Z][A-Z0-9]+-[0-9]+\\'" old) old))
         (locators (list (cons "ID" new-key) (cons "ID" old)))
         (patch `(("ID" . ,new-key)
                  ,@(when orig `(("ORIG_ID" . ,orig)))
                  ("Creating"))))
    (unless (ejira--commit-edit buf (lambda () (ejira--apply-heading-patch locators patch)))
      (error "ejira: created %s, but its heading is no longer in %s; the next sync imports it"
             new-key (buffer-name buf)))
    (when (buffer-file-name buf)
      (unless (hash-table-p org-id-locations)
        (setq org-id-locations (make-hash-table :test 'equal)))
      (puthash new-key (abbreviate-file-name (buffer-file-name buf))
               org-id-locations))
    (when orig
      ;; `id:' links pointing at the old UUID would silently stop
      ;; resolving; rewrite them in the already-visited ejira buffers.
      (dolist (b (delete-dups
                  (seq-filter #'buffer-live-p
                              (delq nil
                                    (cons buf
                                          (mapcar #'find-buffer-visiting
                                                  (append (ejira--project-files)
                                                          ejira-extra-scan-files)))))))
        (ejira--commit-edit b (lambda () (ejira--rewrite-id-links orig new-key)))))
    (ejira--resolve-heading marker (list (cons "ID" new-key)))))

(defun ejira--finalize-new-issue (new-key marker orig-state todo-keywords)
  "Post-create housekeeping for a newly-created Jira issue NEW-KEY.
Tries to transition the Jira issue to ORIG-STATE before refreshing so
the push baseline is stamped with the final state.  If the transition
is unavailable, force-sets the org state locally and leaves the heading
dirty so the next save retries the state push.  The issue key itself
must already be recorded (via `ejira--record-new-issue-key')."
  (condition-case err
      (ejira--transition-to-org-state new-key orig-state todo-keywords)
    (error (display-warning 'ejira (format "transition skipped for new issue %s: %s"
                                           new-key (error-message-string err))
                            :warning)))
  (ejira--update-task-or-hold new-key)
  (when (and (stringp orig-state) (> (length orig-state) 0))
    (let* ((m (ejira--find-heading new-key))
           (cur (when m (org-with-point-at m
                          (substring-no-properties (or (org-get-todo-state) ""))))))
      (when (and m (not (equal cur orig-state)))
        ;; Force local state but do NOT re-baseline: the Pushhash from
        ;; ejira--update-task reflects the Jira state, so the heading stays
        ;; dirty and the next save will push the state transition.
        (org-with-point-at m (org-todo orig-state))))))

(defun ejira--finalize-new-issue-review-safe (new-key marker orig-state todo-kws
                                                      reviewed-summary reviewed-body)
  "Like `ejira--finalize-new-issue', then restore post-review local edits.

The reviewed fields are what creation sent; edits made while the
confirmation was open were never sent, and the finalize pull must not
swallow them.  Restoring them locally leaves the finalize-stamped
baseline mismatched, so the heading stays dirty for the next reviewed
push.

The capture happens BEFORE finalizing and is guarded: finalize can
move the heading (refile enforces the Jira hierarchy), leaving MARKER
on an unrelated position.  A marker no longer sitting on the heading
carrying NEW-KEY is stale; the restore is skipped rather than copying
a neighbor's content onto the issue.  The restore itself resolves the
heading by NEW-KEY."
  (let (edited-summary edited-body)
    (org-with-point-at marker
      (if (equal (org-entry-get nil "ID") new-key)
          (setq edited-summary (ejira--jira-summary)
                edited-body (or (ejira--prepare-new-issue-description) ""))
        (message "ejira: %s — creation marker went stale; skipping post-review edit restore" new-key)))
    (ejira--finalize-new-issue new-key marker orig-state todo-kws)
    (when edited-summary
      (when (not (equal (ejira--push-normalize edited-summary)
                        (ejira--push-normalize reviewed-summary)))
        (ejira--set-summary new-key edited-summary))
      (when (not (equal (ejira--push-normalize edited-body)
                        (ejira--push-normalize reviewed-body)))
        (condition-case nil
            (ejira--set-jira-description-jira-markup
             new-key (ejira-parser-org-to-jira edited-body))
          (ejira-parser-error
           ;; The round trip cannot be converted back; never store raw
           ;; markup.  Hand the unsent edit to the user instead.
           (kill-new edited-body)
           (display-warning
            'ejira
            (format "%s: edits made during review could not be restored; they are on the kill ring"
                    new-key)
            :warning)))))))

(defun ejira--push-scan-issue-children (parent-marker project-key)
  "Return (CHILDREN . BLOCKED-OPS) for the new issue heading at PARENT-MARKER.
CHILDREN are direct-child plists (:marker :id :title :state :body
:priority-id) captured for cascade creation (see
`ejira--creation-target-id' for :id); the priority comes from
the child's Org cookie within PROJECT-KEY's policy.  BLOCKED-OPS are ops for TODOs deeper than the
direct children: the native Jira hierarchy has no place for them, so
they are reported instead of being silently flattened or lost.  The
child bodies are prepared (moved into description position) here, at
plan-build time, so the reviewed fields are exactly what creation sends."
  (let (children blocked)
    (org-with-wide-buffer
     (save-excursion
       (goto-char parent-marker)
       (let* ((parent-level (org-current-level))
              ;; A marker: capturing a child can insert text (its
              ;; creation ID, a moved description), which would leave an
              ;; integer bound short of the later children.
              (end (save-excursion (org-end-of-subtree t) (point-marker))))
         (while (and (outline-next-heading) (< (point) end))
           (let* ((lvl (org-current-level))
                  (type (org-entry-get nil "TYPE"))
                  (todo-state (org-get-todo-state))
                  (heading (org-get-heading t t t t)))
             (if (= lvl (1+ parent-level))
                 (when (and todo-state
                            (not type)
                            (not (org-in-commented-heading-p))
                            (not (equal heading ejira-description-heading-name))
                            (not (equal heading ejira-comments-heading-name)))
                   (push (list :marker (point-marker)
                               :id     (ejira--creation-target-id (point-marker))
                               :title  (ejira--jira-summary)
                               :state  (substring-no-properties (or todo-state ""))
                               :body   (ejira--prepare-new-issue-description)
                               :priority-id (ejira--new-issue-priority-id
                                             (point-marker) project-key))
                         children))
               (when (and (> lvl (1+ parent-level))
                          todo-state
                          (not type)
                          (not (org-in-commented-heading-p)))
                 (push (list :op 'blocked
                             :marker (point-marker)
                             :title heading
                             :reason "the Jira hierarchy cannot create below a new child issue; create it in Jira after the parent exists")
                       blocked))))))))
    (cons (nreverse children) (nreverse blocked))))

(defun ejira--push-create-cascaded-child (parent-key parent-issuetype project-key child todo-keywords assign-self)
  "Create CHILD under PARENT-KEY following the native hierarchy policy.
PARENT-ISSUETYPE is the Jira issue type of PARENT-KEY; the child's type
and relationship follow it: under an Initiative another Epic, under an
Epic a Task/Story with an Epic Link, otherwise a Sub-task.  CHILD is a
plist with :marker :title :state :body (the body already prepared at
scan time).  TODO-KEYWORDS is the org-todo-keywords-1 list for
state-transition lookup; ASSIGN-SELF is the value (t/nil) of the
parent's assign-self cell."
  (let* ((child-marker (if (plist-get child :id)
                           (ejira--creation-target (plist-get child :marker)
                                                   (plist-get child :id))
                         (plist-get child :marker)))
         (orig-state   (plist-get child :state))
         (summary      (plist-get child :title))
         (description  (plist-get child :body))
         (child-type
          (cond
           ((member parent-issuetype ejira-epic-parent-issuetypes)
            ejira-epic-type-name)
           ((equal parent-issuetype ejira-epic-type-name)
            ejira-epic-child-type-name)
           (t ejira-subtask-type-name)))
         (extra-fields
          (cond
           ((member parent-issuetype ejira-epic-parent-issuetypes)
            (unless ejira-parent-link-field
              (display-warning
               'ejira
               (format "ejira-parent-link-field is nil — new Epic %s is not linked to Initiative %s"
                       summary parent-key)
               :warning))
            (delq nil
                  (list (when (and ejira-epic-summary-field)
                          `(,ejira-epic-summary-field . ,summary))
                        (when ejira-parent-link-field
                          `(,ejira-parent-link-field . ,parent-key)))))
           ((equal parent-issuetype ejira-epic-type-name)
            (when ejira-epic-field
              (list `(,ejira-epic-field . ,parent-key))))
           (t nil)))
         (priority-id (or (plist-get child :priority-id)
                          (ejira--default-priority-id project-key parent-key)))
         (result (progn
                   ;; Journal the attempt before the request, mirroring
                   ;; the standalone create path: a cascade child whose
                   ;; request reaches Jira but whose response is lost
                   ;; must look created, or the next save duplicates the
                   ;; ticket.
                   (setq child-marker (ejira--commit-creation-journal child-marker))
                   (ejira--jira-write (format "create %s under %s" child-type parent-key)
                     (apply #'jiralib2-create-issue
                            project-key child-type
                            summary
                            (ejira-parser-org-to-jira description)
                            (delq nil
                                  (append extra-fields
                                          (list
                                           (when (equal child-type
                                                        ejira-subtask-type-name)
                                             `(parent . ((key . ,parent-key))))
                                           (when priority-id
                                             `(priority . ((id . ,priority-id)))))))))))
         (new-key (ejira--alist-get result 'key)))
    (setq child-marker (ejira--record-new-issue-key new-key child-marker))
    (when assign-self
      (let ((my-name (cdr (assoc 'name (jiralib2-get-user-info)))))
        (when my-name
          (ejira--jira-write (format "assign %s" new-key)
            (jiralib2-assign-issue new-key my-name)))))
    (ejira--finalize-new-issue new-key child-marker orig-state todo-keywords)
    (ejira--commit-issue new-key)))

(defun ejira--commit-issue (key)
  "Make the sync metadata of issue KEY's heading durable in its file.
Called once a send has finished changing the heading (see
`ejira--commit-heading').  Return nil when the heading is gone."
  (when-let ((m (ejira--find-heading key)))
    (ejira--commit-heading m)))

(defun ejira--clear-pending (marker key property)
  "Remove the staged PROPERTY of issue KEY once Jira has applied it.
MARKER is where the heading was at plan time.  The removal reaches the
file with the rest of the heading's metadata: a staged change left
there would be sent again by the next review."
  (ejira--refresh-buffers (cons (marker-buffer marker) (ejira--tracked-buffers)))
  (let ((m (ejira--resolve-heading marker (list (cons "ID" key)))))
    (org-with-point-at m
      (org-delete-property property))
    (ejira--commit-heading m)))

(defun ejira--push-priority-label (priority-id reference-key)
  "Return the review label of new-issue PRIORITY-ID, or nil without one.
The name comes from the cached priority scheme of REFERENCE-KEY: building
the plan never calls the server for it.  An uncached scheme shows the id."
  (when priority-id
    (let* ((scheme (when (and reference-key
                              (hash-table-p ejira--priority-scheme-cache))
                     (gethash (ejira--priority-scheme-cache-key reference-key)
                              ejira--priority-scheme-cache)))
           (entry (ejira--priority-entry-by-id scheme priority-id)))
      (or (plist-get entry :name)
          (format "id %s" priority-id)))))

(defun ejira--push-finalize (marker &optional reviewed-hash remote-identity
                                    priority)
  "Refresh MARKER's push baselines after a successful push; make them durable.

With REVIEWED-HASH, re-baseline only when the heading still matches the
state that was reviewed and sent.  Content edited after the review (in
the buffer while the confirmation was open, or in the file by another
writer) was never sent, so the heading stays dirty for the next review.
With REMOTE-IDENTITY, record the remote field state the push left in
Jira, for three-way reconciliation.  It is recorded even when the
heading changed after the review, because it describes Jira, not the
heading.  Without it, the next cycle would mistake the push for a
remote change and hold the issue as changed on both sides.  PRIORITY,
a plist (:id :name :rank) of the priority Jira now holds, is recorded
the same way."
  ;; The push waited on Jira: edit the current version of the file.
  (let ((locators (org-with-point-at marker (ejira--heading-locators))))
    (ejira--refresh-buffers (cons (marker-buffer marker) (ejira--tracked-buffers)))
    (setq marker (ejira--resolve-heading marker locators)))
  (org-with-point-at marker
    ;; Compared before recording the priority: the priority identity is
    ;; one of the hashed content fields.
    (let ((changed (and reviewed-hash
                        (not (equal reviewed-hash
                                    (md5 (ejira--heading-reviewed-hash)))))))
      (when remote-identity
        (org-set-property ejira-remote-hash-property
                          (md5 remote-identity)))
      (when priority
        (ejira--record-priority-identity priority))
      (if changed
          (message "ejira: %s changed since review; keeping it dirty"
                   (or (org-entry-get nil "ID") "<heading>"))
        (ejira--update-push-baseline))))
  (let ((ejira--pushing t))
    (ejira--commit-heading marker)))

(defun ejira--buffer-has-pushable-p ()
  "Return non-nil if the current buffer contains any ejira-managed heading."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^[ \t]*:Pushhash:" nil t))))

(defun ejira--push-scan-buffer (buf)
  "Return list of pending-op plists for BUF."
  (with-current-buffer buf
    (org-with-wide-buffer
     (save-excursion
       (let ((ops nil)
             (skip-until nil))
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (when (and skip-until (< (point) skip-until))
             ;; Inside a captured new-issue subtree: the cascade owns it,
             ;; and deeper TODOs were collected as blocked ops.  Scanning
             ;; it again would create the children twice - once standalone
             ;; under the grandparent and once through the parent's cascade.
             (goto-char skip-until))
           (let* ((type (org-entry-get nil "TYPE"))
                  (id (org-entry-get nil "ID"))
                  (pending-delete (org-entry-get nil "PendingDelete"))
                  (pending-transition (org-entry-get nil "PendingTransition"))
                  (pending-issuetype (org-entry-get nil "PendingIssuetype"))
                  (pending-epic (org-entry-get nil "PendingEpic"))
                  (todo-state (org-get-todo-state))
                  (heading-title (org-get-heading t t t t))
                  (marker (point-marker)))
             ;; Skip headings marked with the org COMMENT keyword (and any
             ;; of their descendants, since COMMENT is inherited) — these
             ;; are explicitly excluded from export/agenda by the user and
             ;; must never be detected as new issues or pushed to Jira.
             (if (org-in-commented-heading-p)
                 nil
               ;; Rule H: PendingDelete (check first)
               (if (and (equal pending-delete "t") (equal type "ejira-comment"))
                   (push (list :op 'delete
                               :object 'comment
                               :key (org-entry-get nil "CommId")
                               :project (when-let ((ik (org-entry-get nil "ID" t))) (car (split-string ik "-")))
                               :parent-issue (org-entry-get nil "ID" t)
                               :marker marker
                               :data (list :issue-key (org-entry-get nil "ID" t)
                                           :commid (org-entry-get nil "CommId")
                                           :body (ejira--get-heading-body marker)))
                         ops)
                 ;; Rules A-G (only when NOT PendingDelete)
                 ;; Rule A: Dirty Pushhash — issues
                 (when (and (member type ejira-pushable-types)
                            id
                            (not (equal type "ejira-comment"))
                            (ejira--locally-modified-p)
                            (not pending-delete))
                   (let ((proj (car (split-string id "-"))))
                     (push (list :op 'update
                                 :object 'issue
                                 :key id
                                 :project proj
                                 :parent-issue id
                                 :marker marker
                                 :data nil)
                           ops)))
                 ;; Rule A2: Dirty Pushhash — existing comments (have CommId)
                 (when (and (equal type "ejira-comment")
                            (org-entry-get nil "CommId")
                            (ejira--locally-modified-p)
                            (not pending-delete))
                   (let* ((commid (org-entry-get nil "CommId"))
                          (issue-key (org-entry-get nil "ID" t))
                          (proj (car (split-string issue-key "-"))))
                     (push (list :op 'update
                                 :object 'comment
                                 :key commid
                                 :project proj
                                 :parent-issue issue-key
                                 :marker marker
                                 :data (list :issue-key issue-key
                                             :commid commid))
                           ops)))
                 ;; Rule B: PendingTransition
                 (when (and pending-transition (member type ejira-pushable-types))
                   (push (list :op 'update
                               :object 'status
                               :key id
                               :project (car (split-string id "-"))
                               :parent-issue id
                               :marker marker
                               :data (list :action-name pending-transition))
                         ops))
                 ;; Rule C: PendingIssuetype
                 (when pending-issuetype
                   (push (list :op 'update
                               :object 'issuetype
                               :key id
                               :project (car (split-string id "-"))
                               :parent-issue id
                               :marker marker
                               :data (list :new-type pending-issuetype
                                           :old-type (org-entry-get nil "Issuetype")))
                         ops))
                 ;; Rule D: PendingEpic
                 (when pending-epic
                   (push (list :op 'update
                               :object 'epic
                               :key id
                               :project (car (split-string id "-"))
                               :parent-issue id
                               :marker marker
                               :data (list :new-epic pending-epic))
                         ops))
                 ;; Rule E/F: New heading without TYPE.  The parent is the
                 ;; NEAREST task ancestor: plain sections between the TODO
                 ;; and its task do not interrupt the hierarchy.
                 (when (and todo-state
                            (not type)
                            (not (equal heading-title ejira-description-heading-name))
                            (not (equal heading-title ejira-comments-heading-name)))
                   (cond
                    ;; A previous creation attempt never completed: its
                    ;; outcome is unknown (the request may have reached
                    ;; Jira before the response was lost).  Never re-create
                    ;; blindly; resolve by hand.
                    ((org-entry-get nil "Creating")
                     (push (list :op 'blocked
                                 :marker marker
                                 :title heading-title
                                 :reason "previous creation attempt has unknown outcome; check Jira, then either set the heading's ID or remove the Creating property")
                           ops))
                    (t
                     (let* ((parent-info (ejira--nearest-task-ancestor))
                            (parent-type       (nth 0 parent-info))
                            (parent-id         (nth 1 parent-info))
                            (parent-issuetype  (nth 2 parent-info))
                            (project-key       (when parent-id
                                                 (car (split-string parent-id "-")))))
                       (cond
                        ((null parent-info)
                         (push (list :op 'blocked
                                     :marker marker
                                     :title heading-title
                                     :local-only t
                                     :reason "no project or task ancestor; cannot determine the project")
                               ops))
                        ;; Native hierarchy only: a subtask cannot have
                        ;; children, so the branch is blocked rather than
                        ;; flattened or silently skipped.
                        ((equal parent-type "ejira-subtask")
                         ;; It can never become a Jira issue: a local task,
                         ;; reported by the review-time scan but not held.
                         (push (list :op 'blocked
                                     :marker marker
                                     :title heading-title
                                     :local-only t
                                     :reason (format "%s is a subtask and Jira subtasks cannot have children; restructure the outline" parent-id))
                               ops))
                        ;; Under Initiative (or other epic-parent type) →
                        ;; create Epic.  The hierarchy fields must be
                        ;; configured: creating without the link would
                        ;; orphan the issue instead of blocking loudly.
                        ((and (equal parent-type "ejira-issue")
                              (member parent-issuetype ejira-epic-parent-issuetypes))
                         (if (not ejira-parent-link-field)
                             (push (list :op 'blocked
                                         :marker marker
                                         :title heading-title
                                         :reason (format "cannot create an Epic under %s: ejira-parent-link-field is not configured; set it to the Portfolio Parent Link field id" parent-id))
                                   ops)
                           (let ((kids (ejira--push-scan-issue-children marker project-key)))
                             (push (list :op 'create
                                         :object 'issue
                                         :key nil
                                         :project project-key
                                         :parent-issue parent-id
                                         :marker marker
                                         :data (list :project-key project-key
                                                     :issue-type ejira-epic-type-name
                                                     :parent-initiative parent-id
                                                     :children (car kids)))
                                   ops)
                             (setq ops (append (cdr kids) ops))
                             (setq skip-until
                                   (max (or skip-until 0)
                                        (save-excursion (ejira--true-subtree-end)))))))
                        ;; Under Epic → create Task (or Story) with Epic Link
                        ((equal parent-type "ejira-epic")
                         (if (not ejira-epic-field)
                             (push (list :op 'blocked
                                         :marker marker
                                         :title heading-title
                                         :reason (format "cannot create an issue under Epic %s: ejira-epic-field is not configured; set it to the Epic Link field id" parent-id))
                                   ops)
                           (let ((kids (ejira--push-scan-issue-children marker project-key)))
                             (push (list :op 'create
                                         :object 'issue
                                         :key nil
                                         :project project-key
                                         :parent-issue parent-id
                                         :marker marker
                                         :data (list :project-key project-key
                                                     :issue-type ejira-epic-child-type-name
                                                     :parent-epic parent-id
                                                     :children (car kids)))
                                   ops)
                             (setq ops (append (cdr kids) ops))
                             (setq skip-until
                                   (max (or skip-until 0)
                                        (save-excursion (ejira--true-subtree-end)))))))
                        ;; Under Issue/Story → create Sub-task (Jira parent
                        ;; link).  A new subtask cannot own children:
                        ;; capture the subtree, block every TODO in it,
                        ;; and skip past it.
                        ((member parent-type '("ejira-issue" "ejira-story"))
                         (let ((kids (ejira--push-scan-issue-children marker project-key)))
                           (push (list :op 'create
                                       :object 'subtask
                                       :key nil
                                       :project project-key
                                       :parent-issue parent-id
                                       :marker marker
                                       :data (list :parent-key parent-id
                                                   :project-key project-key))
                                 ops)
                           (setq ops
                                 (append
                                  (cdr kids)
                                  (mapcar
                                   (lambda (child)
                                     (list :op 'blocked
                                           :marker (plist-get child :marker)
                                           :title (plist-get child :title)
                                           :reason (format "%s becomes a subtask and Jira subtasks cannot have children; restructure the outline" parent-id)))
                                   (car kids))
                                  ops))
                           (setq skip-until
                                 (max (or skip-until 0)
                                      (save-excursion (ejira--true-subtree-end))))))
                        ;; Under Project → create issue (top-level)
                        ((equal parent-type "ejira-project")
                         (let ((kids (ejira--push-scan-issue-children marker project-key)))
                           (push (list :op 'create
                                       :object 'issue
                                       :key nil
                                       :project parent-id
                                       :parent-issue nil
                                       :marker marker
                                       :data (list :project-key parent-id
                                                   :children (car kids)))
                                 ops)
                           (setq ops (append (cdr kids) ops))
                           (setq skip-until
                                 (max (or skip-until 0)
                                      (save-excursion (ejira--true-subtree-end)))))))))))
                 ;; Rule G: New comment draft — heading directly under Comments,
                 ;; no CommId yet.  Catches manually-added plain headings and
                 ;; org-capture stubs (TYPE=ejira-comment, no CommId).
                 (when (and (not (org-entry-get nil "CommId"))
                            (or (not type) (equal type "ejira-comment"))
                            (not todo-state))
                   (let* ((parent-title
                           (save-excursion
                             (when (org-up-heading-safe)
                               (org-get-heading t t t t))))
                          (issue-key
                           (when (equal parent-title ejira-comments-heading-name)
                             (save-excursion
                               (org-up-heading-safe)
                               (when (org-up-heading-safe)
                                 (org-entry-get nil "ID")))))
                          (proj (when issue-key (car (split-string issue-key "-")))))
                     (when issue-key
                       (push (if (org-entry-get nil "Creating")
                                 ;; As for issues: the earlier attempt may
                                 ;; have reached Jira; never post blindly.
                                 (list :op 'blocked
                                       :marker marker
                                       :title heading-title
                                       :reason "previous comment creation attempt has unknown outcome; check Jira, then either set the heading's CommId or remove the Creating property")
                               (list :op 'create
                                     :object 'comment
                                     :key nil
                                     :project proj
                                     :parent-issue issue-key
                                     :marker marker
                                     :data (list :issue-key issue-key)))
                             ops))))))
             (goto-char (line-end-position))))
         ;; Only projects ejira syncs: a heading of another project is
         ;; never pushed, wherever it lives.  Blocked ops carry no project
         ;; and are kept for reporting.
         (cl-remove-if (lambda (op)
                         (and (not (eq (plist-get op :op) 'blocked))
                              (not (ejira--synced-project-p (plist-get op :project)))))
                       (nreverse ops)))))))

(defun ejira--push-scan-all ()
  "Scan all ejira-managed org files for pending operations."
  (let ((ejira-dir (file-truename (expand-file-name ejira-org-directory))))
    (cl-mapcan
     (lambda (f)
       (when (and (file-exists-p f)
                  (string-prefix-p ejira-dir (file-truename f)))
         (ejira--push-scan-buffer (find-file-noselect f t))))
     (org-agenda-files))))

(defun ejira--push-build-plans (pending-ops)
  "Build plan plists from PENDING-OPS."
  (let ((plans nil)
        (issue-update-ops nil)
        (other-ops nil))
    (dolist (op pending-ops)
      (if (and (eq (plist-get op :op) 'update)
               (eq (plist-get op :object) 'issue))
          (push op issue-update-ops)
        (push op other-ops)))
    (when issue-update-ops
      (let* ((keys (mapcar (lambda (op) (plist-get op :key)) issue-update-ops))
             (remote-items
              (apply #'jiralib2-jql-search
                     (format "key in (%s)" (s-join ", " keys))
                     ;; `comment' feeds the remote identity stored on success.
                     '("summary" "description" "assignee" "priority" "duedate" "status"
                       "comment"))))
        (dolist (op issue-update-ops)
          (let* ((key (plist-get op :key))
                 (marker (plist-get op :marker))
                 (project (plist-get op :project))
                 (item (cl-find-if (lambda (i)
                                     (equal (ejira--alist-get i 'key) key))
                                   remote-items))
                 (local-summary (org-with-point-at marker (ejira--jira-summary)))
                 (local-desc-org (or (org-with-point-at marker
                                       (ejira--jira-description))
                                     ""))
                 ;; Outline level of the heading holding the Jira-facing
                 ;; description.  Exporting relative to it keeps the original
                 ;; h1/h2/... levels when an edited description is pushed back.
                 ;; In body-as-description mode the task heading itself is
                 ;; the container.
                 (desc-level (org-with-point-at marker
                               (if (ejira--description-in-body-p)
                                   (org-current-level)
                                 (when-let ((d (ejira--find-child-heading
                                                (if (ejira--jira-projection-p)
                                                    ejira-jira-description-heading-name
                                                  ejira-description-heading-name))))
                                   (ejira--heading-body-level d)))))
                 (local-assignee (or (org-entry-get marker "Assignee") ""))
                 (priority-scheme (when item (ejira--get-priority-scheme key)))
                 (local-priority-entry
                  (ejira--local-priority-entry marker priority-scheme project))
                 (local-priority-id (plist-get local-priority-entry :id))
                 (local-priority-name (plist-get local-priority-entry :name))
                 ;; The priority Jira holds after a successful push (sent
                 ;; or already equal), recorded on the heading by
                 ;; finalization so a pushed cookie edit stops looking dirty.
                 (local-priority
                  (when local-priority-id
                    (list :id local-priority-id
                          :name local-priority-name
                          :rank (ejira--priority-rank priority-scheme
                                                      local-priority-id
                                                      project))))
                 (local-deadline (when-let ((d (org-get-deadline-time marker)))
                                   (format-time-string "%Y-%m-%d" d)))
                 (remote-summary (when item (ejira--alist-get item 'fields 'summary)))
                 (remote-desc-org (when item
                                    (ejira--expected-jira-description
                                     marker
                                     (ejira--alist-get item 'fields 'description))) )
                 (remote-assignee (or (when item
                                        (ejira--alist-get item 'fields 'assignee 'displayName))
                                      ""))
                 (remote-priority-id (when item
                                       (ejira--priority-id-string
                                        (ejira--alist-get item 'fields 'priority 'id))))
                 (remote-priority-name (when item
                                         (ejira--alist-get item 'fields 'priority 'name)))
                 (remote-deadline (when item (ejira--alist-get item 'fields 'duedate)))
                 (remote-status-name (when item (ejira--alist-get item 'fields 'status 'name)))
                 (local-state (org-with-point-at marker
                                (substring-no-properties (or (org-get-todo-state) ""))))
                 (todo-kws (org-with-point-at marker org-todo-keywords-1))
                 (local-state-jira-names
                  (when (and local-state todo-kws (boundp 'ejira-todo-states-alist))
                    (let* ((pos (cl-position local-state todo-kws :test #'string=))
                           (idx (when pos (1+ pos))))
                      (when idx
                        (delq nil (mapcar (lambda (e) (when (= (cdr e) idx) (car e)))
                                          ejira-todo-states-alist))))))
                 (state-matches (member remote-status-name local-state-jira-names))
                 ;; A legacy heading has no exact Jira priority identity and
                 ;; is intentionally ignored here.  It will be migrated on a
                 ;; clean pull, or after its current dirty push succeeds.
                 (priority-changed
                  (and local-priority-id
                       (not (equal local-priority-id remote-priority-id))))
                 (changes (let ((base-changes
                                 (ejira-confirm-field-changes
                                  `(("summary"     ,remote-summary ,local-summary)
                                    ("description" ,(ejira--remote-body-for-diff
                                                     (or remote-desc-org "") local-desc-org)
                                     ,local-desc-org)
                                    ("assignee"    ,remote-assignee ,local-assignee)
                                    ("deadline"    ,(or remote-deadline "") ,(or local-deadline ""))
                                    ,@(when (and local-state remote-status-name
                                                 (not state-matches))
                                        `(("state" ,(or remote-status-name "") ,local-state)))))))
                            (if priority-changed
                                (cons (list "priority"
                                            (or remote-priority-name remote-priority-id "")
                                            (or local-priority-name local-priority-id ""))
                                      base-changes)
                              base-changes)))
                 (summary-changed (assoc "summary" changes))
                 (desc-changed (assoc "description" changes))
                 (assignee-changed (assoc "assignee" changes))
                 (deadline-changed (assoc "deadline" changes))
                 (state-changed (assoc "state" changes))
                 ;; Hash of the reviewed local state.  Finalization
                 ;; re-baselines only when the heading still matches: edits
                 ;; made while the confirmation was open were never sent and
                 ;; must stay dirty for the next save.
                 (reviewed-hash (org-with-point-at marker
                                  (md5 (ejira--heading-reviewed-hash))))
                 ;; Whether the remote fields changed since their last
                 ;; acknowledged state.  A missing baseline is unknown:
                 ;; treated as changed for automatic pulls, but not as a
                 ;; conflict.
                 (remote-changed-p (org-with-point-at marker
                                     (ejira--remote-changed-p item)))
                 ;; Identity of the remote values as they will be after this
                 ;; plan's send.  Stored as the remote baseline on success.
                 (sent-identity
                  (concat
                   (ejira--push-normalize
                    (if summary-changed local-summary
                      (or remote-summary local-summary "")))
                   "\0"
                   (ejira--push-normalize
                    (if (or summary-changed desc-changed)
                        (ejira-parser-org-to-jira local-desc-org desc-level)
                      (or (when item (ejira--alist-get item 'fields 'description))
                          "")))
                   "\0"
                   (ejira--push-normalize
                    (or (if (and priority-changed local-priority-id)
                            local-priority-id remote-priority-id)
                        ""))
                   "\0"
                   (ejira--push-normalize
                    (or (if deadline-changed local-deadline remote-deadline)
                        ""))
                   "\0"
                   (ejira--push-normalize
                    (if state-changed ejira-remote-identity-unknown
                      (or remote-status-name "")))
                   "\0"
                   (ejira--push-normalize
                    (if assignee-changed local-assignee
                      (or remote-assignee "")))
                   ;; An issue update leaves the comments as fetched.
                   "\0"
                   (if item (ejira--remote-comments-identity item) ""))))
            (if changes
                (push (list :op 'update
                            :object 'issue
                            :project project
                            :title key
                            :parent-issue key
                            :changes changes
                            :remote-changed remote-changed-p
                            :send (let ((key key) (marker marker)
                                        (local-summary local-summary)
                                        (local-desc-org local-desc-org)
                                        (desc-level desc-level)
                                        (local-assignee local-assignee)
                                        (local-priority-id local-priority-id)
                                        (local-priority local-priority)
                                        (local-deadline local-deadline)
                                        (local-state local-state)
                                        (todo-kws todo-kws)
                                        (reviewed-hash reviewed-hash)
                                        (sent-identity sent-identity)
                                        (summary-changed summary-changed)
                                        (desc-changed desc-changed)
                                        (assignee-changed assignee-changed)
                                        (priority-changed priority-changed)
                                        (deadline-changed deadline-changed)
                                        (state-changed state-changed))
                                    (lambda ()
                                      (when (or summary-changed desc-changed)
                                        (ejira--jira-write (format "update %s summary/description" key)
                                          (jiralib2-update-summary-description
                                           key local-summary
                                           (ejira-parser-org-to-jira local-desc-org
                                                                     desc-level))))
                                      (when assignee-changed
                                        (let* ((users (ejira--get-assignable-users key))
                                               (username (car (rassoc local-assignee users))))
                                          (ejira--jira-write (format "assign %s" key)
                                            (jiralib2-assign-issue key username))))
                                      (when (and priority-changed local-priority-id)
                                        (ejira--jira-write (format "set %s priority" key)
                                          (jiralib2-update-issue
                                           key `(priority . ((id . ,local-priority-id))))))
                                      (when deadline-changed
                                        ;; A removed deadline clears the due
                                        ;; date: Jira takes JSON null (a nil
                                        ;; value), and rejects "" as an
                                        ;; unparsable date.
                                        (ejira--jira-write (format "set %s due date" key)
                                          (jiralib2-update-issue
                                           key `(duedate . ,local-deadline))))
                                      (when state-changed
                                        (ejira--transition-to-org-state key local-state todo-kws))
                                      ;; Resolved again after the requests: a
                                      ;; reload may have moved the heading.
                                      (ejira--push-finalize
                                       (ejira--resolve-heading marker (list (cons "ID" key)))
                                       reviewed-hash sent-identity local-priority))))
                      plans)
              ;; No changes vs remote — re-baseline to clear the dirty hash
              ;; and record the remote fields as the new acknowledged state.
              ;; A cookie edit that already matches Jira's priority is
              ;; acknowledged too, or the heading would stay dirty.
              (when item
                (org-with-point-at marker
                  (ejira--store-remote-baseline item)
                  (when local-priority
                    (ejira--record-priority-identity local-priority))
                  (ejira--migrate-push-baseline))))))))
    (dolist (op (nreverse other-ops))
      (let* ((op-type (plist-get op :op))
             (object (plist-get op :object))
             (key (plist-get op :key))
             (marker (plist-get op :marker))
             (project (plist-get op :project))
             (data (plist-get op :data))
             (plan nil))
        (cond
         ((and (eq op-type 'update) (eq object 'comment))
          (let* ((issue-key (plist-get data :issue-key))
                 (commid (plist-get data :commid))
                 (comment-data (jiralib2-get-comment issue-key commid))
                 (remote-body (when comment-data (ejira--alist-get comment-data 'body)))
                 (remote-org (when comment-data
                               (ejira--expected-org-body marker remote-body)))
                 ;; Comment bodies are imported relative to the comment
                 ;; heading; export relative to the same level so edited
                 ;; comments round-trip their heading levels.
                 (comment-level (ejira--heading-body-level marker))
                 (local-body (ejira--get-heading-body marker))
                 (reviewed-hash (org-with-point-at marker
                                  (md5 (ejira--heading-reviewed-hash))))
                 (changes (ejira-confirm-field-changes
                           `(("body" ,(ejira--remote-body-for-diff
                                       (or remote-org "") local-body)
                              ,local-body)))))
            (when changes
              (setq plan (list :op 'update
                               :object 'comment
                               :project project
                               :title (format "%s comment %s" issue-key commid)
                               :parent-issue issue-key
                               :changes changes
                               :send (let ((issue-key issue-key) (commid commid)
                                           (marker marker)
                                           (comment-level comment-level)
                                           (body local-body)
                                           (reviewed-hash reviewed-hash))
                                       (lambda ()
                                         (ejira--jira-write (format "edit comment %s on %s" commid issue-key)
                                           (jiralib2-edit-comment
                                            issue-key commid
                                            (ejira-parser-org-to-jira body comment-level)))
                                         (ejira--push-finalize
                                          (ejira--resolve-heading marker (list (cons "CommId" commid)))
                                          reviewed-hash))))))))
         ((and (eq op-type 'update) (eq object 'status))
          (let ((action-name (plist-get data :action-name)))
            (setq plan (list :op 'update
                             :object 'status
                             :project project
                             :title key
                             :parent-issue key
                             :changes `(("transition" "" ,action-name))
                             :send (let ((key key) (marker marker) (action-name action-name))
                                     (lambda ()
                                       (let* ((actions (jiralib2-get-actions key))
                                              (action (cl-find-if
                                                       (lambda (a) (equal (cdr a) action-name))
                                                       actions)))
                                         (if action
                                             (progn
                                               (ejira--jira-write (format "transition %s: %s" key action-name)
                                                 (jiralib2-do-action key (car action)))
                                               (ejira--update-task-or-hold key)
                                               (ejira--clear-pending marker key "PendingTransition"))
                                           (error "ejira: transition '%s' not available for %s"
                                                  action-name key)))))))))
         ((and (eq op-type 'update) (eq object 'issuetype))
          (let ((new-type (plist-get data :new-type))
                (old-type (plist-get data :old-type)))
            (setq plan (list :op 'update
                             :object 'issuetype
                             :project project
                             :title key
                             :parent-issue key
                             :changes `(("issuetype" ,(or old-type "") ,new-type))
                             :send (let ((key key) (marker marker) (new-type new-type))
                                     (lambda ()
                                       (ejira--jira-write (format "set %s issue type" key)
                                         (jiralib2-set-issue-type key new-type))
                                       (ejira--update-task-or-hold key)
                                       (ejira--clear-pending marker key "PendingIssuetype")))))))
         ((and (eq op-type 'update) (eq object 'epic))
          (let ((new-epic (plist-get data :new-epic)))
            (setq plan (list :op 'update
                             :object 'epic
                             :project project
                             :title key
                             :parent-issue key
                             :changes `(("epic" "" ,new-epic))
                             :send (let ((key key) (marker marker) (new-epic new-epic)
                                         (epic-field ejira-epic-field))
                                     (lambda ()
                                       (ejira--jira-write (format "set %s epic link" key)
                                         (jiralib2-update-issue key `(,epic-field . ,new-epic)))
                                       (ejira--update-task-or-hold key)
                                       (ejira--clear-pending marker key "PendingEpic")))))))
         ((and (eq op-type 'create) (eq object 'subtask))
          (let* ((parent-key (plist-get data :parent-key))
                 (project-key (plist-get data :project-key))
                 (heading-title (org-with-point-at marker (ejira--jira-summary)))
                 ;; Prepare (and therefore capture) the description at
                 ;; plan-build time: the reviewed fields must be exactly
                 ;; what creation sends, not a re-read of later edits.
                 (local-body (or (org-with-point-at marker
                                   (ejira--prepare-new-issue-description))
                                 ""))
                 (local-state (org-with-point-at marker
                                (substring-no-properties (or (org-get-todo-state) ""))))
                 (priority-id (ejira--new-issue-priority-id
                               marker project-key parent-key))
                 (fields `(("title" ,heading-title)
                           ("state" ,local-state)
                           ("priority" ,(ejira--push-priority-label
                                         priority-id parent-key))
                           ("description" ,(or local-body ""))))
                 ;; One shared cell for the plan and the send: the review
                 ;; toggles the plan's list, and the send must read the
                 ;; same object or the toggle is silently ignored.
                 (assign-self-cell (list ejira--assign-new-issues)))
            (setq plan (list :op 'create
                             :object 'subtask
                             :project project-key
                             :title (format "new subtask: %s"
                                            (substring heading-title
                                                       0 (min 60 (length heading-title))))
                             :parent-issue parent-key
                             :fields fields
                             :assign-self assign-self-cell
                             :send (let ((marker marker) (project-key project-key)
                                         (target-id (ejira--creation-target-id marker))
                                         (parent-key parent-key)
                                         (priority-id priority-id)
                                         (summary heading-title)
                                         (reviewed-summary heading-title)
                                         (reviewed-body local-body)
                                         (subtask-type ejira-subtask-type-name)
                                         (description (ejira-parser-org-to-jira local-body))
                                         (orig-state local-state)
                                         (todo-kws (org-with-point-at marker
                                                     (when (boundp 'org-todo-keywords-1)
                                                       org-todo-keywords-1)))
                                         (assign-self assign-self-cell))
                                     (lambda ()
                                       ;; Resolve the heading by its ID: an
                                       ;; earlier send may have moved it.
                                       (setq marker (ejira--creation-target marker target-id))
                                       ;; Journal the attempt in the file
                                       ;; before the request: if Jira accepts
                                       ;; it and the response is lost, the
                                       ;; :Creating: property blocks a blind
                                       ;; duplicate on the next scan.
                                       (setq marker (ejira--commit-creation-journal marker))
                                       (let* ((priority-id
                                               (or priority-id
                                                   (ejira--default-priority-id
                                                    project-key parent-key)))
                                              (result (ejira--jira-write (format "create sub-task under %s" parent-key)
                                                        (apply #'jiralib2-create-issue
                                                               project-key subtask-type
                                                               summary description
                                                               (delq nil
                                                                     (list
                                                                      `(parent . ((key . ,parent-key)))
                                                                      (when priority-id
                                                                        `(priority . ((id . ,priority-id)))))))))
                                              (new-key (ejira--alist-get result 'key)))
                                         ;; Identity first: a failure in any
                                         ;; later step must not make the
                                         ;; heading look uncreated.
                                         (setq marker (ejira--record-new-issue-key new-key marker))
                                         (when (car assign-self)
                                           (let ((my-name (cdr (assoc 'name (jiralib2-get-user-info)))))
                                             (when my-name
                                               (ejira--jira-write (format "assign %s" new-key)
                                                 (jiralib2-assign-issue new-key my-name)))))
                                         (ejira--finalize-new-issue-review-safe
                                          new-key marker orig-state todo-kws
                                          reviewed-summary reviewed-body)
                                         (ejira--commit-issue new-key))))))))

         ((and (eq op-type 'create) (eq object 'issue))
          (let* ((project-key      (plist-get data :project-key))
                 (children         (plist-get data :children))
                 (issue-type       (or (plist-get data :issue-type)
                                       (or ejira-story-type-name "Task")))
                 (parent-epic      (plist-get data :parent-epic))
                 (parent-initiative (plist-get data :parent-initiative))
                 (parent-issue     (or parent-epic parent-initiative
                                       (plist-get op :parent-issue)))
                 (heading-title (org-with-point-at marker (ejira--jira-summary)))
                 ;; Prepare (and therefore capture) the description at
                 ;; plan-build time: the reviewed fields must be exactly
                 ;; what creation sends, not a re-read of later edits.
                 (local-body (or (org-with-point-at marker
                                   (ejira--prepare-new-issue-description))
                                 ""))
                 (desc-markup (ejira-parser-org-to-jira local-body))
                 (local-state (org-with-point-at marker
                                (substring-no-properties (or (org-get-todo-state) ""))))
                 (is-epic (equal issue-type ejira-epic-type-name))
                 (priority-id (ejira--new-issue-priority-id
                               marker project-key parent-issue))
                 ;; What creation sets besides the summary and the body;
                 ;; empty values are left out of the review.
                 (fields `(("title" ,heading-title)
                           ("state" ,local-state)
                           ("type" ,issue-type)
                           ("epic link" ,parent-epic)
                           ("parent" ,(when parent-initiative
                                        (if ejira-parent-link-field
                                            parent-initiative
                                          (format "%s (not linked: ejira-parent-link-field is nil)"
                                                  parent-initiative))))
                           ("priority" ,(ejira--push-priority-label
                                         priority-id parent-issue))
                           ("description" ,(or local-body ""))))
                 (label (cond (is-epic "epic")
                              (parent-epic "task")
                              (t "issue"))))
            (setq plan (list :op 'create
                             :object 'issue
                             :project project-key
                             :label label
                             :title (format "new %s: %s" label heading-title)
                             :parent-issue parent-issue
                             :fields fields
                             :children children
                             :assign-self (list ejira--assign-new-issues)
                             :send (let ((marker marker) (project-key project-key)
                                         (target-id (ejira--creation-target-id marker))
                                         (orig-state local-state)
                                         (summary heading-title)
                                         (desc-markup desc-markup)
                                         (children children)
                                         (priority-id priority-id)
                                         (issue-type issue-type)
                                         (parent-epic parent-epic)
                                         (parent-initiative parent-initiative)
                                         (is-epic is-epic)
                                         (assign-self (list ejira--assign-new-issues))
                                         (epic-field ejira-epic-field)
                                         (epic-summary-field ejira-epic-summary-field)
                                         (todo-kws (org-with-point-at marker
                                                     (when (boundp 'org-todo-keywords-1)
                                                       org-todo-keywords-1))))
                                     (lambda ()
                                       ;; Resolve the heading by its ID: an
                                       ;; earlier send may have moved it.
                                       (setq marker (ejira--creation-target marker target-id))
                                       ;; Journal the attempt in the file
                                       ;; before the request; see the
                                       ;; subtask path.
                                       (setq marker (ejira--commit-creation-journal marker))
                                       (let* ((epic-name-arg
                                               (when (and is-epic epic-summary-field)
                                                 `(,epic-summary-field . ,summary)))
                                              (priority-id
                                               (or priority-id
                                                   (ejira--default-priority-id
                                                    project-key parent-issue)))
                                              (result (ejira--jira-write (format "create %s in %s" issue-type project-key)
                                                        (apply #'jiralib2-create-issue
                                                               project-key issue-type
                                                               summary desc-markup
                                                               (delq nil
                                                                     (list epic-name-arg
                                                                           (when (and parent-epic epic-field)
                                                                             `(,epic-field . ,parent-epic))
                                                                           (when (and is-epic parent-initiative
                                                                                      ejira-parent-link-field)
                                                                             `(,ejira-parent-link-field . ,parent-initiative))
                                                                           (when priority-id
                                                                             `(priority . ((id . ,priority-id)))))))))
                                              (new-key (ejira--alist-get result 'key)))
                                         (when (and is-epic parent-initiative
                                                    (not ejira-parent-link-field))
                                           (display-warning
                                            'ejira
                                            (format "ejira-parent-link-field is nil — new Epic %s is not linked to Initiative %s"
                                                    summary parent-initiative)
                                            :warning))
                                         (when (and is-epic (not epic-summary-field))
                                           (display-warning
                                            'ejira
                                            "ejira-epic-summary-field is nil — run `ejira-guess-epic-sprint-fields' to auto-configure.  Epic Name not set for new Epic."
                                            :warning))
                                         ;; Identity first: a failure in any
                                         ;; later step must not make the
                                         ;; heading look uncreated.
                                         (setq marker (ejira--record-new-issue-key new-key marker))
                                         (when (car assign-self)
                                           (let ((my-name (cdr (assoc 'name (jiralib2-get-user-info)))))
                                             (when my-name
                                               (ejira--jira-write (format "assign %s" new-key)
                                                 (jiralib2-assign-issue new-key my-name)))))
                                         (ejira--finalize-new-issue-review-safe
                                          new-key marker orig-state todo-kws
                                          summary local-body)
                                         (ejira--commit-issue new-key)
                                         (dolist (child children)
                                           (condition-case err
                                               (ejira--push-create-cascaded-child
                                                new-key issue-type project-key child todo-kws
                                                (car assign-self))
                                             (error
                                              (display-warning
                                               'ejira
                                               (format "cascade creation failed for %s: %s"
                                                       (plist-get child :title)
                                                       (error-message-string err))
                                               :error)))))))))))
         ((and (eq op-type 'create) (eq object 'comment))
          (let* ((issue-key (plist-get data :issue-key))
                 (body (ejira--get-heading-body marker))
                 (preview (if (and body (> (length body) 0)) body "(no body)")))
            (setq plan (list :op 'create
                             :object 'comment
                             :project project
                             :title (format "new comment on %s" issue-key)
                             :parent-issue issue-key
                             :preview preview
                             :send (let ((marker marker) (issue-key issue-key)
                                         (body body)
                                         ;; The draft's identity until Jira
                                         ;; assigns the comment id.
                                         (target-id (ejira--creation-target-id marker)))
                                     (lambda ()
                                       (setq marker (ejira--creation-target marker target-id))
                                       ;; Journal the attempt in the file, as
                                       ;; issue creation does: a lost response
                                       ;; must not post the comment twice.
                                       (setq marker (ejira--commit-creation-journal marker))
                                       (let* ((comment (ejira--parse-comment
                                                        (ejira--jira-write (format "add comment on %s" issue-key)
                                                          (jiralib2-add-comment
                                                           issue-key
                                                           (ejira-parser-org-to-jira body)))))
                                              (commid (format "%s" (ejira-comment-id comment))))
                                         ;; The comment id replaces the draft's
                                         ;; ID and journal in one write, before
                                         ;; anything else can fail.
                                         (unless (ejira--commit-edit
                                                  (marker-buffer marker)
                                                  (lambda ()
                                                    (ejira--apply-heading-patch
                                                     (list (cons "ID" target-id))
                                                     `(("CommId" . ,commid)
                                                       ("TYPE" . "ejira-comment")
                                                       ("ID")
                                                       ("Creating")))))
                                           (error "ejira: added comment %s on %s, but its heading is no longer in %s; the next sync imports it"
                                                  commid issue-key (buffer-name (marker-buffer marker))))
                                         (ejira--update-comment issue-key comment)
                                         (ejira--commit-heading
                                          (ejira--resolve-heading
                                           marker (list (cons "CommId" commid)))))))))))
         ((and (eq op-type 'delete) (eq object 'comment))
          (let* ((issue-key (plist-get data :issue-key))
                 (commid (plist-get data :commid))
                 (body (plist-get data :body)))
            (setq plan (list :op 'delete
                             :object 'comment
                             :project project
                             :title (format "delete comment on %s" issue-key)
                             :parent-issue issue-key
                             :preview (or body "(empty)")
                             :send (let ((issue-key issue-key) (commid commid) (marker marker))
                                     (lambda ()
                                       (ejira--jira-write (format "delete comment %s on %s" commid issue-key)
                                         (jiralib2-delete-comment issue-key commid))
                                       ;; Removed from the file too: a heading
                                       ;; left there would be deleted again by
                                       ;; the next review.  A heading already
                                       ;; gone from the file needs nothing.
                                       (ejira--commit-edit
                                        (marker-buffer marker)
                                        (lambda ()
                                          (ejira--apply-heading-patch
                                           (list (cons "CommId" commid)) 'cut))))))))))
        (when plan (push plan plans))))
    (nreverse plans)))

(defmacro ejira--with-pre-scan (buf &rest body)
  "Bind `ejira--pre-scanning' t while scanning BUF for pending operations.
Causes all `ejira--with-expand-all' calls to skip their per-call
`outline-show-all', which is safe because org structural navigation
(org-goto-first-child, re-search-forward, org-narrow-to-subtree, etc.)
operates on buffer text regardless of fold state."
  (declare (indent 1))
  `(with-current-buffer ,buf
     (let ((ejira--pre-scanning t))
       ,@body)))

(defun ejira--save-plan-build-edits ()
  "Save what building push plans wrote into the current buffer.
Building plans assigns creation IDs and re-baselines headings that
already match Jira; those edits are ejira's, and are saved unless the
buffer also holds unsaved user edits, which stay for the user to save."
  (unless (ejira--user-edits-p)
    (let ((ejira--pushing t))
      (ejira--save-buffer-safe))))

(defun ejira-push-at-point ()
  "Scan the ejira heading at point and show ejira-confirm for it."
  (interactive)
  (ejira--with-transaction
    (ejira--with-pre-scan (current-buffer)
      (let* ((ops (ejira--push-scan-buffer (current-buffer)))
             (point-ops (cl-remove-if-not
                         (lambda (op)
                           (let ((m (plist-get op :marker)))
                             (and m (equal (marker-buffer m) (current-buffer))
                                  (= (save-excursion (goto-char m)
                                                     (line-beginning-position))
                                     (line-beginning-position)))))
                         ops))
             (plans (when point-ops (ejira--push-build-plans point-ops))))
        (ejira--save-plan-build-edits)
        (if plans
            (ejira-confirm-show plans)
          (message "ejira: nothing to push at point"))))))

(defun ejira--auto-sync-file-p (&optional file)
  "Return non-nil when FILE (default: current buffer's file) auto-syncs.
Auto-sync files are reconciled by the coordinated cycle (pull, then
review every local change) instead of a bare save-time review; see
`ejira-auto-sync-tracked'.  Without FILE, an Org buffer holding an
ejira issue heading counts as tracked too."
  (let ((name (or file (buffer-file-name))))
    (and name
         (or (member (file-truename name) (ejira--auto-sync-files))
             (and (not file)
                  ejira-auto-sync-tracked
                  (derived-mode-p 'org-mode)
                  (ejira--buffer-has-issue-heading-p))))))

(defun ejira--push-on-save ()
  "Offer to push locally-edited ejira items after saving a managed buffer.
Auto-sync files (see `ejira-auto-sync-tracked') are handed to the
automatic reconciliation queue instead, with review."
  (when (and ejira-push-on-save
             (not ejira--pushing)
             (not ejira--syncing)
             ;; A save landing mid-reconciliation must not start a
             ;; competing scan: the cycle already owns this file.
             (not ejira--sync-in-progress)
             (derived-mode-p 'org-mode)
             (ejira--auto-sync-file-p))
    ;; A save in Emacs is the user asking to sync: review what is held.
    (ejira--auto-sync-enqueue (buffer-file-name) t))
  (when (and ejira-push-on-save
             (not ejira--pushing)
             (not ejira--syncing)
             (not ejira--sync-in-progress)
             (derived-mode-p 'org-mode)
             (not (ejira--auto-sync-file-p))
             (ejira--buffer-has-pushable-p))
    (ejira--with-transaction
      (ejira--with-pre-scan (current-buffer)
        (let* ((ops   (ejira--push-scan-buffer (current-buffer)))
               (plans (when ops (ejira--push-build-plans ops))))
          (ejira--save-plan-build-edits)
          (dolist (op ops)
            (when (eq (plist-get op :op) 'blocked)
              (message "ejira: %s — %s"
                       (or (plist-get op :title) "<heading>")
                       (plist-get op :reason))))
          (when plans
            (ejira-confirm-show plans)))))))

(add-hook 'after-save-hook #'ejira--push-on-save)

(provide 'ejira-push)
;;; ejira-push.el ends here
