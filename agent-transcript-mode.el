;;; agent-transcript-mode.el --- Read agent transcripts as a conversation -*- lexical-binding: t; -*-

;;; Commentary:

;; `agent-shell' transcripts are markdown logs: YAML frontmatter, a
;; title, then one timestamped section per event, all emitted by
;; `agent-shell--append-transcript':
;;
;;   ## User (2026-10-11 00:21:04)
;;   ## Agent's Thoughts (2026-10-11 00:21:04)
;;   ## Agent (2026-10-11 00:21:04)
;;   ## Question (2026-10-11 00:21:04)      ; older transcripts
;;   ### Tool Call [completed]: ...
;;
;; This mode displays them as the conversation they record: the
;; frontmatter stays, but the title, the timestamps, the thoughts and
;; the tool calls go, and a run of agent messages broken up by
;; thoughts and tool calls reads as one "# Agent" block.
;;
;;   ---
;;   agent: "OpenCode"
;;   ---
;;
;;   # User
;;
;;   What broke?
;;
;;   # Agent
;;
;;   [message 1]
;;   [message 2]
;;
;; Nothing is rewritten: the view is overlays over the real file, and the
;; buffer is read-only while the mode is on, so toggling it off gives
;; back the file byte for byte.
;;
;; Everything else that looks like a heading is content: agent prose has
;; its own headings (indented two levels by the transcript writer) and
;; tool output embeds whole files, comments and headings of its own.  So
;; sections are split on those five forms only, never on bare "^#+ ",
;; and never inside a fenced code block, where a quoted transcript can
;; carry section headers of its own.

;;; Code:

(defgroup agent-transcript nil
  "Read agent-shell transcripts as a plain User/Agent conversation."
  :group 'convenience)

(defcustom agent-transcript-section-labels
  '(("User"  . "# User")
    ("Agent" . "# Agent"))
  "Alist of transcript section name to the heading shown in its place.

A section whose name is absent is hidden entirely, as is everything
between the frontmatter and the first labelled section (the title),
and every tool call.  Consecutive
sections sharing a label are merged under a single heading, so adding
\(\"Question\" . \"# User\") would both reveal question sections and fold
them into the surrounding user message."
  :type '(alist :key-type string :value-type string)
  :group 'agent-transcript)

(defconst agent-transcript--section-regexp
  (concat "^\\(?:"
          "## \\(User\\|Agent's Thoughts\\|Agent\\|Question\\)"
          " ([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\})$"
          "\\|### Tool Call \\["
          "\\)")
  "Regexp matching the start of any agent-shell transcript section.
Group 1 is the section name for \"## \" sections, nil for tool calls.")

(defvar-local agent-transcript--saved-read-only nil
  "Value of `buffer-read-only' before `agent-transcript-mode' was enabled.
Unbound locally while the mode is off, which is how the mode tells a
fresh enable from a redundant one.")

(defun agent-transcript--label (name)
  "Return the heading displayed for section NAME, or nil to hide it."
  (cdr (assoc name agent-transcript-section-labels)))

(defun agent-transcript--hide-region (start end)
  "Hide text from START to END with an invisible overlay."
  (when (< start end)
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'invisible 'agent-transcript)
      (overlay-put ov 'agent-transcript t)
      (overlay-put ov 'evaporate t))))

(defun agent-transcript--show-region (start end text &optional header)
  "Display TEXT in place of the region from START to END.
With HEADER, mark the overlay as a conversation heading and face it as
one, so `agent-transcript-next-turn' can find it."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'display
                 (if header
                     (propertize text 'face (if (facep 'markdown-header-face-1)
                                                'markdown-header-face-1
                                              'bold))
                   text))
    (overlay-put ov 'agent-transcript t)
    (overlay-put ov 'agent-transcript-header header)
    (overlay-put ov 'evaporate t)))

(defun agent-transcript--code-block-ranges ()
  "Return the (START . END) range of every fenced code block, in order.
A transcript that quotes transcript syntax — a pasted sample, a tool
call that cat'd another transcript — holds lines that look exactly like
section headers.  The writer's header indentation does not reach inside
fences, so the headers have to be skipped here instead.

Fences close on a run of the same character at least as long as the
opener, per CommonMark, which is what the transcript writer relies on to
wrap tool output that itself contains backticks.  An unclosed fence is
ignored rather than run to the end of the buffer as CommonMark would
have it: pasted messages do carry stray opening fences, and letting one
swallow the rest of the transcript would hide the whole conversation."
  (save-excursion
    (goto-char (point-min))
    (let ((ranges nil))
      (while (re-search-forward "^[ \t]*\\(```+\\|~~~+\\)" nil t)
        (let ((start (match-beginning 0))
              (opener-end (line-end-position))
              (closer (format "^[ \t]*%s\\{%d,\\}[ \t]*$"
                              (regexp-quote (substring (match-string 1) 0 1))
                              (length (match-string 1)))))
          (forward-line 1)
          (if (re-search-forward closer nil t)
              (push (cons start (min (point-max) (1+ (line-end-position)))) ranges)
            (goto-char opener-end))))
      (nreverse ranges))))

(defconst agent-transcript--separator "\n\n"
  "Text shown between two consecutive blocks of the conversation.
Every gap is normalised to this, so the view does not inherit the
transcript's own ragged spacing.")

(defun agent-transcript--frontmatter-end ()
  "Return the end of the YAML frontmatter block at the top of the buffer.
Transcripts open with a \"---\"-delimited block written by
`agent-shell--make-transcript-frontmatter'.  Return the position
after its closing \"---\" line and any blank lines that follow it,
so the block stays visible and the hidden run starts at the title.
Return `point-min' when the buffer does not open with frontmatter,
including when the opening \"---\" is never closed."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at-p "---[ \t]*\n")
        (progn
          (forward-line 1)
          (if (re-search-forward "^---[ \t]*\n" nil t)
              (progn
                (skip-chars-forward "\n")
                (point))
            (point-min)))
      (point-min))))

(defun agent-transcript--plan ()
  "Return the overlays to build for this buffer, in buffer order.
Each element is a (hide START END) or (show START END TEXT) form, the
latter displaying TEXT in place of the region.  Return nil when the
buffer holds no labelled section.

Walks the sections once carrying three pieces of state: HIDE-START,
where the current hidden run began, nil inside a visible one;
LAST-LABEL, the heading last displayed; and LAST-END, the end of the
last region spoken for, which keeps the gap-trimming below from reaching
back into it.  A section repeating LAST-LABEL has its header shown as a
bare separator, which is what merges it into the block above.

The frontmatter is never hidden: the first hidden run starts after it
\(see `agent-transcript--frontmatter-end'), so only the title between
it and the first labelled section goes."
  (save-excursion
    (goto-char (point-min))
    (let* ((frontmatter-end (agent-transcript--frontmatter-end))
           (hide-start frontmatter-end)
           (last-label nil)
           (last-end (point-min))
           (fences (agent-transcript--code-block-ranges))
           (plan nil))
      (while (re-search-forward agent-transcript--section-regexp nil t)
        (let ((header-start (match-beginning 0))
              (header-end (line-end-position))
              (label (and (match-string 1)
                          (agent-transcript--label (match-string 1)))))
          ;; Matches arrive in buffer order, so the fence list only ever
          ;; needs to move forward.
          (while (and fences (<= (cdar fences) header-start))
            (pop fences))
          (cond
           ((and fences (<= (caar fences) header-start))) ; quoted, not a section
           ((null label)
            (unless hide-start (setq hide-start header-start)))
           (t
            ;; Swallow the blank lines the previous block trails, so the
            ;; separator below is the only thing standing between them.
            (push (list 'hide
                        (if last-label
                            (max last-end
                                 (save-excursion
                                   (goto-char (or hide-start header-start))
                                   (skip-chars-backward "\n")
                                   (point)))
                          frontmatter-end)
                        header-start nil)
                  plan)
            (if (equal label last-label)
                (let ((body-start (save-excursion
                                    (goto-char header-end)
                                    (forward-line 1)
                                    (skip-chars-forward "\n")
                                    (point))))
                  (push (list 'show header-start body-start
                              agent-transcript--separator)
                        plan)
                  (setq last-end body-start))
              (push (list 'show header-start header-end
                          (if last-label
                              (concat agent-transcript--separator label)
                            label)
                          'header)
                    plan)
              (setq last-end header-end))
            (setq hide-start nil
                  last-label label)))))
      (when hide-start
        (push (list 'hide hide-start (point-max) nil) plan))
      (and last-label (nreverse plan)))))

(defun agent-transcript--apply-filter ()
  "Rebuild the conversation view over the whole buffer.
Leave the buffer untouched when it holds no labelled section, so that a
markdown file that is not a transcript does not go blank."
  (remove-overlays (point-min) (point-max) 'agent-transcript t)
  (if-let* ((plan (agent-transcript--plan)))
      (pcase-dolist (`(,op ,start ,end ,text ,header) plan)
        (pcase op
          ('hide (agent-transcript--hide-region start end))
          ('show (agent-transcript--show-region start end text header))))
    (message "agent-transcript: no %s section here, showing the file as is"
             (mapconcat #'car agent-transcript-section-labels "/"))))

(defun agent-transcript--header-positions ()
  "Return the positions of the visible conversation headings, in order."
  (sort (delq nil (mapcar (lambda (ov)
                            (and (overlay-get ov 'agent-transcript-header)
                                 (overlay-start ov)))
                          (overlays-in (point-min) (point-max))))
        #'<))

(defun agent-transcript-next-turn (&optional count)
  "Move to the COUNTth next conversation heading.
A negative COUNT moves backwards."
  (interactive "p")
  (let* ((count (or count 1))
         (backward (< count 0))
         (positions (seq-filter (if backward
                                    (lambda (pos) (< pos (line-beginning-position)))
                                  (lambda (pos) (> pos (point))))
                                (agent-transcript--header-positions))))
    (if-let* ((target (nth (1- (abs count))
                           (if backward (nreverse positions) positions))))
        (goto-char target)
      (user-error "No %s turn" (if backward "previous" "next")))))

(defun agent-transcript-previous-turn (&optional count)
  "Move to the COUNTth previous conversation heading."
  (interactive "p")
  (agent-transcript-next-turn (- (or count 1))))

(defun agent-transcript--leave-hidden-text ()
  "Move point forward out of hidden text, if it sits in any."
  (when-let* ((ov (seq-find (lambda (ov)
                              (eq (overlay-get ov 'invisible) 'agent-transcript))
                            (overlays-at (point)))))
    (goto-char (overlay-end ov))))

(defun agent-transcript--parse-frontmatter ()
  "Parse YAML frontmatter from the current buffer.
Returns an alist of (key . value) pairs, or nil if the buffer has no
frontmatter."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "\\`---[ \t]*\n")
      (let (fields)
        (forward-line 1)
        (while (not (or (eobp) (looking-at-p "^---[ \t]*$")))
          (when (looking-at "^\\([^:]+\\):[ \t]*\"\\([^\"]*\\)\"[ \t]*$")
            (push (cons (match-string 1) (match-string 2)) fields))
          (forward-line 1))
        (when fields
          (nreverse fields))))))

(defun agent-transcript--find-agent-config (agent-name)
  "Find an agent-shell config matching AGENT-NAME (display name).
Searches `:mode-line-name' and `:buffer-name' fields of all resolved
agent configs.  Returns the config alist, or nil if not found."
  (when (and agent-name (fboundp 'agent-shell--resolved-agent-configs))
    (seq-find (lambda (config)
                (or (equal (map-elt config :mode-line-name) agent-name)
                    (equal (map-elt config :buffer-name) agent-name)))
              (agent-shell--resolved-agent-configs))))

(defun agent-transcript-resume-session ()
  "Resume the agent-shell session recorded in this transcript.
Reads the frontmatter for the agent name, session ID and working
directory, then starts a new agent-shell that resumes the session."
  (interactive)
  (unless (fboundp 'agent-shell--start)
    (user-error "agent-shell is not loaded"))
  (let ((fm (agent-transcript--parse-frontmatter)))
    (unless fm
      (user-error "No frontmatter found in this transcript"))
    (let* ((agent-name (cdr (assoc "agent" fm)))
           (session-id (cdr (assoc "session_id" fm)))
           (workdir (cdr (assoc "working_directory" fm))))
      (unless session-id
        (user-error "No session_id in frontmatter"))
      (unless workdir
        (user-error "No working_directory in frontmatter"))
      (unless (file-directory-p workdir)
        (user-error "Working directory no longer exists: %s" workdir))
      (let ((config (agent-transcript--find-agent-config agent-name)))
        (unless config
          (user-error "No agent-shell config found for agent: %s" agent-name))
        (agent-shell--start
         :config config
         :session-id session-id
         :new-session t
         :cwd workdir)))))

(defvar agent-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-transcript-next-turn)
    (define-key map (kbd "p") #'agent-transcript-previous-turn)
    (define-key map (kbd "g") #'agent-transcript-refresh)
    (define-key map (kbd "q") #'agent-transcript-mode)
    (define-key map (kbd "C-c C-a") #'agent-transcript-mode)
    (define-key map (kbd "R") #'agent-transcript-resume-session)
    map)
  "Keymap for `agent-transcript-mode'.
Single-letter keys are safe here: the mode makes the buffer read-only.")

;;;###autoload
(define-minor-mode agent-transcript-mode
  "Toggle agent transcript view: the User/Agent conversation alone.

Keeps the frontmatter, but hides the title, thoughts and tool calls,
and merges each run of agent messages into one block.  The buffer
becomes read-only; turning the mode off restores it and the raw file
view."
  :lighter " 🤖"
  :keymap agent-transcript-mode-map
  (if agent-transcript-mode
      (progn
        (unless (local-variable-p 'agent-transcript--saved-read-only)
          (setq agent-transcript--saved-read-only buffer-read-only))
        (setq buffer-read-only t)
        ;; Required: markdown-mode turns `buffer-invisibility-spec' into a
        ;; list (it adds `markdown-markup'), and a list spec hides only the
        ;; symbols it lists.  Without this, every overlay below is a no-op.
        (add-to-invisibility-spec 'agent-transcript)
        (add-hook 'after-revert-hook #'agent-transcript-refresh nil t)
        (agent-transcript--apply-filter)
        (agent-transcript--leave-hidden-text))
    (remove-hook 'after-revert-hook #'agent-transcript-refresh t)
    (remove-from-invisibility-spec 'agent-transcript)
    (remove-overlays (point-min) (point-max) 'agent-transcript t)
    (when (local-variable-p 'agent-transcript--saved-read-only)
      (setq buffer-read-only agent-transcript--saved-read-only)
      (kill-local-variable 'agent-transcript--saved-read-only))))

(defun agent-transcript-refresh ()
  "Rebuild the view, for instance after the transcript grew."
  (interactive)
  (when agent-transcript-mode
    (agent-transcript--apply-filter)))

(defcustom agent-transcript-file-regexp
  "/\\.?agent-shell/transcripts/.*\\.md\\'"
  "Regexp matching transcript files `agent-transcript-mode' auto-enables on.
Matched against the full file name, so it covers both the per-repo
\".agent-shell/transcripts/\" directory and a global
\"agent-shell/transcripts/<repo>/\" one."
  :type 'regexp
  :group 'agent-transcript)

;;;###autoload
(defun agent-transcript-maybe-enable ()
  "Enable `agent-transcript-mode' if the file is an agent-shell transcript.
Meant for `find-file-hook', which runs after the major mode is set."
  (when (and buffer-file-name
             (string-match-p agent-transcript-file-regexp buffer-file-name))
    (agent-transcript-mode 1)))

;;;###autoload
(add-hook 'find-file-hook #'agent-transcript-maybe-enable)

;;;###autoload
(defun agent-transcript-view-file (file)
  "Open FILE with `agent-transcript-mode' enabled."
  (interactive "fTranscript file: ")
  (find-file file)
  (agent-transcript-mode 1))

(provide 'agent-transcript-mode)

;;; agent-transcript-mode.el ends here
