;; -*- lexical-binding: t; -*-
(use-package async)

(require 'project)
(require 'project-local)

(add-hook 'project-find-functions 'conf--project-try-local 90)

;; project files - capf
(defvar conf--project-files-cache (make-hash-table :test 'equal))
(defvar conf--project-files-cache-time (make-hash-table :test 'equal))

(defun conf--project-files-cached (project)
  (let* ((load-dir conf--base-dir)
         (root (project-root project))
         (cache-time (gethash root conf--project-files-cache-time))
         (now (current-time)))
    (when (or (not cache-time)
              (> (float-time (time-subtract now cache-time)) 10)) ; 10s cache
      (puthash root now conf--project-files-cache-time)
      (async-start
       (lambda ()
         (add-to-list 'load-path load-dir)
         (require 'project)
         (require 'project-local)
         (require 'vc-git)
         (project-files project))

       (lambda (result)
         (puthash root
                  (mapcar (lambda (f) (file-relative-name f root)) result)
                  conf--project-files-cache))))
    (gethash root conf--project-files-cache)))

(defun conf--project-files-cached-current ()
  (conf--project-files-cached (project-current)))

(defun conf--project-files-capf ()
  (when-let* ((project (project-current))
              (bounds (bounds-of-thing-at-point 'filename))
              (start (or (car bounds) (point)))
              (end (or (cdr bounds) (point)))
              (table (completion-table-with-cache
                     (lambda (_prefix)
                       (conf--project-files-cached project)))))
    (list start
          end
          table
          :exclusive 'no)))

(defun conf--aidermacs-keywords-completion-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (keywords '("/add" "/drop" "/reset" "/paste")))
    (when bounds
      (list (car bounds) (cdr bounds) keywords
            :exclusive 'no))))

(use-package shell-maker)
(use-package acp)

(use-package agent-shell
  :bind
  (:map agent-shell-mode-map
   ("TAB"       . nil)
   ("<backtab>" . nil)
   ("C-M-n"     . agent-shell-next-item)
   ("C-M-p"     . agent-shell-previous-item)
   ("C-c RET"   . agent-shell-submit)
   ("RET"       . newline)
   :map agent-shell-viewport-view-mode-map
   ("TAB"       . nil)
   ("<backtab>" . nil)
   ("C-M-n"     . agent-shell-next-item)
   ("C-M-p"     . agent-shell-previous-item)
   :map agent-shell-ui-fragment-map
   ("TAB"       . agent-shell-ui-toggle-fragment))
  :config
  (with-eval-after-load 'shell-maker
    (define-key agent-shell-mode-map (kbd "C-M-h") nil))
  (setq agent-shell-anthropic-default-session-mode-id "plan")
  (advice-add 'agent-shell--project-files :override #'conf--project-files-cached-current)
  (setq agent-shell-highlight-blocks t)
  (setq agent-shell-session-strategy 'prompt)
  (setq markdown-overlays-prettify-tables t)
  (setopt agent-shell-session-restore-verbosity 'full)
  (setq agent-shell-elicitation--experimental-feature-enabled t)
  ;; (setq agent-shell-persistent-prompt-enabled t)
  (setq agent-shell-show-cost-indicator t)
  (setq agent-shell-busy-submit-default-function 'agent-shell-busy-submit-steer)
  (setq agent-shell-busy-submit-override-function 'agent-shell-busy-submit-queue)
  (setq agent-shell-preferred-agent-config '(preselect . claude-code))
  (setopt agent-shell-opencode-default-model-id "openrouter/mistralai/mistral-large-4-0")

  (setq agent-shell-openai-default-config-options '(("reasoning_effort" . "high")))

  (add-to-list 'agent-shell-markdown-language-mapping '("json" . "js-json"))
  (add-to-list 'agent-shell-markdown-language-mapping '("tsx" . "tsx-ts"))
  (add-to-list 'agent-shell-markdown-language-mapping '("ts" . "typescript-ts"))
  (add-to-list 'agent-shell-markdown-language-mapping '("jsx" . "tsx-ts"))
  (add-to-list 'agent-shell-markdown-language-mapping '("typescript" . "typescript-ts"))
  (add-to-list 'agent-shell-markdown-language-mapping '("hcl" . "terraform")))

(defun agent-shell-project-next-buffer ()
  "Switch to the next agent-shell buffer in the current project."
  (interactive)
  (let* ((buffers (agent-shell-project-buffers))
         (current (current-buffer))
         (tail (member current buffers))
         (next (if (and tail (cadr tail))
                   (cadr tail)
                 (car buffers))))
    (when next
      (switch-to-buffer next))))

(defun agent-shell-project-previous-buffer ()
  "Switch to the previous agent-shell buffer in the current project."
  (interactive)
  (let* ((buffers (agent-shell-project-buffers))
         (current (current-buffer))
         (idx (seq-position buffers current))
         (prev (when idx
                 (nth (mod (1- idx) (length buffers)) buffers))))
    (when prev
      (switch-to-buffer prev))))

(define-key agent-shell-mode-map (kbd "M-l") #'agent-shell-project-next-buffer)
(define-key agent-shell-mode-map (kbd "M-h") #'agent-shell-project-previous-buffer)
(define-key agent-shell-mode-map (kbd "M-t") #'agent-shell-new-shell)
(define-key agent-shell-mode-map (kbd "C-d") nil)

(setq agent-shell-context-sources '(files region error))

(defun my/agent-shell-transcript-file-path-global ()
  "Generate a transcript file path under `user-emacs-directory'.

For example:

 ~/.emacs.d/agent-shell/transcripts/<time>.md"
  (let* ((dir (expand-file-name
               (concat "agent-shell/transcripts/")
               user-emacs-directory))
         (filename (format-time-string "%F-%H-%M-%S.md")))
    (make-directory dir t)
    (expand-file-name filename dir)))


(defun my/agent-shell-transcript-file-path ()
  "Like `agent-shell--default-transcript-file-path', but place the
transcript under the main worktree root when inside a linked worktree."
  (let* ((default-directory
          (if (and (fboundp 'magit-inside-worktree-p)
                   (magit-inside-worktree-p :noerror)
                   (fboundp 'magit-list-worktrees))
              (car (car (magit-list-worktrees)))
            default-directory))
         (dir      (agent-shell--dot-subdir "transcripts"))
         (filename (format-time-string "%F-%H-%M-%S.md"))
         (filepath (expand-file-name filename dir)))
    filepath))

(setq agent-shell-transcript-file-path-function #'my/agent-shell-transcript-file-path-global)

(defun conf--agent-shell-update-transcript-title (title)
  "Update the transcript file's frontmatter with TITLE.
Adds or replaces the `title' key in the YAML frontmatter of the
current agent-shell buffer's transcript file."
  (when-let* ((filepath agent-shell--transcript-file)
              ((file-exists-p filepath))
              (title (string-trim title))
              ((not (string-empty-p title))))
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (if (looking-at-p "\\`---\n")
          (let ((end (save-excursion
                       (forward-line 1)
                       (re-search-forward "^---$" nil t)
                       (point))))
            ;; Check if title key already exists in frontmatter
            (goto-char (point-min))
            (forward-line 1)
            (if (re-search-forward "^title:\\s-*\".*\"\\s-*$" end t)
                ;; Replace existing title line
                (progn
                  (beginning-of-line)
                  (delete-region (point) (line-end-position))
                  (insert (format "title: %s" (json-encode-string title))))
              ;; Add title key after the opening ---
              (goto-char (point-min))
              (forward-line 1)
              (insert (format "title: %s\n" (json-encode-string title))))
            (write-region (point-min) (point-max) filepath nil 'no-message))
        (message "Transcript %s has no frontmatter; skipping title update"
                 (abbreviate-file-name filepath))))))

(defun conf--agent-shell-on-session-title-changed (event)
  "Handle `session-title-changed' EVENT by updating the transcript title."
  (when-let* ((data (map-elt event :data))
              (title (map-elt data :title)))
    (conf--agent-shell-update-transcript-title title)))

(defun conf--agent-shell-subscribe-title-changed ()
  "Subscribe to `session-title-changed' to update transcript frontmatter."
  (agent-shell-subscribe-to
   :shell-buffer (current-buffer)
   :event 'session-title-changed
   :on-event #'conf--agent-shell-on-session-title-changed))

(add-hook 'agent-shell-mode-hook #'conf--agent-shell-subscribe-title-changed)

(defun conf--agent-shell-send-current-file ()
  (interactive)
  (when (buffer-file-name)
    (agent-shell--insert-to-shell-buffer
     :text (agent-shell--get-files-context :files (list (buffer-file-name))))))

(defvar agent-shell--status-colors
  '((busy . "orange")
    (blocked . "red")
    (ready . "green"))
  "Alist mapping `agent-shell-status' symbols to a display color.

See `agent-shell-status' for the meaning of each status.")

(defun agent-shell-rename-buffer (name)
  "Rename the current agent-shell buffer to NAME."
  (interactive
   (list (read-string "Rename buffer: " (buffer-name))))
  (rename-buffer name t))

(defun agent-shell--status-propertized (status)
  "Return STATUS (a symbol) as a colorized, left-justified 8-char string."
  (let ((color (alist-get status agent-shell--status-colors)))
    (format "%-8s"
            (propertize (symbol-name status)
                        'face (if color (list :foreground color) 'default)))))

(defun agent-shell--annotate-buffer (name)
  "Annotate agent-shell buffer NAME with status and project for completion."
  (when-let* ((buf (get-buffer name)))
    (with-current-buffer buf
      (format "  %s %s %s"
              (agent-shell--status-propertized (agent-shell-status))
              (propertize (abbreviate-file-name (or (agent-shell-cwd) "?"))
                          'face '(:foreground "cyan"))
              (propertize (let ((title (string-trim
                                        (or (map-nested-elt agent-shell--state
                                                            '(:session :title))
                                            ""))))
                            (if (> (length title) 80)
                                (concat (substring title 0 77) "...")
                              title))
                          'face '(:foreground "gray70"))))))

(defun agent-shell--make-annotate-function (candidates)
  "Return an annotate function aligning annotations across CANDIDATES.

Since annotations are appended right after each candidate's text, and
candidates (buffer names) can have different lengths, this pads each
candidate with enough spaces to reach the width of the longest one
before delegating to `agent-shell--annotate-buffer', so that the
status/path/title columns all start at the same position."
  (let ((width (if candidates (apply #'max (mapcar #'length candidates)) 0)))
    (lambda (name)
      (when-let* ((annotation (agent-shell--annotate-buffer name)))
        (concat (make-string (max 0 (- width (length name))) ?\s)
                annotation)))))

(cl-defun agent-shell--embark-with-target-buffer (&rest rest &key run target &allow-other-keys)
  "Run action RUN with `current-buffer' bound to the buffer named TARGET.

Follows the same pattern as `embark--cd', but switches to the target
buffer instead of its associated directory."
  (if-let* ((buf (get-buffer target)))
      (with-current-buffer buf
        (apply run :target target rest))
    (apply run :target target rest)))

(defvar agent-shell-embark-buffer-map nil
  "Keymap for Embark actions on `agent-shell-buffer' category targets.")

(with-eval-after-load 'embark
  (setq agent-shell-embark-buffer-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map embark-buffer-map)
          (define-key map "r" #'agent-shell-rename-buffer)
          map))
  (add-to-list 'embark-keymap-alist '(agent-shell-buffer . agent-shell-embark-buffer-map))
  (add-to-list 'embark-around-action-hooks
               '(agent-shell-rename-buffer agent-shell--embark-with-target-buffer))
  (add-to-list 'embark-target-injection-hooks
               '(agent-shell-rename-buffer embark--ignore-target))

  ;; ;; or use C-u to not quit
  ;; (setf (alist-get 'agent-shell-rename-buffer embark-quit-after-action) nil)

  (add-to-list 'embark-post-action-hooks '(agent-shell-rename-buffer embark--restart)))

(defun agent-shell--list-and-select-from (buffers &optional allow-new)
  "Select and switch to an agent-shell buffer from BUFFERS.

Candidates are buffer names under a custom `agent-shell-buffer' category,
so `embark-act' offers `agent-shell-embark-buffer-map' (buffer actions,
plus `agent-shell-rename-buffer' bound to \"r\") on the selection.
Using a category distinct from the plain `buffer' one also means our own
`annotation-function' is used directly, without needing `marginalia-cycle'.

Selection supports Consult-style buffer preview: as you move through
candidates, the corresponding buffer is shown, just like `consult-buffer'.

When ALLOW-NEW is non-nil, append a \"New agent shell\" entry as the
last completion candidate.  Picking it forces a new shell, equivalent
to \\<agent-shell-mode-map>\\`C-u M-x agent-shell'."
  (let* ((new-label "➕ New agent shell")
         (new-worktree-label "➕ New agent shell (create worktree)")
         (names (mapcar #'buffer-name buffers))
         (candidates (if allow-new (append names (list new-label new-worktree-label)) names)))
    (if (null candidates)
        (if allow-new
            (agent-shell-new-shell)
          (message "No agent-shell buffers found."))
      (let* ((choice (consult--read
                      candidates
                      :prompt "Agent shell: "
                      :category 'agent-shell-buffer
                      :annotate (agent-shell--make-annotate-function names)
                      :require-match t
                      :sort nil                       ; keep "New agent shell" last
                      :state (consult--buffer-preview) ; live preview, no custom action on RET
                      :history 'agent-shell--buffer-history))
             (buf (unless (equal choice new-label) (get-buffer choice)))
             ;; Capture context (region/files/error/line) from the
             ;; *current* (source) buffer before switching away, mirroring
             ;; what `agent-shell--dwim' does for the default entry point.
             (text (when buf (agent-shell--context :shell-buffer buf))))
        (cond
         ((equal choice new-label) (my/agent-shell-start))
         ((equal choice new-worktree-label) (my/agent-shell-new-worktree-shell))
         (buf
          (agent-shell--display-buffer buf)
          (when text
            (agent-shell--insert-to-shell-buffer :text text :shell-buffer buf))))))))

(defun my/agent-shell-new-worktree-shell ()
  (interactive)
  (unless (agent-shell-worktree--git-repo-root)
    (user-error "Not in a git repository"))
  (let* ((worktrees-dir (agent-shell--dot-subdir "worktrees"))
         (worktree-name (agent-shell-worktree--generate-name))
         (default-path (file-name-concat worktrees-dir worktree-name))
         (worktree-path (expand-file-name (read-directory-name
                                           "Worktree directory: "
                                           default-path))))
    (when (file-exists-p worktree-path)
      (user-error "Directory already exists: %s" worktree-path))
    (make-directory (file-name-directory worktree-path) t)
    ;; Create the worktree
    (let ((output (shell-command-to-string
                   (format "git worktree add %s 2>&1"
                           (shell-quote-argument worktree-path)))))
      (unless (file-exists-p worktree-path)
        (user-error "Failed to create worktree: %s" output))
      (let ((default-directory worktree-path))
        (my/agent-shell-start)))))

(defun agent-shell-list-and-select ()
  "Select from all agent-shell buffers."
  (interactive)
  (agent-shell--list-and-select-from (agent-shell-buffers)))

(defun agent-shell-list-and-select-project ()
  "Select from agent-shell buffers in the current project, including worktrees."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (agent-shell--list-and-select-from (agent-shell-project-buffers))))

(defun agent-shell-project-select-or-new ()
  "Select an agent-shell buffer in the current project, or start a new one.

Lists all agent-shell buffers in the current project (including
worktrees), with an extra \"New agent shell\" option as the last
completion candidate.  Picking it forces a new shell, equivalent to
`C-u M-x agent-shell'."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (agent-shell--list-and-select-from (agent-shell-repo-buffers) t)))

(defun agent-shell-in-project ()
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (buffers (agent-shell-repo-buffers))
         (buf (car buffers))
         (text (when buf (agent-shell--context :shell-buffer buf))))
    (if buf
        (progn
          (agent-shell--display-buffer buf)
          (when text
            (agent-shell--insert-to-shell-buffer :text text :shell-buffer buf)))
      (my/agent-shell-start))))

(defun agent-shell-repo-buffers ()
  "Return agent-shell buffers whose CWD is in the same git repo as the current buffer."
  (let ((worktree-paths (mapcar #'car (magit-list-worktrees)))
        (buffers (agent-shell-buffers)))
    (seq-filter (lambda (buf)
                  (seq-some (lambda (wt)
                              (string-prefix-p
                               (expand-file-name wt)
                               (buffer-local-value 'default-directory buf)))
                            worktree-paths))
                buffers)))

(defun my/project-cmd (cmd)
  "Return a named command wrapping CMD with project root as default-directory."
  (let ((fn (make-symbol (format "my/project--%s" cmd))))
    (fset fn (lambda ()
               (interactive)
               (let ((default-directory (project-root (project-current t))))
                 (call-interactively cmd))))
    fn))

(add-to-list 'project-switch-commands (list (my/project-cmd 'agent-shell)
                                            "Agent shell" "a a"))
(add-to-list 'project-switch-commands (list (my/project-cmd 'agent-shell-project-select-or-new)
                                            "Agent shell select" "a p"))
(add-to-list 'project-switch-commands (list (my/project-cmd 'my/agent-shell-start)
                                            "Agent shell new" "a n"))
(add-to-list 'project-switch-commands (list (my/project-cmd 'my/agent-shell-new-worktree-shell)
                                            "Agent shell new (in worktree)" "a N"))

(transient-define-prefix conf--agent-shell-menu ()
  "Transient menu for agent-shell commands."
  ["Agent Shell"
   ["Core"
    ("a" "Start agent shell" agent-shell)
    ("p" "Select project shell" agent-shell-project-select-or-new)
    ("s" "Select agent shell" agent-shell-list-and-select)
    ("m" "Set session mode" agent-shell-set-session-mode)
    ("v" "Set model" agent-shell-set-session-model)
    ("r" "Rename" agent-shell-rename-buffer)
    ("n" "New shell" my/agent-shell-start)
    ("N" "New shell (in worktree)" my/agent-shell-new-worktree-shell)]
   ["Send"
    ("f" "Send file" agent-shell-send-file)
    ("F" "Send current file" conf--agent-shell-send-current-file)
    ("c" "Prompt compose" agent-shell-prompt-compose)]
   ["Other"
    ("!" "Insert shell command output" agent-shell-insert-shell-command-output)
    ("t" "Transcripts" conf/agent-shell-transcripts)
    ("R" "Resume from transcript" agent-transcript-resume-session)]])

(global-set-key (kbd "C-c a") 'conf--agent-shell-menu)

;; tmp: fix completion of command with :
(defun my/agent-shell-allow-colon-in-commands (orig char-class trigger)
  (funcall orig (if (and (eq trigger ?/) (equal char-class "[:alnum:]_-"))
                    "[:alnum:]_:-"
                  char-class)
           trigger))

(advice-add 'agent-shell--completion-bounds :around
            #'my/agent-shell-allow-colon-in-commands)

(defconst my/agent-shell--agent-type-aliases
  '(("claude" . claude-code)
    ("claude-code" . claude-code)
    ("codex" . codex)
    ("opencode" . opencode)
    ("gemini" . gemini-cli)
    ("gemini-cli" . gemini-cli)
    ("copilot" . copilot)
    ("cursor" . cursor)
    ("goose" . goose)
    ("grok" . grok-build)
    ("grok-build" . grok-build)
    ("mistral" . mistral-vibe)
    ("mistral-vibe" . mistral-vibe)
    ("qwen" . qwen-code)
    ("qwen-code" . qwen-code)
    ("antigravity" . antigravity)
    ("auggie" . auggie)
    ("cline" . cline)
    ("codebuddy" . codebuddy)
    ("droid" . droid)
    ("hermes" . hermes)
    ("junie" . junie)
    ("kimi" . kimi)
    ("kiro" . kiro)
    ("le-chat" . mistral-vibe)
    ("omp" . omp)
    ("openclaw" . openclaw)
    ("pi" . pi)
    ("qoder" . qoder))
  "Alist mapping common agent type strings to identifier symbols.

Used by `agent-shell-make' to resolve an agent type string like
\"claude\" or \"codex\" to the corresponding agent identifier symbol.")

(defun my/agent-shell--agent-identifier (agent)
  "Resolve AGENT type to an identifier symbol.

AGENT can be:
- A string with a common alias (e.g., \"claude\", \"codex\", \"opencode\")
- A string with a full identifier name (e.g., \"claude-code\", \"gemini-cli\")
- A symbol identifier directly (e.g., \\='claude-code, \\='codex)

Returns the identifier symbol, or signals an error if AGENT is not
a string or symbol."
  (cond
   ((null agent)
    (error "Agent type cannot be nil"))
   ((symbolp agent) agent)
   ((stringp agent)
    (or (cdr (assoc-string agent my/agent-shell--agent-type-aliases))
        ;; Fall back to using the string as a symbol directly.
        (intern agent)))
   (t (error "Invalid agent type: %S (expected string or symbol)" agent))))

(defun my/raise-buffer (buffer)
  "Make BUFFER the most recent in `buffer-list' without displaying it."
  (interactive "bBuffer to raise: ")
  (let ((target (get-buffer buffer)))
    (dolist (b (buffer-list))
      (unless (eq b target)
        (bury-buffer-internal b)))))

(cl-defun my/agent-shell-start ()
  (interactive)
  (agent-shell--start
   :config (agent-shell-select-config :prompt "Start new agent: ")
   :session-strategy 'new
   :new-session t))

;;;###autoload
(cl-defun my/agent-shell-make (&key prompt name (agent "claude") directory)
  "Create a new agent shell programmatically, without displaying it.

PROMPT is an instruction prompt string to send to the agent
immediately after the shell is ready.  When nil or empty, no
prompt is sent and the shell simply starts.

NAME is the name for the shell buffer, formatted as \"Agent @ NAME\"
\(e.g., \"Agent @ refactor-auth\").  When nil or empty, a default
name is generated from the agent name and project.

AGENT is the agent type as a string (e.g., \"claude\", \"codex\",
\"opencode\")

DIRECTORY is the working directory path for the shell.  When nil,
uses `agent-shell-cwd' (typically the project root)."
  ;; Resolve agent type to a configuration.
  (let* ((identifier (my/agent-shell--agent-identifier agent))
         (config (agent-shell--resolve-config-designator identifier))
         (default-directory (if directory
                                (expand-file-name directory)
                              (agent-shell-cwd))))
    (unless config
      (error "Unknown agent type: %s (identifier: %s)" agent identifier))
    (unless (file-directory-p default-directory)
      (error "Directory does not exist: %s" default-directory))
    ;; Start the shell in the background.
    (let ((shell-buffer (agent-shell--start
                         :config config
                         :session-strategy 'new
                         :new-session t
                         :no-focus t)))
      (when (and name (not (string-empty-p (string-trim name))))
        (shell-maker-set-buffer-name shell-buffer
                                     (format "Agent @ %s" (string-trim name))))
      (when (and prompt (not (string-empty-p (string-trim prompt))))
        (agent-shell--insert-to-shell-buffer
         :text prompt
         :submit t
         :shell-buffer shell-buffer
         :no-focus t))
      (my/raise-buffer shell-buffer)
      shell-buffer)))

;; (use-package agent-recall
;;   :config
;;   (setq agent-recall-search-paths '("~/work"))
;;   (setq agent-recall-search-function 'consult-ripgrep)
;;   (add-hook 'agent-shell-mode-hook #'agent-recall-track-sessions))


;; Transcripts open as a plain User/Agent conversation.
(require 'agent-transcript-mode)

;;; Consult: list agent-shell transcripts

(defvar conf--agent-shell-transcript-dir
  (expand-file-name "agent-shell/transcripts/" user-emacs-directory)
  "Directory containing agent-shell transcript files.")

(defvar conf--agent-shell--transcript-cache nil
  "Cache of parsed transcript metadata, keyed by file path.
Each value is a plist with :started, :workdir, :agent, :title, :mtime.")

(defun conf--agent-shell--parse-frontmatter (file)
  "Parse YAML frontmatter from transcript FILE.
Returns an alist of (key . value) pairs, or nil if FILE has no
frontmatter (older transcripts)."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 2048) ; read first 2KB
      (goto-char (point-min))
      (when (looking-at-p "\\`---\n")
        (let (fields)
          (forward-line 1)
          (while (not (or (eobp) (looking-at-p "^---$")))
            (when (looking-at "^\\([^:]+\\):\\s-*\"\\([^\"]*\\)\"\\s-*$")
              (push (cons (match-string 1) (match-string 2)) fields))
            (forward-line 1))
          (when fields
            (nreverse fields)))))))

(defun conf--agent-shell--relative-time (iso-time)
  "Format ISO-TIME string as a relative age like \"5 minutes ago\"."
  (let* ((time (date-to-time iso-time))
         (elapsed (max 0 (float-time (time-since time))))
         (mins (/ elapsed 60))
         (hours (/ elapsed 3600))
         (days (/ elapsed 86400)))
    (cond
     ((< mins 1) "just now")
     ((< mins 60) (format "%d minute%s ago"
                         (round mins) (if (= 1 (round mins)) "" "s")))
     ((< hours 24) (format "%d hour%s ago"
                          (round hours) (if (= 1 (round hours)) "" "s")))
     (t (format "%d day%s ago"
                (round days) (if (= 1 (round days)) "" "s"))))))

(defun conf--agent-shell--transcript-metadata (file)
  "Return cached metadata plist for transcript FILE, parsing if needed."
  (unless conf--agent-shell--transcript-cache
    (setq conf--agent-shell--transcript-cache (make-hash-table :test 'equal)))
  (let ((mtime (file-attribute-modification-time (file-attributes file))))
    (let ((cached (gethash file conf--agent-shell--transcript-cache)))
      (if (and cached (time-equal-p (plist-get cached :mtime) mtime))
          cached
        (when-let* ((fm (conf--agent-shell--parse-frontmatter file)))
          (let ((meta (list :mtime mtime
                            :started (cdr (assoc "started" fm))
                            :workdir (cdr (assoc "working_directory" fm))
                            :agent (cdr (assoc "agent" fm))
                            :title (cdr (assoc "title" fm)))))
            (puthash file meta conf--agent-shell--transcript-cache)
            meta))))))

(defun conf--agent-shell--transcript-candidates ()
  "Build consult candidates for agent-shell transcripts.
Each candidate is a propertized string \"<relative-time>  <title>  <workdir>\"
with a `conf--agent-shell-file' text property holding the full path.
Sorted by modification time, most recent first."
  (when (file-directory-p conf--agent-shell-transcript-dir)
    (let ((files (directory-files conf--agent-shell-transcript-dir
                                  t "\\.md\\'" nil))
          candidates)
      ;; Sort by mtime, most recent first
      (setq files (sort files
                        (lambda (a b)
                          (time-less-p
                           (file-attribute-modification-time (file-attributes b))
                           (file-attribute-modification-time (file-attributes a))))))
      (dolist (file files)
        (when-let* ((meta (conf--agent-shell--transcript-metadata file))
                    (started (plist-get meta :started))
                    (workdir (plist-get meta :workdir)))
          (let* ((rel-time (conf--agent-shell--relative-time started))
                 (short-dir (abbreviate-file-name workdir))
                 (title (plist-get meta :title))
                 (cand (if (and title (not (string-empty-p title)))
                           (format "%-16s  %-30s  %s"
                                   (propertize rel-time 'face '(:foreground "cyan"))
                                   (propertize title 'face '(:foreground "yellow"))
                                   (propertize short-dir 'face '(:foreground "gray70")))
                         (format "%-16s  %s"
                                 (propertize rel-time 'face '(:foreground "cyan"))
                                 (propertize short-dir 'face '(:foreground "gray70"))))))
            (put-text-property 0 (length cand) 'conf--agent-shell-file file cand)
            (push cand candidates))))
      (nreverse candidates))))

(defun conf--agent-shell--transcript-lookup (selected candidates &rest _)
  "Look up SELECTED in CANDIDATES, return the transcript file path."
  (when-let* ((found (member selected candidates)))
    (get-text-property 0 'conf--agent-shell-file (car found))))

;;;###autoload
(defun conf/agent-shell-transcripts ()
  "List agent-shell transcripts with `consult'.
Shows relative time and working directory for each transcript.
Only transcripts with YAML frontmatter are listed."
  (interactive)
  (let ((candidates (conf--agent-shell--transcript-candidates)))
    (if (null candidates)
        (user-error "No agent-shell transcripts with frontmatter found")
      (let ((file (consult--read
                   candidates
                   :prompt "Agent shell transcript: "
                   :category 'conf-agent-shell-transcript
                   :require-match t
                   :sort nil
                   :state (consult--file-preview)
                   :preview-key '(:debounce 0.1 any)
                   :lookup #'conf--agent-shell--transcript-lookup
                   :history 'conf--agent-shell-transcript-history)))
        (when file
          (find-file file))))))

(defvar conf--agent-shell-transcript-history nil
  "History for `conf/agent-shell-transcripts'.")

(provide 'conf-agent-shell)
