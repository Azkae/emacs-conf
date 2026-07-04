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
  :config
  (with-eval-after-load 'shell-maker
    (define-key agent-shell-mode-map (kbd "C-M-h") nil))
  (setq agent-shell-anthropic-default-session-mode-id "plan")
  (advice-add 'agent-shell--project-files :override #'conf--project-files-cached-current)
  (setq agent-shell-highlight-blocks t)
  (setq agent-shell-session-strategy 'prompt)
  (setq markdown-overlays-prettify-tables t)

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

(setq agent-shell-context-sources '(files region error))

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

(setq agent-shell-transcript-file-path-function #'my/agent-shell-transcript-file-path)

(defun conf--agent-shell-send-current-file ()
  (interactive)
  (when (buffer-file-name)
    (agent-shell--insert-to-shell-buffer
     :text (agent-shell--get-files-context :files (list (buffer-file-name))))))

(defun agent-shell--annotate-buffer (name)
  "Annotate agent-shell buffer NAME with status and project for completion."
  (when-let* ((buf (get-buffer name)))
    (with-current-buffer buf
      (format "  %-8s %s"
              (if (shell-maker-busy) "⏳ busy" "✅ idle")
              (abbreviate-file-name (or (agent-shell-cwd) "?"))))))

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

When ALLOW-NEW is non-nil, append a \"New agent shell\" entry as the
last completion candidate.  Picking it forces a new shell, equivalent
to \\<agent-shell-mode-map>\\`C-u M-x agent-shell'."
  (let* ((new-label "➕ New agent shell")
         (names (mapcar #'buffer-name buffers))
         (candidates (if allow-new (append names (list new-label)) names)))
    (if (null candidates)
        (if allow-new
            (agent-shell-new-shell)
          (message "No agent-shell buffers found."))
      (let* ((vertico-sort-function nil) ; keep "New agent shell" last
             (table (lambda (str pred action)
                     (if (eq action 'metadata)
                         '(metadata (category . agent-shell-buffer)
                                    (annotation-function . agent-shell--annotate-buffer))
                       (complete-with-action action candidates str pred))))
             (choice (completing-read "Agent shell: " table nil t))
             (buf (unless (equal choice new-label) (get-buffer choice)))
             ;; Capture context (region/files/error/line) from the
             ;; *current* (source) buffer before switching away, mirroring
             ;; what `agent-shell--dwim' does for the default entry point.
             (text (when buf (agent-shell--context :shell-buffer buf))))
        (cond
         ((equal choice new-label) (agent-shell-new-shell))
         (buf
          (agent-shell--display-buffer buf)
          (when text
            (agent-shell--insert-to-shell-buffer :text text :shell-buffer buf))))))))

(defun agent-shell-list-and-select ()
  "Select from all agent-shell buffers."
  (interactive)
  (agent-shell--list-and-select-from (agent-shell-buffers)))

(defun agent-shell-list-and-select-project ()
  "Select from agent-shell buffers in the current project, including worktrees."
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (agent-shell--list-and-select-from (agent-shell-repo-buffers))))

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
      (call-interactively 'agent-shell))))

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

(add-to-list 'project-switch-commands '(agent-shell-list-and-select-project "Agent shell select" "a s"))
(add-to-list 'project-switch-commands '(agent-shell-project-select-or-new "Agent shell" "a a"))

(transient-define-prefix conf--agent-shell-menu ()
  "Transient menu for agent-shell commands."
  ["Agent Shell"
   ["Core"
    ("a" "Start agent shell" agent-shell-project-select-or-new)
    ("s" "Select agent shell" agent-shell-list-and-select)
    ("m" "Set session mode" agent-shell-set-session-mode)
    ("v" "Set model" agent-shell-set-session-model)]
   ["Send"
    ("f" "Send file" agent-shell-send-file)
    ("F" "Send current file" conf--agent-shell-send-current-file)
    ("c" "Prompt compose" agent-shell-prompt-compose)]
   ["Other"
    ("!" "Insert shell command output" agent-shell-insert-shell-command-output)]])

(global-set-key (kbd "C-c a") 'conf--agent-shell-menu)

(provide 'conf-agent-shell)
