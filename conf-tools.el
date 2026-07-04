;; -*- lexical-binding: t; -*-

(use-package calc
  :straight (:type built-in)
  :bind
  (:map calc-mode-map
        ("M-z" . calc-undo)
        ("M-Z" . calc-redo)
        ("i"   . calc-algebraic-entry)))

(use-package cus-edit
  :straight (:type built-in)
  :bind
  (:map custom-dirlocals-map
        ("M-s" . Custom-dirlocals-save)))

(use-package proced
  :straight (:type built-in)
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-descent t)
  (proced-format 'medium) ;; can be changed interactively with `F'
  (proced-filter 'user))   ;; can be changed interactively with `f'

(use-package verb
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "M-,") 'eww-back-url)
  (define-key eww-mode-map (kbd "C-M-,") 'eww-forward-url))

(use-package nhexl-mode)

(define-key hexl-mode-map (kbd "M-X") nil)
(define-key hexl-mode-map (kbd "M-q") nil)
(define-key hexl-mode-map (kbd "C-x C-h") nil)
(define-key hexl-mode-map (kbd "C-x C-j") nil)
(define-key hexl-mode-map (kbd "C-x C-k") nil)
(define-key hexl-mode-map (kbd "C-x C-l") nil)
(define-key hexl-mode-map (kbd "M-f") nil)

(use-package git-timemachine
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine")
  :config
  (add-hook 'git-timemachine-mode-hook #'font-lock-ensure)
  (add-hook 'git-timemachine-mode-hook #'meow--switch-to-motion))

(use-package sql
  :straight (:type built-in)
  :hook
  (sql-interactive-mode . toggle-truncate-lines)
  :config
  (modify-syntax-entry ?- "w" sql-mode-syntax-table))

(use-package sqlformat
  :defer t
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g"))

  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(use-package string-inflection
  :bind
  ("C-c _" . string-inflection-toggle))

(midnight-mode)

(defun string-edit-in-minibuffer ()
  "Edit the string in the minibuffer using string-edit."
  (interactive)
  (when (minibufferp)
    (let* ((string-at-point (minibuffer-contents-no-properties))
           (current-prompt (minibuffer-prompt))
           (capfs completion-at-point-functions)
           (cursor-pos (- (point) (minibuffer-prompt-end))))
      (string-edit
       current-prompt
       string-at-point
       (lambda (result)
         (with-current-buffer (window-buffer (minibuffer-window))
           (delete-minibuffer-contents)
           (insert result)
           (select-window (minibuffer-window))))
       :abort-callback (lambda ()
                         (select-window (minibuffer-window))))
      (with-current-buffer (get-buffer "*edit string*")
        (setq-local completion-at-point-functions capfs)
        (goto-char (point-max))
        (backward-char (- (length string-at-point) cursor-pos))))))

(define-key minibuffer-local-map (kbd "C-c C-e") 'string-edit-in-minibuffer)

(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

(use-package casual
  :custom
  (casual-timezone-datestamp-format "%a %b %-e %Y, %H:%M"))

(use-package minuet
  :bind
  (("C-c , c" . #'minuet-complete-with-minibuffer)
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ("M-p" . #'minuet-previous-suggestion)
   ("M-n" . #'minuet-next-suggestion)
   ("M-A" . #'minuet-accept-suggestion)
   ("M-a" . #'minuet-accept-suggestion-line))
  :config
  ;; (setq minuet-provider 'claude)

  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1)
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)

  (defun conf--block-minuet ()
    (not (bound-and-true-p meow-insert-mode)))

  (setq minuet-auto-suggestion-block-functions 'conf--block-minuet))

(defun conf--restart ()
  (interactive)
  (call-process "sh" nil nil nil "-c"
                (concat "emacsclient -e \"(kill-emacs)\" && "
                        "emacsclient -c -n -a \"\" && "
                        "sleep 1 && "
                        "emacsclient -e \"(progn (switch-to-buffer \\\"*Messages*\\\") (select-frame-set-input-focus (selected-frame)))\" &")))

(defun insert-project-file-path ()
  "Select a file from current project and insert its relative path at point."
  (interactive)
  (let* ((project (project-current))
         (root (project-root project))
         (files (project-files project))
         (relative-files (mapcar (lambda (f) (file-relative-name f root)) files))
         (selected (completing-read-multiple "Insert file: " relative-files nil t)))
    (insert (string-join selected " "))))

(global-set-key (kbd "C-c /") 'insert-project-file-path)

(use-package cycle-quotes
  :bind
  (("C-c '" . cycle-quotes)))


(use-package highlight-parentheses
  :hook
  (prog-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-colors '("firebrick1")))

(use-package crux
  :bind
  (("C-c C-u" . crux-upcase-region)
   ("C-c C-l" . crux-downcase-region)))

(use-package topsy
  :straight (topsy :type git :host github :repo "alphapapa/topsy.el")
  :hook
  (prog-mode . topsy-mode)
  :config
  (defun conf--topsy--beginning-of-defun ()
    (when (> (window-start) 1)
      (save-excursion
        (if (treesit-parser-list)
            (progn
              (goto-char (- (window-start) 1))
              (let* ((node (treesit-defun-at-point)))
                (when node
                  (goto-char (treesit-node-start node))
                  (beginning-of-line)
                  (font-lock-ensure (point) (pos-eol))
                  (buffer-substring (point) (pos-eol)))))
          (when (memq major-mode '(emacs-lisp-mode))
            (goto-char (window-start))
            (beginning-of-defun)
            (font-lock-ensure (point) (pos-eol))
            (buffer-substring (point) (pos-eol)))))))

  (setf (alist-get nil topsy-mode-functions) 'conf--topsy--beginning-of-defun)
  (setf (alist-get 'emacs-lisp-mode topsy-mode-functions) 'conf--topsy--beginning-of-defun))


(use-package dirvish
  :custom
  (dired-listing-switches "-alFh")
  :init
  (dirvish-override-dired-mode)
  ;; (dirvish-peek-mode)
  )

(use-package impatient-mode
  :defer t
  :config
  (setq httpd-port 8087))

(defun conf--is-jinx-library-available ()
  (let ((enchant-flags
         (condition-case nil
             (car (process-lines "pkg-config" "--cflags" "--libs" "enchant-2"))
           (error nil))))
    (when enchant-flags t)))

(use-package jinx
  :if (conf--is-jinx-library-available)
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         :map jinx-overlay-map
         ("M-p" . nil)
         ("M-n" . nil))
  :custom
  (jinx-languages "en_US fr")
  :config
  (with-eval-after-load 'gptel
    (defun jinx--gptel-ignored-p (start)
      "Return t if the word at START has the 'gptel property."
      (get-char-property start 'gptel))

    ;; Disable spell checking for llm response
    (add-to-list 'jinx--predicates 'jinx--gptel-ignored-p)))

(use-package sideline
  :custom
  (sideline-truncate t)
  (sideline-truncate-suffix "…")
  :init
  (add-hook 'after-save-hook #'sideline-render-this))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line) ; 'point to show errors only on point
                                              ; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake))
  (setq sideline-force-display-if-exceeds nil)
  (setq sideline-order-right 'down)

  (defun conf--sideline-stop-p ()
    (or (buffer-modified-p) (sideline-stop-p)))
  (setq sideline-inhibit-display-function #'conf--sideline-stop-p))

(defun conf--insert-emoji ()
  (interactive)
  (when (and (boundp 'meow-mode) (not (meow-insert-mode-p)))
    (meow-insert))
  (ns-do-show-character-palette))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  ;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'git-commit-post-finish-hook 'diff-hl-magit-post-refresh)
  (add-hook 'magit-post-commit-hook 'diff-hl-magit-post-refresh)

  ;; Fixes conflict with meow (n/p)
  ;; Make sure to add after initializing meow since it also add keymaps to emulation-mode-map-alists
  (require 'diff-hl-show-hunk-inline)
  (add-hook 'meow-global-mode-hook
            #'(lambda ()
                (setq emulation-mode-map-alists
                      (cl-remove-if (lambda (alist)
                                      (and (listp alist)
                                           (assq 'diff-hl-show-hunk-inline-transient-mode alist)))
                                    emulation-mode-map-alists))
                (add-to-list 'emulation-mode-map-alists
                                      `((diff-hl-show-hunk-inline-transient-mode
                                         . ,diff-hl-show-hunk-inline-transient-mode-map)))))
  :custom
  (diff-hl-show-hunk-inline-smart-lines nil)
  (diff-hl-show-hunk-inline-hide-hunk t)
  (diff-hl-update-async t)
  (diff-hl-disable-on-remote t))

;; Note: use dtrt-indent-undo to undo the guess
(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :custom
  (dtrt-indent-global-mode t)
  (dtrt-indent-min-relevant-lines 1)
  (dtrt-indent-verbosity 0))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "\C-c\C-e"))

(use-package browse-at-remote
  :config
  (defalias 'open-on-github #'browse-at-remote))

(use-package vundo
  :bind
  (("C-x u" . vundo)
   ("C-x C-u" . vundo)
   :map vundo-mode-map
   ("h" . vundo-backward)
   ("l" . vundo-forward)
   ("j" . vundo-next)
   ("k" . vundo-previous)))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg))

(use-package symbol-overlay
  :bind
  ([f7] . symbol-overlay-put)
  :init
  (setq symbol-overlay-inhibit-map t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-vcs-max-length 32)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-env-version nil))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package treemacs
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package with-editor
  :hook
  (vterm-mode . with-editor-export-editor))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(provide 'conf-tools)
