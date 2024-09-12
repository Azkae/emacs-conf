;;; emacs-conf --- Summary
;;; Commentary:
;;
;;; Code:

;; ------------------
;; bootstrap straight
;; ------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-demand t)
(setq package-enable-at-startup nil)

;; set load path
(setq conf--base-dir (file-name-directory (or load-file-name default-directory)))
(add-to-list 'custom-theme-load-path conf--base-dir)
(add-to-list 'load-path conf--base-dir)

(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

(load-if-exists "~/.emacs.d/secrets.el")

;; -------------------
;; base emacs settings
;; -------------------

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier nil))

(setq ring-bell-function 'ignore)

(require 'cl-lib)
(require 'cl)

(require 'paren)
(show-paren-mode)

;; save minibuffer history
(savehist-mode 1)

(global-auto-revert-mode)
(setq inhibit-startup-message t)
(setq c-toggle-auto-newline t)
(setq make-backup-files nil)
(delete-selection-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq native-comp-async-report-warnings-errors nil)
(global-unset-key (kbd "C-z"))
(normal-erase-is-backspace-mode 1)

;; speedup long lines
(setq bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(setq auto-window-vscroll nil)
(setq redisplay-skip-fontification-on-input t)
(global-so-long-mode 1)

;; treat camelCase as multiple words for cursor movement
(global-subword-mode)

;; prefer vertical splits
(setq split-width-threshold 140)

;; Trackpad horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; TODO: use bind-key: https://melpa.org/#/bind-key

;; basic keybindings
(global-set-key (kbd "C-f") "\C-a\C-a\C-@\C-e")
(global-set-key [C-return] 'newline)

(global-set-key (kbd "M-$") 'shrink-window)
(global-set-key (kbd "M-*") 'enlarge-window)
(global-set-key (kbd "M-à") 'shrink-window-horizontally)
(global-set-key (kbd "M-)") 'enlarge-window-horizontally)

(global-set-key (kbd "C-c b") 'pop-tag-mark)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key [C-backspace] 'delete-backward-char)

;; remove anoying keybindings
(global-set-key (kbd "C-x DEL") 'ignore)
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "M-t"))

(define-key key-translation-map (kbd "M-g") (kbd "C-g"))
; (define-key crm-local-completion-map (kbd "M-v") nil)

(global-set-key (kbd "C-c p b") 'profiler-start)
(global-set-key (kbd "C-c p r") 'profiler-report)
(global-set-key (kbd "C-c p e") 'profiler-stop)

;; don't ask confirmation for kill-buffer with process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defun kill-region-maybe()
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)))

(global-set-key (kbd "M-x") 'kill-region-maybe)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-d") "\C-a\C-@\C-e")
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-e") 'recenter)
(global-set-key (kbd "M-N") 'goto-line)
(global-set-key (kbd "M-k") 'compile)
(global-set-key (kbd "C-x C-c") nil)

(defun move-up (amount)
  (condition-case nil
      (scroll-down amount)
    (error nil))
  (previous-line amount))
(defun move-down (amount)
  (condition-case nil
      (scroll-up amount)
    (error nil))
  (next-line amount))

(global-set-key (kbd "M-<up>")   (lambda () (interactive) (move-up 4)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (move-down 4)))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(pixel-scroll-precision-mode)

(setq frame-resize-pixelwise t)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(global-set-key (kbd "M-q") nil)
(define-key prog-mode-map (kbd "M-q") nil)
(global-set-key (kbd "M-q M-c") 'copy-file-name-to-clipboard)

;; moving windows
(global-set-key (kbd "M-q <left>")  'windmove-left)
(global-set-key (kbd "M-q <right>") 'windmove-right)
(global-set-key (kbd "M-q <up>")    'windmove-up)
(global-set-key (kbd "M-q <down>")  'windmove-down)

(global-set-key (kbd "M-q M-<left>")  'windmove-left)
(global-set-key (kbd "M-q M-<right>") 'windmove-right)
(global-set-key (kbd "M-q M-<up>")    'windmove-up)
(global-set-key (kbd "M-q M-<down>")  'windmove-down)

;; fix some coding systems
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

;; Duplicate region
(defun duplicate-line-or-region (&optional n)
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
		      (buffer-substring (region-beginning) (region-end))
		    (prog1 (thing-at-point 'line)
		      (end-of-line)
		      (if (< 0 (forward-line 1))
			  (newline))))))
	(dotimes (i (abs (or n 1)))
	  (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position))))
	(if (> 0 n)
	    (comment-region (line-beginning-position) (line-end-position)))
	(forward-line 1)
	(forward-char pos)))))

(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "C-d") nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'delete-selection-mode 'delete-selection-pre-hook)

(global-set-key (kbd "C-d") 'duplicate-line-or-region)

;; better buffer names
(require 'uniquify)

;; setup cc mode
(c-add-style "better-cc-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (tab-width . 4)                 ; better reading of code written with tabs
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (inlambda . 0)
                                   (statement-case-open . +)
                                   (innamespace 0)
                                   (arglist-close 0)))))

(defun --cc-style-setup()
  (c-set-style "better-cc-style"))

(add-hook 'c-mode-hook '--cc-style-setup)
(add-hook 'c++-mode-hook '--cc-style-setup)

(defun --set-tab-width()
  (setq tab-width 4)
  (setq c-basic-offset 4))

(add-hook 'cmake-mode-hook '--set-tab-width)
(add-hook 'objc-mode-hook '--set-tab-width)
(setq-default tab-width 4)

(setq c-default-style "linux")

(add-to-list 'auto-mode-alist '("\\.mm?\\'" . objc-mode))

;; avoid boring buffers
(setq boring-buffers
      '("\\*EGLOT .*\\*"
        "\\*Warnings\\*"
        "\\*straight-.*\\*"
        "\\*Async-.*\\*"
        "\\*scratch.*\\*"
        "\\*Messages.*\\*"
        "\\*helm-.*\\*"
        "\\*helm .*\\*"
        "magit-process:.*"
        "\\*Flymake .*\\*"
        "\\*apheleia-.*\\*"
        "\\*Native-compile-Log\\*"
        "\\*help\\*"))

(defun is-buffer-valid (buffer-name)
  (not (cl-loop for boring-buffer in boring-buffers
                thereis (string-match boring-buffer buffer-name))))

(defun conf--skip-temp-buffers (func)
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (funcall func)
    (while
        (and
         (not (is-buffer-valid (buffer-name)))
         (not (equal bread-crumb (buffer-name))))
      (funcall func))))

(defun conf--next-buffer ()
  (interactive)
  (conf--skip-temp-buffers 'next-buffer))

(defun conf--prev-buffer ()
  (interactive)
  (conf--skip-temp-buffers 'previous-buffer))

(defun kill-this-buffer-avoid-boring ()
  (interactive)
  (kill-this-buffer)
  (when (not (is-buffer-valid (buffer-name)))
    (conf--skip-temp-buffers 'previous-buffer)))

(global-set-key [remap next-buffer] 'conf--next-buffer)
(global-set-key [remap previous-buffer] 'conf--prev-buffer)
(global-set-key [remap kill-this-buffer] 'kill-this-buffer-avoid-boring)

;; better performance
;; (setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(defun conf--backward-delete-word ()
  "Delete word backwards, and delete matching pair if at point."
  (interactive)
  (let ((pt (point)))
    (when (and (electric-pair-mode)
               (char-before)
               (char-after)
               (or (eq (matching-paren (char-before)) (char-after))
                   (and (eq (char-before) (char-after))
                        (eq (char-before) (string-to-char "\"")))))
      (delete-char 1)
      (delete-char -1))
    (delete-region (point) (progn (backward-word 1) (point)))))

(global-set-key (kbd "M-DEL") 'conf--backward-delete-word)

;; Avoid inadvertently changing font size
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "C-<wheel-up>") 'ignore)
(global-set-key (kbd "C-<wheel-down>") 'ignore)

;; --------------
;; setup packages
;; --------------

(use-package el-patch)

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(use-package undo-fu
  :custom
  (undo-fu-ignore-keyboard-quit t)
  (undo-fu-allow-undo-in-region t)
  :config
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-Z") 'undo-fu-only-redo))

(use-package flymake
  :bind
  (("C-c i f" . flymake-mode)
   :map flymake-mode-map
   ("C-c i l" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-indicator-type 'fringes)
  (flymake-show-diagnostics-at-end-of-line nil))

(defun conf--org-open-link-maybe()
  (interactive)
  (if (eq (car (org-element-context)) 'link)
      (call-interactively 'org-open-at-point)
    (call-interactively 'org-meta-return)))

(use-package org
  :bind
  (:map org-mode-map
        ("M-." . org-open-at-point)
        ("M-<return>" . conf--org-open-link-maybe)
        ("M-<up>"     . (lambda () (interactive) (move-up 4)))
        ("M-<down>"   . (lambda () (interactive) (move-down 4)))
        )
  :config
  (setq org-startup-folded t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select t)
  (modify-syntax-entry ?= "." org-mode-syntax-table)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t) (python . t))))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package diminish)

(diminish 'gcmh-mode)
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(diminish 'subword-mode)

(add-hook 'cc-mode-hook (lambda () (abbrev-mode -1)))

(electric-pair-mode)
(electric-indent-mode)

(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

(use-package vundo
  :bind
  (("C-x u" . vundo)))

(defun delete-slash ()
  (search-backward "/")
  (delete-region (point) (point-max)))

(defun delete-until-slash ()
  (interactive)
  (when (eq (char-before) ?/)
    (delete-slash))
  (delete-slash)
  (insert "/"))

;; performance regression on helm on mac os.
;; run on helm repo:
;; git revert 1ecefa3840aa5bdd8d4959d2c4efd3ea0e433f64 && git reset HEAD~1
(use-package helm
  :diminish helm-mode
  :bind
  (("M-f"         . helm-occur)
   ("C-x C-f"     . helm-find-files)
   ("C-x b"       . helm-mini)
   ("C-b"         . helm-resume)
   ("C-p"         . helm-buffers-list)
   ("M-X"         . helm-M-x)
   ("M-o"         . helm-find-files)
   ("M-O"         . helm-buffers-list)
   ("C-x c M-y"   . helm-show-kill-ring)
   :map helm-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   ("M-v"         . yank)
   ("C-i"         . helm-execute-persistent-action) ;tab
   ("M-z"         . helm-select-action) ;tab
   ([M-backspace] . conf--backward-delete-word)
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   ("DEL"         . nil)
   :map helm-occur-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   :map helm-find-files-map
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   ("M-e"         . helm-config--ff-open-vterm)
   ("C-x g"       . helm-config--ff-open-magit)
   ("M-E"         . helm-config--ff-open-vterm)
   ("M-r"         . helm-ff-run-rename-file)
   ("DEL"         . nil)
   ([M-backspace] . delete-until-slash)
   :map helm-read-file-map
   ([M-backspace] . delete-until-slash)
   :map helm-grep-map
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   ("<right>"     . nil)
   ("<left>"      . nil)
   ("DEL"         . nil))
  :config
  (setq
   helm-buffers-fuzzy-matching t
   helm-ff-newfile-prompt-p nil
   helm-split-window-inside-p t
   helm-echo-input-in-header-line t
   helm-move-to-line-cycle-in-source nil
   ;; Disable helm in minibuffer region completion (eval-expression for example)
   helm-mode-handle-completion-in-region nil
   helm-scroll-amount 6
   helm-find-files-ignore-thing-at-point t)
  (helm-mode))

;; see https://gist.github.com/PaulCapestany/15d6f04077c1a9bc98968a778d60956e to use ripgrep?
(use-package helm-ag
  :straight (helm-ag :type git :host github :repo "emacsorphanage/helm-ag"
                     :fork (:host github :repo "Azkae/emacs-helm-ag"))
  :bind
  (("M-R"         . helm-do-ag)
   ("M-F"         . helm-do-ag-buffers)
   :map helm-find-files-map
   ("M-R"         . helm-config--ff-run-helm-ag)
   :map helm-ag-mode-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   :map helm-do-ag-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   :map helm-ag-edit-map
   ("RET"         . helm-ag-mode-jump-other-window))
  :custom
  ;; (helm-ag-base-command "rg --vimgrep --no-heading --smart-case")
  (helm-ag-base-command "ag --nocolor --nogroup")
  )

;; (defun remove-helm-smartparens ()
;;   (smartparens-mode -1))

;; (add-hook 'helm-minibuffer-set-up-hook 'remove-helm-smartparens)

;; (defun remove-helm-electric-pair ()
;;   (electric-pair-local-mode -1))

;; (add-hook 'helm-minibuffer-set-up-hook 'remove-helm-electric-pair)

(defun helm-config--helm-do-ag-on-file-maybe(basename)
  (interactive)
  (let* ((basename (expand-file-name basename))
         (basename (if (not (file-directory-p basename))
                       (file-name-directory basename)
                     basename)))
    (helm-do-ag basename)))

(defun conf--vterm-toggle-insert-cd()
  (interactive)
  (conf--vterm-save-cd)
  ;; If the helm session was started from a vterm buffer,
  ;; insert the cd directly inside the vterm buffer
  (if (eq major-mode 'vterm-mode)
      (progn
        (vterm-send-string (concat " cd " (shell-quote-argument default-directory)) t)
        (vterm-send-return))
    (call-interactively #'vterm-toggle-cd-show)))

(defun open-vterm-action(basename)
  (interactive)
  (let* ((basename (expand-file-name basename))
         (basename (if (not (file-directory-p basename))
                       (file-name-directory basename)
                     basename))
         (default-directory basename))

    (conf--vterm-toggle-insert-cd)))

(defun open-magit-action(basename)
  (interactive)
  (let* ((basename (expand-file-name basename))
         (basename (if (not (file-directory-p basename))
                       (file-name-directory basename)
                     basename))
         (default-directory basename))

    (call-interactively #'magit)))

(defun helm-config--helm-do-ag-on-project-root(basename)
  (interactive)
  (helm-do-ag (projectile-project-root)))

(defun open-vterm-on-project-root-action(basename)
  (interactive)
  (let* ((default-directory (projectile-project-root)))
    (conf--vterm-toggle-insert-cd)))

(defun helm-config--ff-open-magit()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'open-magit-action)))

(defun helm-config--ff-open-vterm()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'open-vterm-action)))

(defun helm-config--ff-open-vterm-root()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'open-vterm-on-project-root-action)))

(defun helm-config--ff-run-helm-ag()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-config--helm-do-ag-on-file-maybe)))

(defun helm-config--ff-run-helm-ag-root()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-config--helm-do-ag-on-project-root)))

(add-hook
 'helm-find-files-after-init-hook
 (lambda ()
   (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-file-maybe helm-source-find-files)
   (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-find-files)
   (helm-add-action-to-source "Open magit" 'open-magit-action helm-source-find-files)))

(with-eval-after-load "helm-projectile"
  (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-file-maybe helm-source-projectile-projects)
  (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-project-root helm-source-projectile-files-list)
  (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-projectile-projects)
  (helm-add-action-to-source "Open vterm on project root" 'open-vterm-on-project-root-action helm-source-projectile-files-list)
  (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-projectile-files-list)
  (helm-add-action-to-source "Open magit" 'open-magit-action helm-source-projectile-files-list)
  (helm-add-action-to-source "Open magit" 'open-magit-action helm-source-projectile-projects))

(defun helm-config--helm-do-ag-projectile-project-symbol ()
  (interactive)
    (helm-do-ag (projectile-project-root) nil (symbol-name (symbol-at-point))))

(define-key text-mode-map (kbd "M-.") 'helm-config--helm-do-ag-projectile-project-symbol)

;; Waiting for dumb-jump to support xref-find-references: https://github.com/jacktasia/dumb-jump/issues/433
(define-key text-mode-map (kbd "M-?") 'helm-config--helm-do-ag-projectile-project-symbol)
(define-key prog-mode-map (kbd "M-?") 'helm-config--helm-do-ag-projectile-project-symbol)

(defun projectile-run-compile ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (call-interactively 'compile)))

(defun projectile-run-lldb ()
  "Invoke `gdb' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (call-interactively 'lldb)))

(setq projectile-keymap-prefix (kbd "M-p"))
(use-package projectile
  :diminish projectile-mode
  :bind
  (:map projectile-mode-map
   ("M-p k" . projectile-run-compile)
   ("M-p d" . projectile-run-lldb))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil
        projectile-svn-command projectile-generic-command)
  (setq projectile-mode-line "Projectile") ;avoid lag in tramp
  (add-hook 'projectile-mode-hook
            (lambda ()
              (remove-hook 'find-file-hook #'projectile-cache-files-find-file-hook t)
              (remove-hook 'find-file-hook #'projectile-visit-project-tags-table t))))

(setq helm-projectile-fuzzy-match nil)
(use-package helm-projectile
  :bind
  (:map helm-projectile-find-file-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   ([M-backspace] . conf--backward-delete-word)
   ("M-e"         . helm-config--ff-open-vterm)
   ("M-E"         . helm-config--ff-open-vterm-root)
   ("M-R"         . helm-config--ff-run-helm-ag-root)
   ("M-p"         . previous-history-element)
   ("C-x g"       . helm-config--ff-open-magit)
   :map helm-projectile-projects-map
   ("M-e"         . helm-config--ff-open-vterm)
   ("M-R"         . helm-config--ff-run-helm-ag)
   ("C-x g"       . helm-config--ff-open-magit))
  :config
  (setq projectile-completion-system 'helm))
(helm-projectile-on)
;; must be binded after (helm-projectile-on) because it remap projectile keybindings
(define-key projectile-mode-map [remap projectile-ag] 'helm-do-ag-project-root)

(use-package dumb-jump
  :init
  ;; (define-key prog-mode-map (kbd "M-.") 'xref-find-definitions)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'ag))

(use-package symbol-overlay
  :bind
  ([f7] . symbol-overlay-put)
  :custom
  (symbol-overlay-inhibit-map t))

(use-package cmake-mode
  :bind
  (:map cmake-mode-map
        ;; dump-jump doesn't work on cmake
        ("M-." . helm-config--helm-do-ag-projectile-project-symbol))
  :config
  (setq cmake-tab-width 4))

;; (use-package powerline
;;   :config
;;   (powerline-default-theme)
;;   (setq powerline-display-buffer-size nil
;;         powerline-display-mule-info   nil
;;         powerline-display-hud         nil))

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

(use-package zoom-frm
  :bind
  (("C-x C-+" . zoom-in)
   ("C-x C--" . zoom-out)))

(global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-in)
(global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-out)

;; (use-package git-gutter-fringe+
;;   :diminish git-gutter+-mode
;;   :config
;;   (global-git-gutter+-mode))

;; (use-package git-gutter-fringe
;;   :diminish git-gutter-mode
;;   :config
;;   (global-git-gutter-mode))

(use-package phi-search
  :config
  (setq phi-search-limit 10000))

(use-package multiple-cursors
  :bind
  (("C-j"      . mc/mark-next-like-this)
   ("M-j"      . mc/mark-next-symbol-like-this)
   ("C-n"      . mc/skip-to-next-like-this)
   ("C-S-n"    . mc/unmark-previous-like-this)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click)
   :map python-mode-map
   ("C-j"      . mc/mark-next-like-this)
   :map mc/keymap
   ("<return>" . nil)
   ("M-v" . nil))
  :config
  (add-to-list 'mc/unsupported-minor-modes 'electric-indent-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (format "%s/snippets" conf--base-dir)))
  (yas-global-mode 1))

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x v l" . magit-log-buffer-file))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-visit-avoid-head-blob t)
  (magit-auto-revert-immediately t)
  (magit-bury-buffer-function (lambda (_) (magit-mode-quit-window t)))
  (vc-display-status nil))
(setq smerge-command-prefix "\C-cv")

(use-package request)

(defun conf--create-pull-request-github (repo branch)
  "Create a new PR on Github."
  (browse-url
   (format "https://github.com/%s/pull/new/%s" repo branch)))

(defun conf--show-pull-request-github (repo number)
  "Visit the current branch's PR on Github."
  (browse-url
   (format "https://github.com/%s/pull/%s" repo number)))

(defun conf--get-current-repo ()
  (replace-regexp-in-string
   "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
   (magit-get "remote"
              (magit-get-push-remote)
              "url")))

(defun conf--visit-circle-ci ()
  (interactive)
  (let ((repo (conf--get-current-repo))
        (branch (magit-get-current-branch)))
    (browse-url (format "https://app.circleci.com/pipelines/github/%s?branch=%s" repo branch))))

(defun conf--visit-pull-request-url-github ()
  (interactive)
  (lexical-let ((repo (conf--get-current-repo))
         (branch (magit-get-current-branch))
         (commit (magit-rev-parse
                  (and magit-copy-revision-abbreviated "--short")
                  "HEAD")))
    (request (format "https://api.github.com/repos/%s/commits/%s/pulls"
                     repo commit)
      :headers `(("Authorization" . ,(concat "Bearer " github-token)))
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "Got error: %S" error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (if (not (equal (length data) 0))
                      (let* ((id (alist-get 'number (aref data 0))))
                        (conf--show-pull-request-github repo id))
                    (conf--create-pull-request-github repo branch)))))))

(defun conf--visit-pull-request-url-gitlab ()
  "Visit the current branch's PR on Gitlab."
  (interactive)
  (browse-url
   (format "https://%s/%s/-/merge_requests/new?merge_request%%5Bsource_branch%%5D=%s"
           (replace-regexp-in-string
            "\\`.+@\\(.+\\):.+\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (replace-regexp-in-string
            "\\`.+:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (url-hexify-string (magit-get-current-branch)))))

(eval-after-load 'magit
  '(define-key magit-mode-map "h"
     #'conf--visit-pull-request-url-github))

(eval-after-load 'magit
  '(define-key magit-mode-map "H"
     #'conf--visit-circle-ci))

(straight-use-package '(git-timemachine :type git :host github :repo "emacsmirror/git-timemachine"))

(use-package magit-delta
  :straight (magit-delta :type git :host github :repo "dandavison/magit-delta"
                     :fork (:host github :repo "jumper047/magit-delta"))

  :if (executable-find "delta")
  ;; :hook (magit-mode . magit-delta-mode)
  :bind
  (:map magit-mode-map
        ("," . #'conf--magit-delta-toggle))
  :custom
  (magit-delta-default-dark-theme "Monokai Extended")
  (magit-delta-default-light-theme "Github")
  (magit-delta-hide-plus-minus-markers nil)
  :config

  (defun conf--magit-delta-toggle ()
    "Toggle magit-delta-mode and refresh magit."
    (interactive)
    (progn
      (call-interactively 'magit-delta-mode)
      (magit-refresh)))

  (add-hook 'magit-delta-mode-hook
            (lambda ()
              (if magit-delta-mode
                  (progn
                    ;; For some reason (face-attribute 'diff-added :background) does not work if called top-level
                    (setq magit-delta-delta-args `("--max-line-distance" "0.6" "--true-color" "always" "--color-only"
                                                   "--plus-style" ,(format "syntax \"%s\"" (face-attribute 'diff-added :background))
                                                   "--plus-emph-style" ,(format "syntax \"%s\"" (face-attribute 'diff-refine-added :background))
                                                   "--minus-emph-style" ,(format "syntax \"%s\"" (face-attribute 'diff-refine-removed :background))
                                                   "--minus-style" ,(format "normal \"%s\"" (face-attribute 'diff-removed :background))))

                    (setq
                     conf--saved-magit-diff-added-highlight (face-attribute 'magit-diff-added-highlight :background)
                     conf--saved-magit-diff-added (face-attribute 'magit-diff-added :background)
                     conf--saved-magit-diff-removed-highlight (face-attribute 'magit-diff-removed-highlight :background)
                     conf--saved-magit-diff-removed (face-attribute 'magit-diff-removed :background))

                    (set-face-attribute 'magit-diff-added-highlight nil
                                        :background (face-attribute 'diff-added :background))
                    (set-face-attribute 'magit-diff-added nil
                                        :background (face-attribute 'diff-added :background))
                    (set-face-attribute 'magit-diff-removed-highlight nil
                                        :background (face-attribute 'diff-removed :background))
                    (set-face-attribute 'magit-diff-removed nil
                                        :background (face-attribute 'diff-removed :background))

                    (setq face-remapping-alist
                          (seq-difference face-remapping-alist
                                          '((magit-diff-removed . default)
                                            (magit-diff-removed-highlight . default)
                                            (magit-diff-added . default)
                                            (magit-diff-added-highlight . default)))))

                (set-face-attribute 'magit-diff-added-highlight nil
                                    :background conf--saved-magit-diff-added-highlight)
                (set-face-attribute 'magit-diff-added nil
                                    :background conf--saved-magit-diff-added)
                (set-face-attribute 'magit-diff-removed-highlight nil
                                    :background conf--saved-magit-diff-removed-highlight)
                (set-face-attribute 'magit-diff-removed nil
                                    :background conf--saved-magit-diff-removed))))
  )

(use-package quickrun
  :bind
  (("M-q r" . quickrun))
  :config
  (setq quickrun-timeout-seconds -1)
  (add-hook 'quickrun-after-run-hook
            (lambda()
              (with-current-buffer quickrun--buffer-name
                (read-only-mode -1)
                (end-of-buffer)
                (insert "\n-- End --")
                (read-only-mode +1))))
  )

(use-package yaml-mode
  :hook
  (yaml-mode . toggle-truncate-lines))

(use-package swift-mode)

(use-package cython-mode)

(use-package helm-tramp
  :bind
  (("C-c s" . helm-tramp))
  :config
  (setq tramp-default-method "ssh")
  (defun package-installed-p (v) nil))

(use-package glsl-mode)

(use-package fill-column-indicator
  :config
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  (setq fci-rule-column 120))

(use-package jinja2-mode)

(use-package nhexl-mode)

(use-package dockerfile-mode)

(use-package eglot
  ;; :straight nil
  :bind
  (:map eglot-mode-map
        ("M-." . xref-find-definitions)
        ("M-?" . xref-find-references)
        ("<mouse-2>" . eglot-code-actions-at-mouse))
  :hook
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . (lambda () (setq-local eglot-ignored-server-capabilities '(:inlayHintProvider))))
  ((typescript-ts-mode-hook tsx-ts-mode-hook) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  (rust-mode . eglot-ensure)
  :custom
  (eglot-report-progress t)
  ;; help with perf:
  (eglot-events-buffer-size 0)
  :config

  ;; (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--completion-style=detailed")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--completion-style=detailed" "--header-insertion-decorators=0" "--header-insertion=never")))
  (setq eldoc-echo-area-use-multiline-p 2)
  ;; Disable auto indent after '}' on cpp mode, may break a few things..
  ;; (remove-hook 'post-self-insert-hook 'eglot--post-self-insert-hook t)

  (fset #'jsonrpc--log-event #'ignore)

  (add-to-list 'eglot-stay-out-of 'company-backends)

  (defun conf--project-try-cargo-toml (dir)
    "Try to locate a Rust project."
    (when (locate-dominating-file dir "Cargo.toml")
      `(transient . ,dir)))

  (add-hook 'project-find-functions 'conf--project-try-cargo-toml nil nil)
  (setq-default eglot-workspace-configuration
                (lambda (&rest args)
                  (let ((root (locate-dominating-file default-directory "pyproject.toml")))
                    (when root
                      (let ((process-environment (cl-remove-if (lambda (element) (string-prefix-p "VIRTUAL_ENV=" element))
                                                               process-environment)))
                        (let* ((venv-full-path (string-trim (shell-command-to-string "poetry env info --path")))
                               (venv (file-name-nondirectory venv-full-path))
                               (venv-path (file-name-directory venv-full-path)))
                          `((:pyright .
                                      (:venvPath ,venv-path
                                       :venv ,venv)))))))))
  )

(use-package clang-format)

(use-package js
  :straight nil
  :bind
  (:map js-json-mode-map
        ("M-." . 'helm-config--helm-do-ag-projectile-project-symbol))
  :hook
  (js-json-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package rust-mode)

(use-package restclient)

(defun restclient-collapse-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (restclient-toggle-body-visibility)
      (restclient-jump-next))))

(defun smerge-next-safe ()
  (condition-case err
      (not (smerge-next))
    ('error
     nil)))

(defun goto-next-conflict ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (not (smerge-next-safe))
      (vc-find-conflicted-file)
      (if (eq buffer (current-buffer))
          (message "No conflicts found")
        (goto-char 0)
        (smerge-next-safe)))))

(global-set-key (kbd "M-q e") 'goto-next-conflict)

(add-hook 'restclient-mode-hook 'restclient-collapse-all)

(use-package emojify
  :config
  (emojify-set-emoji-styles '(unicode))
  (setq emojify-display-style 'unicode))

(use-package solidity-mode
  :config
  (add-hook 'solidity-mode-hook '--set-tab-with))

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g"))

  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(use-package vterm
  :init
  (setq vterm-keymap-exceptions '("M-q" "C-q" "C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "M-z" "M-X" "M-O" "M-e" "M-E"))
  :bind
  (:map vterm-mode-map
  ("M-z" . 'vterm-copy-mode)
  ("<mouse-1>" . 'vterm-copy-mode)
  ("M-f" . (lambda () (interactive) (vterm-copy-mode 1) (helm-occur)))
  :map vterm-copy-mode-map
  ("M-z" . 'vterm-copy-mode)
  ("M-v" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-yank)))
  ("C-c C-c" . (lambda () (interactive) (vterm-copy-mode -1) (vterm--self-insert)))
  ("DEL" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-send-backspace)))
  ("SPC" . 'vterm-copy-mode)
  )
  :config
  (define-key vterm-copy-mode-map [remap self-insert-command] #'(lambda() (interactive) (vterm-copy-mode -1)
                                                                  (vterm--self-insert)))
  (setq vterm-timer-delay 0.05)
  ;; (setq vterm-timer-delay 0.1)
  )

(defface conf--vterm-face
  '((t :family "Menlo" :height 120))
  "The basic fixed-pitch face."
  :group 'basic-faces)

(add-hook 'vterm-mode-hook
          (lambda ()
            (set
             (make-local-variable 'buffer-face-mode-face) 'conf--vterm-face)
            (buffer-face-mode t)))

(defun conf--vterm-save-cd()
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (cd-cmd (concat " cd " (shell-quote-argument dir))))
    (setq conf--vterm-cd-command cd-cmd)))

(defun conf--vterm-insert-cd()
  (interactive)
  (when conf--vterm-cd-command
        (vterm-send-string conf--vterm-cd-command t)
        (vterm-send-return)))

(defun conf--vterm-hide()
  (quit-restore-window)
  (vterm-toggle--bury-all-vterm)
  (let ((buf (vterm-toggle--recent-other-buffer)))
    (when buf
      (if (and (get-buffer-window buf)
               (not (eq (selected-window)
                        (get-buffer-window buf))))
          (select-window (get-buffer-window buf))
        (switch-to-buffer buf)))))

(defun conf--vterm-toggle()
  (interactive)
  (conf--vterm-save-cd)
  (if (and (not (derived-mode-p 'vterm-mode))
           (vterm-toggle--get-window))
      (vterm-toggle-show)
    (if (derived-mode-p 'vterm-mode)
        (conf--vterm-hide)
      (vterm-toggle))))

(use-package vterm-toggle
  :bind
  (("M-e" . conf--vterm-toggle)
   ("M-E" . (lambda () (interactive) (conf--vterm-save-cd) (vterm-toggle-cd)))
   :map vterm-mode-map
   ("M-E" . conf--vterm-insert-cd)
   ("<M-right>" . vterm-toggle-forward)
   ("<M-left>" . vterm-toggle-backward)
   :map vterm-copy-mode-map
   ("M-E" . (lambda () (interactive) (vterm-copy-mode -1) (conf--vterm-insert-cd)))
   ("<M-right>" . vterm-toggle-forward)
   ("<M-left>" . vterm-toggle-backward))
  :config
  (setq vterm-toggle-hide-method 'bury-all-vterm-buffer)
  (setq vterm-toggle-reset-window-configration-after-exit nil)
  :autoload vterm-toggle-cd-show vterm-toggle--get-window)

(use-package multi-vterm
  :bind
  (:map vterm-mode-map
        ("M-t" . multi-vterm)
        :map vterm-copy-mode-map
        ("M-t" . multi-vterm)))

(use-package helm-xref)

(use-package pyvenv)

(use-package kotlin-mode
  :config
  (modify-syntax-entry ?< "(>" kotlin-mode-syntax-table)
  (modify-syntax-entry ?> ")<" kotlin-mode-syntax-table))

(use-package groovy-mode)

(use-package string-inflection
  :bind
  ("C-c f" . string-inflection-toggle))

(use-package org-roam
  :diminish org-roam-mode
  :init
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (expand-file-name "~/Dropbox/org-roam"))
  (org-roam-verbose nil)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n t" . org-roam-dailies-goto-today)
   ("C-c n y" . org-roam-dailies-goto-yesterday)
   ("C-c n g" . org-roam-graph)
   :map org-mode-map
   (("C-c n i" . org-roam-node-insert))
   (("M-," . org-mark-ring-goto)))
  :config
  (defun org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (org-show-properties)
      (org-hide-properties)))

  ;; call org-hide-properties after inserting a new node
  (add-hook 'org-roam-post-node-insert-hook #'(lambda (_ _) (org-hide-properties))))

(defun get-string-from-file (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 4))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package markdown-mode)

(use-package with-editor
  :hook
  (vterm-mode . with-editor-export-editor))

(use-package treemacs
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-projectile)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package eldoc-box
  :bind
  (("M-§" . eldoc-box-help-at-point)))

;; (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)

(defun conf--corfu-active-p ()
  (and corfu-mode completion-in-region-mode))

(defun conf--corfu-reset()
  (interactive)
  (corfu-quit)
  (corfu--auto-complete-deferred))

(defun conf--corfu-post-command()
  "Refresh completion when prefix length is 3 and no candidates are found."
  (when (and corfu-mode completion-in-region-mode)
    (let* ((input (car corfu--input))
           (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
           (len (length str))
           (candidates corfu--candidates))
      (when (and (= len 3)
                 ;; (>= len 3)
                 ;; (= (% len 3) 0)
                 (not (try-completion str candidates)))
        (conf--corfu-reset)))))

(el-patch-feature corfu)

;; Disable completion starting with [
(with-eval-after-load 'corfu
  (el-patch-defun corfu--capf-wrapper (fun &optional prefix)
    "Wrapper for `completion-at-point' FUN.
The wrapper determines if the Capf is applicable at the current
position and performs sanity checking on the returned result.
For non-exclusive Capfs wrapper additionally checks if the
current input can be completed successfully.  PREFIX is a prefix
length override, set to t for manual completion."
    (pcase (funcall fun)
      ((and res `(,beg ,end ,table . ,plist))
       (and (integer-or-marker-p beg) ;; Valid Capf result
            (<= beg (point) end)      ;; Sanity checking
            ;; When auto completing, check the prefix length!
            (let ((len (or prefix
                           (el-patch-swap (plist-get plist :company-prefix-length)
                                          (and (not (eq (char-before) (string-to-char "[")))
                                               (plist-get plist :company-prefix-length)))
                           (- (point) beg))))
              (or (eq len t) (>= len corfu-auto-prefix)))
            ;; For non-exclusive Capfs, check for valid completion.
            (or (not (eq 'no (plist-get plist :exclusive)))
                (let* ((str (buffer-substring-no-properties beg end))
                       (pt (- (point) beg))
                       (pred (plist-get plist :predicate))
                       (md (completion-metadata (substring str 0 pt) table pred)))
                  ;; We use `completion-try-completion' to check if there are
                  ;; completions. The upstream `completion--capf-wrapper' uses
                  ;; `try-completion' which is incorrect since it only checks for
                  ;; prefix completions.
                  (completion-try-completion str table pred pt md)))
            (cons fun res)))))
  )

(use-package corfu
  :bind
  (("M-RET" . completion-at-point))
  (:map corfu-map
        ("C-a" . nil)
        ("C-e" . nil)
        ("<remap> <move-beginning-of-line>" . nil)
        ("<remap> <move-end-of-line>" . nil)
        ("C-s" . corfu-insert-separator)
        ("TAB" . corfu-expand)
        ("<tab>" . corfu-expand)
        ("RET" . corfu-insert)
        ("<ret>" . corfu-insert)
        ("M-RET" . conf--corfu-reset))
  :hook
  (corfu-mode . (lambda ()
                  (add-hook 'yas-keymap-disable-hook 'conf--corfu-active-p nil t)
                  (add-hook 'post-command-hook #'conf--corfu-post-command)))
  (git-commit-mode . (lambda ()
                       (corfu-mode -1)))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.5 . 0.0))
  (corfu-on-exact-match nil)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer no helm session are active."
    (when (not helm-alive-p)
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (add-hook 'after-save-hook #'corfu-quit)
  )

(add-to-list 'completion-styles-alist
             '(tab completion-basic-try-completion ignore
               "Completion style which provides TAB completion only."))


(use-package orderless
  :init
  (setq completion-styles '(tab orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(defun conf--setup-simple-completion()
  (setq-local completion-at-point-functions conf--basic-completion-functions))

(when (< emacs-major-version 29)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package company)

(use-package cape
  :hook
  ;; (python-mode . conf--setup-simple-completion)
  (emacs-lisp-mode . conf--setup-simple-completion)
  (sh-mode . conf--setup-simple-completion)
  :init
  (setq conf--basic-completion-functions `(cape-file ,(cape-company-to-capf 'company-dabbrev-code) cape-keyword))
  (setq completion-at-point-functions conf--basic-completion-functions))

(use-package apheleia
  :hook
  ((python-mode python-ts-mode) . apheleia-mode)
  ((c++-mode c++-ts-mode) . apheleia-mode)
  (terraform-mode . apheleia-mode)
  ((typescript-ts-mode tsx-ts-mode) . apheleia-mode)
  :init
  ;; (apheleia-global-mode +1)
  )

;; Disable python-ts-mode:
(defun remove-from-alist-by-value (alist value)
  "Remove the first entry with VALUE from ALIST."
  (cl-delete value alist :key (lambda (elt) (cdr elt)) :test #'equal))

(defun remove-python-ts-mode ()
  (interactive)
  (setq auto-mode-alist (remove-from-alist-by-value auto-mode-alist 'python-ts-mode)))

(with-eval-after-load 'python
  (remove-python-ts-mode))

(add-hook 'python-mode 'remove-python-ts-mode)

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :custom
  (fancy-compilation-override-colors nil))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (goto-char 1)
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (quit-window nil (get-buffer-window buf)))
                      buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package zygospore
  :bind
  (("C-x 1" . zygospore-toggle-delete-other-windows)))

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
(use-package wgrep-helm)

(use-package browse-at-remote
  :config
  (defalias 'open-on-github #'browse-at-remote))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  ;; TODO: re-enable when issue https://github.com/dgutov/diff-hl/issues/213 is fixed
  (diff-hl-update-async nil)
  (diff-hl-disable-on-remote t))

;; Poetry project tracking

(defvar conf--poetry-current-root nil)
(defun conf--poetry-track-virtualenv()
  (let ((root (locate-dominating-file default-directory "pyproject.toml")))
    (when (and root (not (string= root conf--poetry-current-root)))
      (let ((process-environment (cl-remove-if
                                  (lambda (element) (string-prefix-p "VIRTUAL_ENV=" element))
                                  process-environment)))
            (let ((venv (string-trim (shell-command-to-string "poetry env info --path"))))
              (message "Applying venv: %s" venv)
              (setq conf--poetry-current-root root)
              (pyvenv-activate venv))))))

(define-minor-mode conf--poetry-tracking-mode
  "Global mode to track poetry projects"
  :global t
  :group 'poetry
  (if conf--poetry-tracking-mode
      (add-hook 'projectile-after-switch-project-hook 'conf--poetry-track-virtualenv)
    (remove-hook 'projectile-after-switch-project-hook 'conf--poetry-track-virtualenv)))
(conf--poetry-tracking-mode)

(use-package treesit-auto
  :custom
  (treesit-auto-langs '(typescript tsx python)))

(defun conf--move-buffer (dir)
  (let* ((buf (current-buffer))
         (window (windmove-find-other-window dir)))
    (when (and window (not (window-minibuffer-p window)))
      (conf--prev-buffer)
      (windmove-do-window-select dir)
      (switch-to-buffer buf))))

(defun conf--move-buffer-left ()
  (interactive)
  (conf--move-buffer 'left))

(defun conf--move-buffer-right ()
  (interactive)
  (conf--move-buffer 'right))

(defun conf--move-buffer-up ()
  (interactive)
  (conf--move-buffer 'up))

(defun conf--move-buffer-down ()
  (interactive)
  (conf--move-buffer 'down))


(global-set-key (kbd "<C-M-left>") 'conf--move-buffer-left)
(global-set-key (kbd "<C-M-right>") 'conf--move-buffer-right)
(global-set-key (kbd "<C-M-up>") 'conf--move-buffer-up)
(global-set-key (kbd "<C-M-down>") 'conf--move-buffer-down)

(use-package dape
  :init
  (setq dape-key-prefix "\C-xd")
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  (repeat-mode 1))
;; Build and run in debugger:
;;   codelldb-cc :cwd "/path/base_dir" :program "/path/base_dir/program" :args ["test"] compile "make -k"

;; Attach running process:
;;   codelldb-cc :cwd "/path/base_dir" :program "/path/base_dir/program" :request "attach" :pid <pid>

(use-package gdscript-mode
  :hook (gdscript-mode . eglot-ensure))

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  ;; (set-face-attribute 'org-modern-bracket-line nil :family "Menlo")
  )

(use-package terraform-mode
  :hook
  (terraform-mode . eglot-ensure))

(use-package casual
  :bind
  (:map calc-mode-map
        ("SPC" . casual-main-menu)))

;; Disable M-o key in html
(eval-after-load 'mhtml-mode
  (add-hook 'mhtml-mode-hook #'(lambda() (bind-key "M-o" nil mhtml-mode-map))))

(which-function-mode)

(with-eval-after-load 'nxml-mode
  (modify-syntax-entry ?< "." nxml-mode-syntax-table)
  (modify-syntax-entry ?> "." nxml-mode-syntax-table)
  (modify-syntax-entry ?/ "." nxml-mode-syntax-table))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode))

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defalias 'conf--insert-emoji 'ns-do-show-character-palette)

;; ;; TODO: try https://github.com/jdtsmith/indent-bars
;; TODO: disable eglot when viewing magit commit
;; TODO: test direnv
;; TODO (longshot): try meowing https://github.com/meow-edit/meow

;; load graphic settings
(require 'graphics)
(require 'custom-python-highlighting)
