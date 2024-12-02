;;; emacs-conf --- Summary -*- lexical-binding: t; -*-
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
(setq delete-active-region t)

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

;; Don't spawn another window during ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Trackpad horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; TODO: use bind-key: https://melpa.org/#/bind-key

;; basic keybindings
(global-set-key (kbd "C-f") "\C-a\C-a\C-@\C-e")
(global-set-key [C-return] 'newline)
(global-set-key (kbd "C-h") nil)

(global-set-key (kbd "M-$") 'shrink-window)
(global-set-key (kbd "M-*") 'enlarge-window)
(global-set-key (kbd "M-à") 'shrink-window-horizontally)
(global-set-key (kbd "M-)") 'enlarge-window-horizontally)

(global-set-key (kbd "M-L") 'downcase-word)
(global-set-key (kbd "M-U") 'upcase-word)

(global-set-key (kbd "C-c b") 'pop-tag-mark)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key [C-backspace] 'delete-backward-char)

;; remove anoying keybindings
(global-set-key (kbd "C-x DEL") 'ignore)
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "M-t"))

(define-key key-translation-map (kbd "M-g") (kbd "C-g"))
; (define-key crm-local-completion-map (kbd "M-v") nil)

(global-set-key (kbd "C-c p b") 'profiler-start)
(global-set-key (kbd "C-c p r") 'profiler-report)
(global-set-key (kbd "C-c p e") 'profiler-stop)

(global-set-key (kbd "C-x C-o") 'other-window)

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
(global-set-key (kbd "M-N") 'goto-line)
(global-set-key (kbd "M-a") 'recenter)
;; (global-set-key (kbd "M-k") 'compile)
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-S-x C-S-c") 'save-buffers-kill-terminal)

(global-set-key (kbd "<mouse-3>") 'xref-find-definitions)
(global-set-key (kbd "<mouse-4>") 'xref-go-back)

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
(global-set-key (kbd "M-j")   (lambda () (interactive) (move-up 4)))
(global-set-key (kbd "M-k") (lambda () (interactive) (move-down 4)))
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
(setq read-process-output-max (* 64 1024)) ;; 64k

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

(use-package el-patch
  :custom
  (el-patch-use-aggressive-defvar t))

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

;; (defun conf--toggle-flymake-end-of-line ()
;;   (interactive)
;;   (if (eq flymake-show-diagnostics-at-end-of-line nil)
;;       (progn (message "Enabled end-of-line diagnostics")
;;              (setq-local flymake-show-diagnostics-at-end-of-line 'short))
;;     (message "Disabled end-of-line diagnostics")
;;     (setq-local flymake-show-diagnostics-at-end-of-line nil))
;;   (flymake-mode -1)
;;   (flymake-mode 1))

(use-package flymake
  :bind
  (("C-c i f" . flymake-mode)
   ;; ("C-c i r" . conf--toggle-flymake-end-of-line)
   :map flymake-mode-map
   ("C-c i l" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-indicator-type 'fringes)
  ;; (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-no-changes-timeout nil))

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

(defun delete-until-slash-maybe ()
  (interactive)
  (if (eq 'file (vertico--metadata-get 'category))
      (delete-until-slash)
    (conf--backward-delete-word)))

;; ;; performance regression on helm on mac os.
;; ;; run on helm repo:
;; ;; git revert 1ecefa3840aa5bdd8d4959d2c4efd3ea0e433f64 && git reset HEAD~1
;; (use-package helm
;;   :diminish helm-mode
;;   :bind
;;   (("M-f"         . helm-occur)
;;    ("C-x C-f"     . helm-find-files)
;;    ("C-x b"       . helm-mini)
;;    ("C-b"         . helm-resume)
;;    ("C-p"         . helm-buffers-list)
;;    ("M-X"         . helm-M-x)
;;    ("M-o"         . helm-find-files)
;;    ("M-O"         . helm-buffers-list)
;;    ("C-x c M-y"   . helm-show-kill-ring)
;;    :map helm-map
;;    ("<right>"     . nil)
;;    ("<left>"      . nil)
;;    ("M-v"         . yank)
;;    ("C-i"         . helm-execute-persistent-action) ;tab
;;    ("M-z"         . helm-select-action) ;tab
;;    ([M-backspace] . conf--backward-delete-word)
;;    ("<M-down>"    . helm-scroll-other-window)
;;    ("<M-up>"      . helm-scroll-other-window-down)
;;    ("DEL"         . nil)
;;    :map helm-occur-map
;;    ("<right>"     . nil)
;;    ("<left>"      . nil)
;;    ("<M-down>"    . helm-scroll-other-window)
;;    ("<M-up>"      . helm-scroll-other-window-down)
;;    :map helm-find-files-map
;;    ("<M-down>"    . helm-scroll-other-window)
;;    ("<M-up>"      . helm-scroll-other-window-down)
;;    ("M-e"         . helm-config--ff-open-vterm)
;;    ("C-x g"       . helm-config--ff-open-magit)
;;    ("M-E"         . helm-config--ff-open-vterm)
;;    ("M-r"         . helm-ff-run-rename-file)
;;    ("DEL"         . nil)
;;    ([M-backspace] . delete-until-slash)
;;    :map helm-read-file-map
;;    ([M-backspace] . delete-until-slash)
;;    :map helm-grep-map
;;    ("<M-down>"    . helm-scroll-other-window)
;;    ("<M-up>"      . helm-scroll-other-window-down)
;;    ("<right>"     . nil)
;;    ("<left>"      . nil)
;;    ("DEL"         . nil))
;;   :config
;;   (setq
;;    helm-buffers-fuzzy-matching t
;;    helm-ff-newfile-prompt-p nil
;;    helm-split-window-inside-p t
;;    helm-echo-input-in-header-line t
;;    helm-move-to-line-cycle-in-source nil
;;    ;; Disable helm in minibuffer region completion (eval-expression for example)
;;    helm-mode-handle-completion-in-region nil
;;    helm-scroll-amount 6
;;    helm-find-files-ignore-thing-at-point t)
;;   (helm-mode))

;; ;; see https://gist.github.com/PaulCapestany/15d6f04077c1a9bc98968a778d60956e to use ripgrep?
;; (use-package helm-ag
;;   :straight (helm-ag :type git :host github :repo "emacsorphanage/helm-ag"
;;                      :fork (:host github :repo "Azkae/emacs-helm-ag"))
;;   :bind
;;   (("M-R"         . helm-do-ag)
;;    ("M-F"         . helm-do-ag-buffers)
;;    :map helm-find-files-map
;;    ("M-R"         . helm-config--ff-run-helm-ag)
;;    :map helm-ag-mode-map
;;    ("<right>"     . nil)
;;    ("<left>"      . nil)
;;    :map helm-do-ag-map
;;    ("<right>"     . nil)
;;    ("<left>"      . nil)
;;    :map helm-ag-edit-map
;;    ("RET"         . helm-ag-mode-jump-other-window))
;;   :custom
;;   ;; (helm-ag-base-command "rg --vimgrep --no-heading --smart-case")
;;   (helm-ag-base-command "ag --nocolor --nogroup")
;;   )

;; ;; (defun remove-helm-smartparens ()
;; ;;   (smartparens-mode -1))

;; ;; (add-hook 'helm-minibuffer-set-up-hook 'remove-helm-smartparens)

;; ;; (defun remove-helm-electric-pair ()
;; ;;   (electric-pair-local-mode -1))

;; ;; (add-hook 'helm-minibuffer-set-up-hook 'remove-helm-electric-pair)

;; (defun helm-config--helm-do-ag-on-file-maybe(basename)
;;   (interactive)
;;   (let* ((basename (expand-file-name basename))
;;          (basename (if (not (file-directory-p basename))
;;                        (file-name-directory basename)
;;                      basename)))
;;     (helm-do-ag basename)))

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

;; (defun open-vterm-action(basename)
;;   (interactive)
;;   (let* ((basename (expand-file-name basename))
;;          (basename (if (not (file-directory-p basename))
;;                        (file-name-directory basename)
;;                      basename))
;;          (default-directory basename))

;;     (conf--vterm-toggle-insert-cd)))

;; (defun open-magit-action(basename)
;;   (interactive)
;;   (let* ((basename (expand-file-name basename))
;;          (basename (if (not (file-directory-p basename))
;;                        (file-name-directory basename)
;;                      basename))
;;          (default-directory basename))

;;     (call-interactively #'magit)))

;; (defun helm-config--helm-do-ag-on-project-root(basename)
;;   (interactive)
;;   (helm-do-ag (projectile-project-root)))

;; (defun open-vterm-on-project-root-action(basename)
;;   (interactive)
;;   (let* ((default-directory (projectile-project-root)))
;;     (conf--vterm-toggle-insert-cd)))

;; (defun helm-config--ff-open-magit()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action 'open-magit-action)))

;; (defun helm-config--ff-open-vterm()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action 'open-vterm-action)))

;; (defun helm-config--ff-open-vterm-root()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action 'open-vterm-on-project-root-action)))

;; (defun helm-config--ff-run-helm-ag()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action 'helm-config--helm-do-ag-on-file-maybe)))

;; (defun helm-config--ff-run-helm-ag-root()
;;   (interactive)
;;   (with-helm-alive-p
;;     (helm-exit-and-execute-action 'helm-config--helm-do-ag-on-project-root)))

;; (add-hook
;;  'helm-find-files-after-init-hook
;;  (lambda ()
;;    (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-file-maybe helm-source-find-files)
;;    (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-find-files)
;;    (helm-add-action-to-source "Open magit" 'open-magit-action helm-source-find-files)))

;; (with-eval-after-load "helm-projectile"
;;   (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-file-maybe helm-source-projectile-projects)
;;   (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-project-root helm-source-projectile-files-list)
;;   (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-projectile-projects)
;;   (helm-add-action-to-source "Open vterm on project root" 'open-vterm-on-project-root-action helm-source-projectile-files-list)
;;   (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-projectile-files-list)
;;   (helm-add-action-to-source "Open magit" 'open-magit-action helm-source-projectile-files-list)
;;   (helm-add-action-to-source "Open magit" 'open-magit-action helm-source-projectile-projects))

;; (defun helm-config--helm-do-ag-projectile-project-symbol ()
;;   (interactive)
;;     (helm-do-ag (projectile-project-root) nil (symbol-name (symbol-at-point))))

;; (define-key text-mode-map (kbd "M-.") 'helm-config--helm-do-ag-projectile-project-symbol)

;; ;; Waiting for dumb-jump to support xref-find-references: https://github.com/jacktasia/dumb-jump/issues/433
;; (define-key text-mode-map (kbd "M-?") 'helm-config--helm-do-ag-projectile-project-symbol)
;; (define-key prog-mode-map (kbd "M-?") 'helm-config--helm-do-ag-projectile-project-symbol)

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
   ("M-p d" . projectile-run-lldb)
   ("M-p s s" . consult-ripgrep))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil
        projectile-svn-command projectile-generic-command
        projectile-current-project-on-switch 'keep)
  (setq projectile-mode-line "Projectile") ;avoid lag in tramp
  (add-hook 'projectile-mode-hook
            (lambda ()
              (remove-hook 'find-file-hook #'projectile-cache-files-find-file-hook t)
              (remove-hook 'find-file-hook #'projectile-visit-project-tags-table t))))

;; (setq helm-projectile-fuzzy-match nil)
;; (use-package helm-projectile
;;   :bind
;;   (:map helm-projectile-find-file-map
;;    ("<right>"     . nil)
;;    ("<left>"      . nil)
;;    ([M-backspace] . conf--backward-delete-word)
;;    ("M-e"         . helm-config--ff-open-vterm)
;;    ("M-E"         . helm-config--ff-open-vterm-root)
;;    ("M-R"         . helm-config--ff-run-helm-ag-root)
;;    ("M-p"         . previous-history-element)
;;    ("C-x g"       . helm-config--ff-open-magit)
;;    :map helm-projectile-projects-map
;;    ("M-e"         . helm-config--ff-open-vterm)
;;    ("M-R"         . helm-config--ff-run-helm-ag)
;;    ("C-x g"       . helm-config--ff-open-magit))
;;   :config
;;   (setq projectile-completion-system 'helm))
;; (helm-projectile-on)
;; ;; must be binded after (helm-projectile-on) because it remap projectile keybindings
;; (define-key projectile-mode-map [remap projectile-ag] 'helm-do-ag-project-root)

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
        ("M-." . conf--consult-ripgrep))
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
  (("M-m"      . mc/mark-next-like-this)
   ;; ("C-M"      . newline)
   ;; ("C-M-m"    . newline)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click)
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
  (setq quickrun-truncate-lines nil)
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

;; (use-package helm-tramp
;;   :bind
;;   (("C-c s" . helm-tramp))
;;   :config
;;   (setq tramp-default-method "ssh")
;;   (defun package-installed-p (v) nil))

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
  :straight nil
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
  (setq eldoc-echo-area-use-multiline-p nil)
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

  ;; Enable flymake only on same:
  ;; This allows to trigger flymake only when the sever published diagnostics
  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
        (when (and (eq nil flymake-no-changes-timeout)
                   (not (buffer-modified-p)))
          (flymake-start t)
          (when (bound-and-true-p sideline-mode)
            (sideline--reset)
            (sideline-render)))))))

(el-patch-feature eglot)

;; Will be fixed in next emacs/eglot release
;; See: https://github.com/emacs-mirror/emacs/commit/29d50e245f84d62a9cf4ce00593ea4c63fc4f44d
(with-eval-after-load 'eglot
  (el-patch-defun eglot--signal-textDocument/didOpen ()
    "Send textDocument/didOpen to server."
    (el-patch-add (eglot--track-changes-fetch eglot--track-changes))
    (setq eglot--recent-changes nil
          eglot--versioned-identifier 0
          eglot--TextDocumentIdentifier-cache nil)
    (jsonrpc-notify
     (eglot--current-server-or-lose)
     :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem)))))

(use-package clang-format)

(use-package js
  :straight nil
  :bind
  (:map js-json-mode-map
        ("M-." . 'conf--consult-ripgrep))
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

;; (use-package helm-xref)

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

(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t))

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
  (("M-§" . eldoc-box-help-at-point))
  :custom
  (eldoc-idle-delay 0.15))

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
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)
        ("M-j" . corfu-next)
        ("M-k" . corfu-previous)
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
  ((git-commit-mode gptel-mode) . (lambda ()
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
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
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
  (diff-hl-update-async t)
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

(use-package casual-calc
  :bind
  (:map calc-mode-map
        ("SPC" . casual-calc-tmenu)))

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

(use-package gptel)

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
  (setq sideline-inhibit-display-function #'conf--sideline-stop-p)
  )

(use-package elysium)


;; (use-package indent-bars
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   ;; Add other languages as needed
;;   (indent-bars-treesit-scope '((python function_definition class_definition for_statement
;; 	  if_statement with_statement while_statement))))


;; Enable vertico
(use-package vertico
  :custom
  (vertico-count 25)
  (vertico-scroll-margin 5)
  (vertico-resize nil)
  (projectile-completion-system 'default)
  :bind
  ("C-z" . vertico-suspend)
  ("M-b" . vertico-repeat)
  ("M-B" . vertico-repeat-select)
  (:map vertico-map
        ("M-p" . previous-history-element)
        ("M-n" . next-history-element)
        ;; ("M-r" . nil)
        ("M-<up>" . (lambda() (interactive) (scroll-other-window-down 5)))
        ("M-<down>" . (lambda() (interactive) (scroll-other-window 5)))
        ("M-k" . (lambda() (interactive) (scroll-other-window-down 5)))
        ("M-j" . (lambda() (interactive) (scroll-other-window 5)))
        ("C-SPC" . embark-select)
        ("M-p" . previous-history-element)
        ("M-n" . next-history-element)
        ("C-j" . next-line)
        ("C-k" . previous-line)
        ("C-h" . left-char)
        ("C-l" . right-char))
  (:map minibuffer-local-map
        ("C-p" . previous-history-element)
        ("C-n" . next-history-element))
  :init
  (vertico-mode)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (defun conf--minibuffer-complete-or-insert-directory ()
    (interactive)
    (if (eq (vertico--metadata-get 'category) 'file)
        (vertico-insert)
      (minibuffer-complete)))

  (keymap-set vertico-map "TAB" #'conf--minibuffer-complete-or-insert-directory))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(defun conf--consult-ripgrep ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(defun conf--minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (when (and delete-selection-mode (region-active-p))
    (setq deactivate-mark t))
  (abort-minibuffers))

(defun conf--minibuffer-candidate ()
  (if (and (bound-and-true-p vertico--input) (minibufferp))
      (cons (vertico--metadata-get 'category) (vertico--candidate))
    nil))

(defun conf--minibuffer-selected-directory-maybe ()
  (let ((minibuffer-candidate (conf--minibuffer-candidate)))
    (when minibuffer-candidate
      (pcase (car minibuffer-candidate)
        ('project-file (projectile-project-root
                        (expand-file-name (cdr minibuffer-candidate))))
        ('file (let ((path (expand-file-name (cdr minibuffer-candidate))))
                 (if (not (file-directory-p path))
                        (file-name-directory path)
                      path)))))))

(defun conf--exit-minibuffer-and-execute (func)
  (run-with-timer 0 nil func)
  (abort-recursive-edit))

(defun conf--select-directory-and-ripgrep ()
  (interactive)
  (let ((path (or (conf--minibuffer-selected-directory-maybe)
                  (read-directory-name "Select directory: "))))
    (conf--exit-minibuffer-and-execute (lambda () (consult-ripgrep path)))))

(defun conf--magit-in-selected-directory ()
  (interactive)
  (if-let* ((selected-path (conf--minibuffer-selected-directory-maybe)))
      (conf--exit-minibuffer-and-execute
       (lambda () (let ((default-directory selected-path))
                    (magit))))
    (conf--exit-minibuffer-and-execute (lambda () (magit)))))

(defun conf--vterm-in-selected-directory ()
  (interactive)
  (if-let* ((selected-path (conf--minibuffer-selected-directory-maybe)))
      (conf--exit-minibuffer-and-execute
       (lambda () (let ((default-directory selected-path))
                    (conf--vterm-toggle-insert-cd))))
    (conf--exit-minibuffer-and-execute (lambda () (conf--vterm-toggle)))))

(defun conf--debug-category()
  (interactive)
  (message (format "Current category: %s" (vertico--metadata-get 'category))))

(use-package consult
  :bind
  (
   ("M-f"         . conf--consult-line)
   ("C-x b"       . consult-buffer)
   ("M-R"         . conf--select-directory-and-ripgrep)
   ("M-X"         . execute-extended-command)
   ("M-o"         . find-file)
   ("M-O"         . consult-buffer)
   :map prog-mode-map
   ;; ("M-."         . conf--consult-ripgrep)
   :map minibuffer-local-map
   ([M-backspace] . delete-until-slash-maybe)
   ("C-g" . conf--minibuffer-keyboard-quit)
   ("M-g" . conf--minibuffer-keyboard-quit)
   ("C-x g" . conf--magit-in-selected-directory)
   ("M-e" . conf--vterm-in-selected-directory)
   )
  :custom
  (consult-async-input-debounce 0.1)
  (consult-line-start-from-top 't)
  (consult-narrow-key "<")
  :config
  (custom-set-faces
    '(consult-line-number-wrapped ((t (:inherit consult-line-number-prefix)))))
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref))

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-file-register
 consult--source-recent-file consult--source-project-recent-file
 :preview-key '(:debounce 0.01 any)) ;; Option 1: Delay preview


(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :bind
  (("M-/" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-help-key "?")
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(defvar consult--previous-point nil
    "Location of point before entering minibuffer.
Used to preselect nearest headings and imenu items.")

(defun consult--set-previous-point (&optional arg1 arg2)
  "Save location of point. Used before entering the minibuffer."
  (setq consult--previous-point (point)))

(advice-add #'consult-org-heading :before #'consult--set-previous-point)
(advice-add #'consult-outline :before #'consult--set-previous-point)
(advice-add #'consult-line :before #'consult--set-previous-point)


(advice-add #'vertico--update :around #'conf--consult-vertico-update-selection)
(advice-add #'vertico--recompute :after #'conf--vertico-set-update-selection)

(defun conf--vertico-set-update-selection(&rest _)
  (setq conf--vertico-update-selection t))


(defun conf--closest-integer-index-sorted (target int-list predicate)
  "Return the index of the closest integer to TARGET in a sorted INT-LIST using PREDICATE to extract integer values."
  (let ((left 0)
        (right (1- (length int-list))))
    ;; Binary search to narrow down the closest index
    (while (< left right)
      (let* ((mid (floor (+ left right) 2))
             (mid-value (funcall predicate (nth mid int-list))))
        (if (< target mid-value)
            (setq right mid)
          (setq left (1+ mid)))))
    ;; After binary search, `left` is close to the target; check the nearest index
    (let ((closest left)
          (closest-value (funcall predicate (nth left int-list))))
      (when (and (> left 0)
                 (< (abs (- target (funcall predicate (nth (1- left) int-list))))
                    (abs (- target closest-value))))
        (setq closest (1- left)))
      closest)))


(defvar conf--minibuffer-command-stack nil
  "Stack of commands that opened each minibuffer session, tracked by depth.")

(defun conf--push-minibuffer-command ()
  "Push the command that opens the minibuffer onto `conf--minibuffer-command-stack`."
  (push this-command conf--minibuffer-command-stack))

(defun conf--pop-minibuffer-command ()
  "Pop the last command from `conf--minibuffer-command-stack` upon minibuffer exit."
  (when conf--minibuffer-command-stack
    (pop conf--minibuffer-command-stack)))

;; Add hooks to track entering and exiting minibuffers
(add-hook 'minibuffer-setup-hook #'conf--push-minibuffer-command)
(add-hook 'minibuffer-exit-hook #'conf--pop-minibuffer-command)

(require 'embark-consult)
(defun conf--consult-vertico-update-selection (orig-fun &rest args)
  "Pick the nearest candidate rather than the first after updating candidates."
  (setq conf--vertico-update-selection nil)
  (let ((result (apply orig-fun args))
        (conf--current-minibuffer-command (car conf--minibuffer-command-stack)))

    ;; (message "conf--vertico-update-selection: %s" conf--vertico-update-selection)
    (when (and conf--vertico-update-selection
               consult--previous-point vertico--candidates
               (memq conf--current-minibuffer-command
                     '(consult-org-heading consult-outline consult-line conf--consult-line)))
      (setq vertico--index
            (max 0 ; if none above, choose the first below
                 (or (conf--closest-integer-index-sorted
                      consult--previous-point
                      vertico--candidates
                      (lambda (cand)
                        (cl-case conf--current-minibuffer-command
                          (consult-outline
                           (car (consult--get-location cand)))
                          (consult-org-heading
                           (get-text-property 0 'consult--candidate cand))
                          (consult-line
                           (car (consult--get-location cand)))
                          (conf--consult-line
                           (car (consult--get-location cand)))
                          (t (error "Missing conf--consult-vertico-update-selection config")))))
                     (length vertico--candidates)))))

    result))

(defun conf--consult-line (&optional initial start)
  "Call `consult-line` with candidates filtered by the symbol at point by default."
  (interactive)
  (run-with-timer 0 nil (lambda () (interactive)
                          (when (minibufferp)
                            (goto-char (minibuffer-prompt-end))
                            (push-mark (point-max) nil t))))
  (consult-line (thing-at-point 'symbol)))

(require 'vertico-multiform)
(vertico-multiform-mode +1)
(define-key vertico-multiform-map (kbd "M-R") nil)
(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

;; (defun +vertico-highlight-unsaved-buffer (buffer-name)
;;   (let ((buffer (get-buffer buffer-name)))
;;     (if (and buffer
;;                (buffer-live-p buffer)
;;                (buffer-modified-p buffer))
;;         (propertize buffer-name 'face 'font-lock-constant-face)
;;       buffer-name)))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
    file))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
      (propertize cmd 'face 'font-lock-constant-face)
      cmd)))

(add-to-list 'vertico-multiform-categories
             '(file
               ;; (vertico-sort-function . sort-directories-first)
               (+vertico-transform-functions . +vertico-highlight-directory)))

(add-to-list 'vertico-multiform-commands
             '(execute-extended-command
               ;; reverse
               (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

;; (add-to-list 'vertico-multiform-categories
;;              '(multi-category
;;                ;; (vertico-sort-function . sort-directories-first)
;;                (+vertico-transform-functions . +vertico-highlight-unsaved-buffer)))


(defun conf--embark-consult-export-xref (items)
  "Create a grep-like buffer listing ITEMS from xref."
  (embark-consult--export-grep
   :header "Exported xref results:\n\n"
   :lines items
   :insert
   (lambda (items)
     (let ((count 0))
       (dolist (item items)
         (let* ((xref (get-text-property 0 'consult-xref item))
                (loc (xref-item-location xref))
                (file (or (xref-file-location-file loc) ""))
                (line (xref-location-line loc))
                (summary (xref-item-summary xref)))
           (insert (format "%s:%d:%s\n" file line summary))
           (cl-incf count)))
       count))
   :footer #'ignore))

;; export grep result with consult-line
(setf (alist-get 'consult-location embark-exporters-alist)
      #'embark-consult-export-location-grep)
(setf (alist-get 'consult-xref embark-exporters-alist)
      #'conf--embark-consult-export-xref)

(define-key embark-file-map "g" #'magit)
(add-to-list 'embark-pre-action-hooks '(magit embark--universal-argument))
(add-to-list 'embark-around-action-hooks '(magit embark--cd))

(define-key embark-file-map "M-e" #'conf--vterm-toggle)
(add-to-list 'embark-pre-action-hooks '(conf--vterm-toggle embark--universal-argument))
(add-to-list 'embark-around-action-hooks '(conf--vterm-toggle embark--cd))

(defun rename-file-and-buffer (old-name new-name &optional ok-if-already-exists)
  "Rename OLD-NAME to NEW-NAME, updating associated buffer if it exists."
  (interactive
   (let* ((current-file (and (buffer-file-name) (file-name-nondirectory (buffer-file-name))))
          (old (read-file-name (if current-file
                                   (format "Rename file ('%s' by default): "
                                           (file-name-nondirectory (buffer-file-name)))
                                 "Rename file: ")
                               nil (buffer-file-name) t))
          (new (read-file-name (format "Rename '%s' to file: " old) (file-name-directory old))))
     (list old new current-prefix-arg)))

  (when (not (file-directory-p (file-name-directory new-name)))
    (if (y-or-n-p (format "Create directory '%s'? "
                          (file-name-directory new-name)))
        (make-directory (file-name-directory new-name))
      (error "Cancelled")))

  (rename-file old-name new-name ok-if-already-exists)

  (let ((buf (find-buffer-visiting old-name)))
    (when buf
      (with-current-buffer buf
        (set-visited-file-name new-name nil t)
        (rename-buffer new-name)
        (set-buffer-modified-p nil)
        (message "Renamed buffer associated with '%s' to '%s'" old-name new-name)))))

(define-key embark-file-map "r" #'rename-file-and-buffer)
(add-to-list 'embark-post-action-hooks '(rename-file-and-buffer embark--restart))

(defun delete-file-and-buffer (filename delete-buffer)
  "Delete the file FILENAME and its associated buffer, if any."
  (interactive
   (let* ((current-file (and (buffer-file-name) (file-name-nondirectory (buffer-file-name))))
          (filename (read-file-name (if current-file
                                        (format "Delete file ('%s' by default): " current-file)
                                      "Delete file: ")
                                    nil (buffer-file-name) t))
          (buffer (find-buffer-visiting filename))
          (delete-buffer (and buffer
                              (y-or-n-p (format "Delete buffer '%s' too? "
                                                (buffer-name buffer))))))
     (list filename delete-buffer)))
  (when filename
    (delete-file filename)
    (message "Deleted file %s" filename)
    (when delete-buffer
      (kill-buffer (find-buffer-visiting filename))
      (message "Deleted buffer associated with %s" filename))))

(define-key embark-file-map "d" #'delete-file-and-buffer)
(add-to-list 'embark-post-action-hooks '(delete-file-and-buffer embark--restart))
(add-to-list 'embark-pre-action-hooks '(delete-file-and-buffer embark--confirm))

(defun copy-file-in-directory (old-name new-name &optional ok-if-already-exists)
  "Rename OLD-NAME to NEW-NAME, updating associated buffer if it exists."
  (interactive
   (let* ((old (read-file-name (format "Copy file ('%s' by default): "
                                       (file-name-nondirectory (buffer-file-name)))
                               nil (buffer-file-name) t))
          (new (read-file-name (format "Copy '%s' to file: " old) (file-name-directory old))))
     (list old new current-prefix-arg)))

  (when (not (file-directory-p (file-name-directory new-name)))
    (if (y-or-n-p (format "Create directory '%s'? "
                          (file-name-directory new-name)))
        (make-directory (file-name-directory new-name))
      (error "Cancelled")))

  (copy-file old-name new-name ok-if-already-exists))

(define-key embark-file-map "c" #'copy-file-in-directory)
(add-to-list 'embark-post-action-hooks '(copy-file-in-directory embark--restart))

(use-package meow
  :config
  (setq meow--kbd-forward-char "C-%")
  (global-set-key (kbd meow--kbd-forward-char) 'forward-char)

  (setq meow--kbd-delete-char "C-$")
  (global-set-key (kbd meow--kbd-delete-char) 'delete-char)

  (setq meow--kbd-kill-line "C-=")
  (global-set-key (kbd meow--kbd-kill-line) 'kill-line)

  (global-set-key (kbd "C-x h") 'conf--prev-buffer)
  (global-set-key (kbd "C-x l") 'conf--next-buffer)
  (global-set-key (kbd "C-x C-h") 'conf--prev-buffer)
  (global-set-key (kbd "C-x C-l") 'conf--next-buffer)

  (global-set-key (kbd "C-x j") nil)
  (global-set-key (kbd "C-x k") nil)
  (global-set-key (kbd "C-x C-j") nil)
  (global-set-key (kbd "C-x C-k") nil)

  (setq meow-use-clipboard t)

  (setq meow-expand-hint-counts
        '((word . 0)
          (line . 0)
          (block . 0)
          (find . 0)
          (till . 0)))

  (defun conf--meow-insert-wrap ()
    (interactive)
    (when (meow--allow-modify-p)
      (setq this-command #'meow-insert)
      (meow--switch-state 'insert)))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore)
     '("C-SPC" . (lambda () (interactive) (meow-left-expand) (meow-right-expand))))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("&" . meow-digit-argument)
     '("é" . meow-digit-argument)
     '("\"" . meow-digit-argument)
     '("'" . meow-digit-argument)
     '("(" . meow-digit-argument)
     '("-" . meow-digit-argument)
     '("è" . meow-digit-argument)
     '("_" . meow-digit-argument)
     '("ç" . meow-digit-argument)
     '("à" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     '("o" . other-window)
     ;; '("h" . conf--prev-buffer)
     ;; '("l" . conf--next-buffer)
     )
    (meow-normal-define-key
     '("à" . meow-expand-0)
     '("&" . meow-expand-1)
     '("é" . meow-expand-2)
     '("\"" . meow-expand-3)
     '("'" . meow-expand-4)
     '("(" . meow-expand-5)
     '("-" . meow-expand-6)
     '("è" . meow-expand-7)
     '("_" . meow-expand-8)
     '("ç" . meow-expand-9)

     '(":" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . conf--meow-insert-wrap)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("C-h" . left-word)
     '("C-j" . forward-paragraph)
     '("C-k" . backward-paragraph)
     '("C-l" . right-word)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("M-h" . meow-left)
     '("M-k" . (lambda () (interactive) (move-up 4)))
     '("M-j" . (lambda () (interactive) (move-down 4)))
     '("M-l" . meow-right)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("M" . (lambda () (interactive) (meow-join -1)))
     '("n" . meow-search)
     '("N" . meow-reverse)
     '("o" . ignore)
     '("O" . meow-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . ignore)                 ; meow-undo
     '("U" . meow-undo)              ; undo-fu-only-redo
     '("M-U" . undo-fu-only-redo)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)
     '("C-SPC" . (lambda () (interactive) (meow-left-expand) (meow-right-expand))))
    (meow-define-keys
        'insert
      '("C-h" . meow-left)
      '("C-j" . meow-next)
      '("C-k" . meow-prev)
      '("C-l" . meow-right)
      '("M-h" . meow-left)
      '("M-k" . (lambda () (interactive) (move-up 4)))
      '("M-j" . (lambda () (interactive) (move-down 4)))
      '("M-l" . meow-right)))
  (meow-setup)
  (add-hook 'git-commit-setup-hook 'meow-insert-mode)
  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (add-hook 'vterm-mode-hook 'meow-insert-mode)
  (meow-global-mode))

(use-package meow-vterm
  :straight (meow-vterm :type git :host github :repo "accelbread/meow-vterm")
  :init
  (meow-vterm-enable))

;; ;; TODO: try https://github.com/jdtsmith/indent-bars
;; TODO: disable eglot when viewing magit commit
;; TODO: test direnv
;; TODO (longshot): try meowing https://github.com/meow-edit/meow

;; load graphic settings
(require 'graphics)
(require 'custom-python-highlighting)
