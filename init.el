;;; emacs-conf --- Summary
;;; Commentary:
;;
;; TODO:
;; - try tree siter: https://github.com/ubolonton/emacs-tree-sitter
;;   faster & better syntax highlighting
;;
;;; Code:

;; ------------------
;; bootstrap straight
;; ------------------
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
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

;; set load path
(setq conf--base-dir (file-name-directory (or load-file-name default-directory)))
(add-to-list 'custom-theme-load-path conf--base-dir)
(add-to-list 'load-path conf--base-dir)

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

(global-auto-revert-mode)
(setq inhibit-startup-message t)
(setq c-toggle-auto-newline t)
(setq make-backup-files nil)
(delete-selection-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;; speedup long lines
(setq bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(setq auto-window-vscroll nil)
(global-so-long-mode 1)

;; TODO: use bind-key: https://melpa.org/#/bind-key

;; basic keybindings
(global-set-key (kbd "C-f") "\C-a\C-@\C-e")
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [C-return] 'newline-and-indent)

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
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-e") 'recenter)
(global-set-key (kbd "M-N") 'goto-line)

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
                                   (innamespace 0)))))

(defun --cc-style-setup()
  (c-set-style "better-cc-style"))

(add-hook 'c-mode-hook '--cc-style-setup)
(add-hook 'c++-mode-hook '--cc-style-setup)

(defun --set-tab-with()
  (setq tab-width 4)
  (setq c-basic-offset 4))

(add-hook 'cmake-mode-hook '--set-tab-with)
(add-hook 'objc-mode-hook '--set-tab-with)

(setq c-default-style "linux")
(setq-default indent-tabs-mode nil)

(add-hook 'c++-mode-hook
      '(lambda()
        (font-lock-add-keywords
         nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; add the new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT
           ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
	   ("\\<[-+]?[0-9]*\\.?[0-9]*\\([uUlL]+\\|[eE][-+]?[0-9]+\\)?[fFlL]?\\>" . font-lock-constant-face)
           ;; user-types (customize!)
           ;; ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
           ;; ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
        ) t)

;; avoid boring buffers
(defvar boring-buffers
  '("\\*.*\\*"
    "COMMIT_EDITMSG")
  "List of boring buffers regexp")

(defun is-buffer-valid (buffer-name)
  (let ((valid-buffer t))
    (loop for boring-buffer in boring-buffers do
          (when (string-match boring-buffer buffer-name)
            (setq valid-buffer nil)))
    valid-buffer))

(defun --contains-valid-buffer (buffer-list)
  (let ((valid-buffer nil))
    (loop for buffer in buffer-list do
          (when (is-buffer-valid (buffer-name buffer))
            (setq valid-buffer t)))
    valid-buffer))

(defun skip-temp-buffers (func)
  (if (--contains-valid-buffer (buffer-list))
      (while (not (is-buffer-valid (buffer-name)))
        (funcall func))))

(defun my-next-buffer ()
  (interactive)
  (next-buffer)
  (skip-temp-buffers 'next-buffer))

(defun my-prev-buffer ()
  (interactive)
  (previous-buffer)
  (skip-temp-buffers 'previous-buffer))

(defun kill-this-buffer-avoid-boring ()
  (interactive)
  (kill-this-buffer)
  (when (not (is-buffer-valid (buffer-name)))
    (skip-temp-buffers 'previous-buffer)))

(global-set-key [remap next-buffer] 'my-next-buffer)
(global-set-key [remap previous-buffer] 'my-prev-buffer)
(global-set-key [remap kill-this-buffer] 'kill-this-buffer-avoid-boring)

;; better performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; --------------
;; setup packages
;; --------------

(use-package diminish)

(use-package autopair
  :diminish autopair-mode
  :config
  (autopair-global-mode t)
  (setq autopair-autowrap t)
  (setq autopair-blink nil))

(use-package company
  :bind
  (("M-RET" . company-complete))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(company-pseudo-tooltip-frontend
			    company-preview-if-just-one-frontend
			    company-echo-metadata-frontend))
  (defun --company-setup ()
    (setq company-backends (delete '(company-dabbrev-code company-gtags company-etags company-keywords) company-backends))
    (setq company-backends (delete 'company-dabbrev company-backends))
    (add-to-list 'company-backends '(company-dabbrev-code company-keywords) t))
  (add-hook 'company-mode-hook '--company-setup)
  ;; remove unwanted (and slow) backends
  (defun cc-company-setup ()
    (setq company-backends (delete 'company-semantic company-backends)))
  (add-hook 'c-mode-hook 'cc-company-setup)
  (add-hook 'c++-mode-hook 'cc-company-setup)
  (global-company-mode))

;; (use-package company-emoji
;;   :config
;;   ;; ...
;;   (add-to-list 'company-backends 'company-emoji))

;; (use-package irony
;;   :config
;;   (add-hook 'c-mode-hook (lambda ()
;; 			 (if (not (equal major-mode 'glsl-mode))
;; 			     (irony-mode))))
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package flycheck-irony
;;   :config
;;   (eval-after-load 'flycheck
;;     '(progn
;;        (require 'flycheck-irony)
;;        (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(use-package flycheck
  :bind
  (("C-c i f" . flycheck-mode)
   :map flycheck-mode-map
   ("C-c i l" . flycheck-list-errors))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  
  (flycheck-define-checker
      python-mypy ""
      :command ("dmypy-single-file" source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-flake8 'python-mypy t)
  
  :hook ((prog-mode) . #'flycheck-mode)
  )

;; (use-package company-irony
;;   :config
;;   (eval-after-load 'company
;;     '(progn
;;        (add-to-list 'company-backends 'company-irony)
;;        ;; company-clang is too slow
;;        (setq company-backends (delete 'company-clang company-backends)))))

;; (use-package company-c-headers
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-c-headers)))

;; (use-package company-irony-c-headers
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-irony-c-headers)))

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("M-Z" . redo))
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

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
   ([M-backspace] . backward-delete-word)
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
   helm-split-window-in-side-p t
   helm-echo-input-in-header-line t
   ;; Disable helm in minibuffer region completion (eval-expression for example)
   helm-mode-handle-completion-in-region nil
   helm-moccur-use-ioccur-style-keys nil
   helm-scroll-amount 6
   helm-moccur-show-buffer-fontification t
   helm-find-files-ignore-thing-at-point t)
  (helm-mode))

(use-package helm-ag
  :straight (:fork (:host github :repo "Azkae/emacs-helm-ag"))
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
   ("RET"         . helm-ag-mode-jump-other-window)))

(defun remove-helm-autopair ()
  (autopair-mode -1))

(add-hook 'helm-minibuffer-set-up-hook 'remove-helm-autopair)

(defun helm-config--helm-do-ag-on-file-maybe(basename)
  (interactive)
  (let* ((basename (expand-file-name basename))
         (basename (if (not (file-directory-p basename))
                       (file-name-directory basename)
                     basename)))
    (helm-do-ag basename)))

(defun open-vterm-action(basename)
  (interactive)
  (let* ((basename (expand-file-name basename))
         (basename (if (not (file-directory-p basename))
                       (file-name-directory basename)
                     basename))
         (default-directory basename))

    ;; use vterm toggle internal to save window configuration: when we do M-e (vterm-toggle) it will restore current window configuration
    (setq vterm-toggle--window-configration (current-window-configuration))
    (vterm)))

(defun helm-config--helm-do-ag-on-project-root(basename)
  (interactive)
  (helm-do-ag (projectile-project-root)))

(defun open-vterm-on-project-root-action(basename)
  (interactive)
  (let* ((default-directory (projectile-project-root)))
    ;; use vterm toggle internal to save window configuration: when we do M-e (vterm-toggle) it will restore current window configuration
    (setq vterm-toggle--window-configration (current-window-configuration))
    (vterm)))

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
   (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-find-files)))

(with-eval-after-load "helm-projectile"
  (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-file-maybe helm-source-projectile-projects)
  (helm-add-action-to-source "Find AG" 'helm-config--helm-do-ag-on-project-root helm-source-projectile-files-list)
  (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-projectile-projects)
  (helm-add-action-to-source "Open vterm on project root" 'open-vterm-on-project-root-action helm-source-projectile-files-list)
  (helm-add-action-to-source "Open vterm" 'open-vterm-action helm-source-projectile-files-list))

(defun helm-config--helm-do-ag-projectile-project-symbol ()
  (interactive)
    (helm-do-ag (projectile-project-root) nil (symbol-name (symbol-at-point))))

(define-key prog-mode-map (kbd "M-.") 'helm-config--helm-do-ag-projectile-project-symbol)

(setq projectile-keymap-prefix (kbd "M-p"))
(use-package projectile
  :diminish projectile-mode
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
   ([M-backspace] . backward-delete-word)
   ("M-e"         . helm-config--ff-open-vterm)
   ("M-E"         . helm-config--ff-open-vterm-root)
   ("M-R"         . helm-config--ff-run-helm-ag-root)
   :map helm-projectile-projects-map
   ("M-e"         . helm-config--ff-open-vterm)
   ("M-R"         . helm-config--ff-run-helm-ag))
  :config
  (setq projectile-completion-system 'helm))
(helm-projectile-on)
;; must be binded after (helm-projectile-on) because it remap projectile keybindings
(define-key projectile-mode-map [remap projectile-ag] 'helm-do-ag-project-root)

;; (use-package helm-gtags
;;   :diminish helm-gtags-mode
;;   :bind
;;   (:map helm-gtags-mode-map
;;    ("M-." . helm-gtags-dwim))
;;   :config
;;   (add-hook 'prog-mode-hook 'helm-gtags-mode)
;;   (add-hook 'dired-mode-hook 'helm-gtags-mode)
;;   (setq helm-gtags-direct-helm-completing t))

(use-package highlight-symbol
  :bind
  ([f7] . highlight-symbol-at-point))

(use-package cmake-mode
  :config
  (setq cmake-tab-width 4))

(use-package powerline
  :config
  (powerline-default-theme)
  (setq powerline-display-buffer-size nil
        powerline-display-mule-info   nil
        powerline-display-hud         nil))

(use-package zoom-frm
  :bind
  (("C-x C-+" . zoom-in)
   ("C-x C--" . zoom-out)))

(global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-in)
(global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-out)

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode))

;; (use-package yascroll
;;   :config
;;   (global-yascroll-bar-mode 1)
;;   (setq yascroll:delay-to-hide 0.3))

(use-package phi-search)

(use-package multiple-cursors
  :bind
  (("C-j"      . mc/mark-next-like-this)
   ("M-j"      . mc/mark-next-symbol-like-this)
   ("C-n"      . mc/skip-to-next-like-this)
   ("C-S-n"    . mc/unmark-previous-like-this)
   :map mc/keymap
   ("<return>" . nil)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (format "%s/snippets" conf--base-dir)))
  (yas-global-mode 1))

(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package git-timemachine)

;; ;; dirlocals:
;; ((nil
;;   (python-shell-interpreter . "python_env")
;;   (flycheck-pycheckers-venv-root . "/path/to/venv/bin")))

;; python_env:
;; source ~/signals/venv/bin/activate && python $*


(use-package quickrun
  :bind
  (("M-q r" . quickrun))
  :config
  (setq quickrun-timeout-seconds 99999999)
  )

(use-package yaml-mode)

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

;; (use-package lsp-mode
;;   :config
;;   (add-hook 'prog-mode-hook #'lsp)
;;   (setq lsp-message-project-root-warning t)

;;   (use-package company-lsp
;;     :config
;;     (push 'company-lsp company-backends))

;;   (use-package lsp-ui
;;     :config
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;     (setq lsp-ui-sideline-show-hover nil)
;;     (setq lsp-ui-sideline-show-code-actions nil)
;;     (setq lsp-ui-doc-enable nil)
;;     (setq lsp-ui-flycheck-live-reporting t))

;;   ;; (use-package lsp-clangd
;;   ;;   :init
;;   ;;   (when (equal system-type 'darwin)
;;   ;;     (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
;;   ;;   (add-hook 'c-mode-hook #'lsp-clangd-c-enable)
;;   ;;   (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
;;   ;;   (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable))

;;   (use-package ccls
;;     :hook ((c-mode c++-mode objc-mode) .
;;            (lambda () (require 'ccls) (lsp))))


;;   ;; (use-package ccls
;;   ;;   :config
;;   ;;   (setq ccls-executable "/usr/local/bin/ccls")
;;   ;;   (add-hook 'c-mode-hook #'lsp-ccls-enable)
;;   ;;   (add-hook 'c++-mode-hook #'lsp-ccls-enable)
;;   ;;   (add-hook 'objc-mode-hook #'lsp-ccls-enable)
;;   ;;   (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))
;;   ;; )
;;   )

(use-package lsp-mode :commands lsp
  :diminish lsp-mode
  :bind
  (:map lsp-mode-map
        ("M-." . lsp-find-definition)
        ("M-?" . lsp-find-references)
        )
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-semantic-highlighting nil)
  ;; (when (string-equal system-type "darwin")
  ;;   (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  )

(use-package lsp-ui ;; :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook 'flymake-mode)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-live-reporting nil))

(use-package company-lsp :commands company-lsp
  :config
  (push 'company-lsp company-backends)
)

(use-package ccls
  :config
  (setq ccls-sem-highlight-method nil)
  ;; (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

(use-package clang-format)

(use-package json-mode)

;; (use-package elixir-mode
;;   :config
;;   (lsp-define-stdio-client lsp-elixir "elixir"
;;                            #'projectile-project-root
;;                            '("/Users/ouabde_r/signals/elixir-ls/bin/language_server.sh")))

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
  :map vterm-copy-mode-map
  ("M-z" . 'vterm-copy-mode))
  :config
  ;; (setq vterm-timer-delay 0.01)
  (setq vterm-timer-delay 0.05)
  ;; (setq vterm-timer-delay 0.1)
  )

(defface conf--vterm-face
  '((t :family "Menlo" :height 125))
  "The basic fixed-pitch face."
  :group 'basic-faces)

(add-hook 'vterm-mode-hook
          (lambda ()
            (set
             (make-local-variable 'buffer-face-mode-face) 'conf--vterm-face)
            (buffer-face-mode t)))

(use-package vterm-toggle
  :bind
  (("M-e" . vterm-toggle)
   ("M-E" . vterm-toggle-cd)
   :map vterm-mode-map
   ("M-E" . vterm-toggle-insert-cd))
  :config
  (setq vterm-toggle-hide-method 'reset-window-configration)
  (setq vterm-toggle-reset-window-configration-after-exit nil)

  ;;; TODO: try scope per project instead of a global shell
  ;; (setq vterm-toggle-scope 'project)
  ;; (setq vterm-toggle-project-root nil)
  )

(use-package helm-xref)

(use-package pyvenv)

(use-package kotlin-mode)

(use-package groovy-mode)

(use-package string-inflection
  :bind
  ("C-c f" . string-inflection-toggle))

(defun conf--org-open-link-maybe()
  (interactive)
  (if (eq (car (org-element-context)) 'link)
      (call-interactively 'org-open-at-point)
    (call-interactively 'org-meta-return)))

;; Fix windmove in org-mode
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map [M-left] 'windmove-left)
            (define-key org-mode-map [M-right] 'windmove-right)
            (define-key org-mode-map [M-up] 'windmove-up)
            (define-key org-mode-map [M-down] 'windmove-down)
            (define-key org-mode-map [M-return] 'conf--org-open-link-maybe)
            (toggle-truncate-lines)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (expand-file-name "~/Dropbox/org-roam"))
      (org-roam-verbose nil)
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n t" . org-roam-dailies-today)
               ("C-c n y" . org-roam-dailies-yesterday)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; load graphic settings
(require 'graphics)
