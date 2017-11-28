;; ------------------
;; bootstrap straight
;; ------------------
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-ensure t)

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

(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-progressive-speed nil)

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

;; Fix windmove in org-mode
(add-hook 'org-mode-hook
          (lambda()
            (define-key org-mode-map [M-left] 'windmove-left)
            (define-key org-mode-map [M-right] 'windmove-right)
            (define-key org-mode-map [M-up] 'windmove-up)
            (define-key org-mode-map [M-down] 'windmove-down)))

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

(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior


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
  (dolist (hook (list
		 'emacs-lisp-mode-hook
		 'lisp-mode-hook
		 'lisp-interaction-mode-hook
		 'scheme-mode-hook
		 'java-mode-hook
		 'c-mode-hook
		 'c++-mode-hook
		 'haskell-mode-hook
		 'asm-mode-hook
		 'emms-tag-editor-mode-hook
		 'sh-mode-hook
		 'python-mode-hook
		 ))
    (add-hook hook 'company-mode))
  (defun --company-setup ()
    (setq company-backends (delete '(company-dabbrev-code company-gtags company-etags company-keywords) company-backends))
    (add-to-list 'company-backends '(company-dabbrev-code company-keywords)))
  (add-hook 'company-mode-hook '--company-setup)
  ;; remove unwanted (and slow) backends
  (defun cc-company-setup ()
    (setq company-backends (delete 'company-semantic company-backends)))
  (add-hook 'c-mode-hook 'cc-company-setup)
  (add-hook 'c++-mode-hook 'cc-company-setup))

(use-package irony
  :config
  (add-hook 'c-mode-hook (lambda ()
			 (if (not (equal major-mode 'glsl-mode))
			     (irony-mode))))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package flycheck-irony
  :config
  (eval-after-load 'flycheck
    '(progn
       (require 'flycheck-irony)
       (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(use-package flycheck
  :bind
  (("C-c i f" . flycheck-mode))
  :config
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company-irony
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (defun setup-irony-company ()
    (company-irony-setup-begin-commands)
    (add-to-list 'company-backends 'company-irony)
    (setq company-backends (delete 'company-clang company-backends)))
  (add-hook 'irony-mode-hook 'setup-irony-company))

(use-package company-c-headers
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (defun setup-irony-company ()
    (company-irony-setup-begin-commands)
    (setq company-backends (delete 'company-clang company-backends)))
  (add-hook 'irony-mode-hook 'setup-irony-company))

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

(use-package helm
  :diminish helm-mode
  :bind
  (("M-f"         . helm-occur)
   ("C-x C-f"     . helm-find-files)
   ("C-x b"       . helm-mini)
   ("C-p"         . helm-buffers-list)
   ("M-X"         . helm-M-x)
   ("M-o"         . helm-find-files)
   ("M-O"         . helm-buffers-list)
   :map helm-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   ("M-v"         . yank)
   ("<tab>"       . helm-execute-persistent-action)
   ([M-backspace] . backward-delete-word)
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   :map helm-moccur-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   :map helm-find-files-map
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   ([M-backspace] . delete-until-slash)
   :map helm-read-file-map
   ([M-backspace] . delete-until-slash)
   :map helm-grep-map
   ("<M-down>"    . helm-scroll-other-window)
   ("<M-up>"      . helm-scroll-other-window-down)
   ("<right>"     . nil)
   ("<left>"      . nil))
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

(require 'ya-helm-ag)

(defun ya-helm-do-ag-on-file-maybe(basename)
  (interactive)
  (let* ((basename (expand-file-name basename))
         (basename (if (not (file-directory-p basename))
                       (file-name-directory basename)
                     basename)))
    (ya-helm-ag (list basename))))

(defun helm-config--ff-run-helm-ag()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'ya-helm-do-ag-on-file-maybe)))

(global-set-key (kbd "M-R") 'ya-helm-do-ag)
(global-set-key (kbd "M-F") 'ya-helm-do-ag-buffers)
(define-key helm-find-files-map (kbd "M-R") 'helm-config--ff-run-helm-ag)

(setq projectile-keymap-prefix (kbd "M-p"))
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil
        projectile-svn-command projectile-generic-command)
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
   :map helm-projectile-projects-map
   ("M-R"         . helm-config--ff-run-helm-ag))
  :config
  (setq projectile-completion-system 'helm))
(helm-projectile-on)
;; must be binded after (helm-projectile-on) because it remap projectile keybindings
(define-key projectile-mode-map [remap projectile-ag] 'ya-helm-do-ag-projectile-project)

(use-package helm-gtags
  :diminish helm-gtags-mode
  :bind
  (:map helm-gtags-mode-map
   ("M-." . helm-gtags-dwim))
  :config
  (add-hook 'prog-mode-hook 'helm-gtags-mode)
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (setq helm-gtags-direct-helm-completing t))

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

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode))

(use-package yascroll
  :config
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide 0.3))

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

(use-package yaml-mode)

;; -----------
;; theme setup
;; -----------

(load-theme 'monokai t)

(set-default-font "Fira Mono-10")
(add-to-list 'default-frame-alist '(font . "Fira Mono-10"))
(add-to-list 'default-frame-alist '(cursor-color . "white"))

(menu-bar-mode -1)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-unset-key (kbd "C-z"))
(normal-erase-is-backspace-mode 1)

(defun git-gutter-fringe--set-faces (frame)
  (with-selected-frame frame
    (git-gutter-fr+-minimal)
    (set-face-attribute 'git-gutter-fr+-added    nil :foreground "gray25")
    (set-face-attribute 'git-gutter-fr+-deleted  nil :foreground "gray25")
    (set-face-attribute 'git-gutter-fr+-modified nil :foreground "gray25")))

(eval-after-load 'git-gutter-fringe+
  '(progn
     (git-gutter-fringe--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'git-gutter-fringe--set-faces)))

(defun helm--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'helm-ff-directory nil :foreground "Cyan" :weight 'bold
			:background nil)
    (set-face-attribute 'helm-ff-dotted-directory nil :foreground "White"
			:background nil)
    (set-face-attribute 'helm-ff-dotted-symlink-directory nil :foreground "Magenta"
			:background nil)
    (set-face-attribute 'helm-ff-executable nil :foreground "Orange"
			:background nil)
    (set-face-attribute 'helm-ff-file nil :foreground "White"
			:background nil)
    (set-face-attribute 'helm-ff-symlink nil :foreground "Magenta"
			:background nil)
    (set-face-attribute 'helm-locate-finish nil :foreground "Black" :weight 'bold
			:background "Yellow")))

(eval-after-load 'helm
  '(progn
     (helm--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'helm--set-faces)))


(defun company--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'company-echo-common nil :underline t :foreground nil)
    (set-face-attribute 'company-preview nil :inherit 'shadow :foreground nil :background nil)
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :underline t :background "LightSteelBlue3" :foreground "dark slate gray")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "SteelBlue3")
    (set-face-attribute 'company-scrollbar-fg nil :background "DeepSkyBlue4")
    (set-face-attribute 'company-template-field nil :background "#49483E" :foreground nil)
    (set-face-attribute 'company-tooltip nil :background "LightSteelBlue1" :foreground "dark slate gray")
    (set-face-attribute 'company-tooltip-annotation nil :inherit 'company-tooltip :foreground "slate gray")
    (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :underline t :foreground nil)
    (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :underline t :foreground nil)
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "LightSteelBlue3")))

(eval-after-load 'company
  '(progn
     (company--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'company--set-faces)))

(defun yascroll--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'yascroll:thumb-fringe    nil :foreground "gray25" :background "gray25")
    (set-face-attribute 'yascroll:thumb-text-area nil :background "gray25")))

(eval-after-load 'yascroll
  '(progn
     (yascroll--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'yascroll--set-faces)))

(defun mode-line--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'mode-line    nil :background "OliveDrab3" :foreground "black")))

(mode-line--set-faces (selected-frame))
(add-hook 'after-make-frame-functions 'mode-line--set-faces)

(defun basic--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'show-paren-match    nil :background "steelblue3")))

(basic--set-faces (selected-frame))
(add-hook 'after-make-frame-functions 'basic--set-faces)
