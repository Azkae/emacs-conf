;; bootstrap straight
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

;; -------------------
;; base emacs settings
;; -------------------

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier nil))

(setq ring-bell-function 'ignore)

(require 'cl-lib)

(require 'paren)
(show-paren-mode)

(global-auto-revert-mode)
(setq inhibit-startup-message t)
(setq c-toggle-auto-newline t)
(setq make-backup-files nil)
(delete-selection-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

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
(global-set-key (kbd "C-x DEL") 'ignore)
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "M-t"))

(global-set-key (kbd "M-g") "\C-g")

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
		 ))
    (add-hook hook 'company-mode))
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
   ;; ("M-<down>"    . helm-scroll-other-window)
   ;; ("M-<up>"      . helm-scroll-other-window-down)
   :map helm-moccur-map
   ("<right>"     . nil)
   ("<left>"      . nil)
   ;; ("M-<down>"    . helm-scroll-other-window)
   ;; ("M-<up>"      . helm-scroll-other-window-down)
   :helm-find-files-map
   ;; ("M-<down>"    . helm-scroll-other-window)
   ;; ("M-<up>"      . helm-scroll-other-window-down)
   ([M-backspace] . delete-until-slash)
   :helm-read-file-map
   ([M-backspace] . delete-until-slash))
  :config
  (setq
   helm-quick-update t
   helm-buffers-fuzzy-matching t
   helm-ff-newfile-prompt-p nil ; do not prompt to create new file
   helm-split-window-in-side-p t
   helm-echo-input-in-header-line t
   helm-mode-handle-completion-in-region nil ; Disable helm in minibuffer region completion (eval-expression for example)
   helm-moccur-use-ioccur-style-keys nil
   helm-scroll-amount 6)
  (helm-mode)
  )

(defun helm-config--ff-run-helm-ag()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-do-ag)))

(use-package helm-ag
  :bind
  (("M-R" . helm-do-ag)
   ("M-F" . helm-do-ag-buffers)
   :map
   ("M-R" . helm-config--ff-run-helm-ag)))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("M-Z" . redo))
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo))
