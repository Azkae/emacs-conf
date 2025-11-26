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

(setq custom-file (concat conf--base-dir "custom.el"))
(load custom-file)

(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

;; -------------------
;; base emacs settings
;; -------------------

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier nil))

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin/"))

(setq ring-bell-function 'ignore)

(require 'cl-lib)
(require 'cl)

(require 'paren)
(show-paren-mode)

;; save minibuffer history
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'compile-command)

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
(setq auto-window-vscroll nil)
(setq redisplay-skip-fontification-on-input t)
(global-so-long-mode 1)

;; treat camelCase as multiple words for cursor movement
(global-subword-mode)

;; prefer vertical splits
(setq split-width-threshold 140)

;; Track-pad horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Better comint settings
(use-package comint
  :straight (:type built-in)
  :config
  (setq comint-output-filter-functions (remove 'comint-postoutput-scroll-to-bottom comint-output-filter-functions))
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-show-maximum-output nil)
  (define-key comint-mode-map (kbd "M-p") #'comint-previous-input)
  (define-key comint-mode-map (kbd "M-b") #'comint-previous-input)
  (define-key comint-mode-map (kbd "M-r") nil)
  (define-key comint-mode-map (kbd "C-M-l") nil))

;; TODO: use bind-key: https://melpa.org/#/bind-key

;; basic keybindings
;; (global-set-key (kbd "C-f") "\C-a\C-a\C-@\C-e")
(global-set-key [C-return] 'newline)
(global-set-key (kbd "C-h") nil)

(global-set-key (kbd "M-à") 'shrink-window-horizontally)
(global-set-key (kbd "M-)") 'enlarge-window-horizontally)

(global-set-key (kbd "C-c b") 'pop-tag-mark)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key [C-backspace] 'delete-backward-char)

;; remove annoying keybindings
(global-set-key (kbd "C-x DEL") 'ignore)
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-o"))

(define-key key-translation-map (kbd "M-g") (kbd "C-g"))
; (define-key crm-local-completion-map (kbd "M-v") nil)

;; (global-set-key (kbd "C-c p b") 'profiler-start)
;; (global-set-key (kbd "C-c p r") 'profiler-report)
;; (global-set-key (kbd "C-c p e") 'profiler-stop)

(global-set-key (kbd "C-x C-o") 'other-window)

;; don't ask confirmation for kill-buffer with process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defun kill-region-maybe()
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)))

(defun conf--scroll-left ()
  (interactive)
  (let ((window (selected-window)))
    (when (and window)
      (set-window-hscroll window (max 0 (- (window-hscroll window) 10))))))

(defun conf--scroll-right ()
  (interactive)
  (let ((window (selected-window)))
    (when window
      (set-window-hscroll window (+ (window-hscroll window) 10)))))

(global-set-key (kbd "M-x") 'kill-region-maybe)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-d") "\C-a\C-@\C-e")
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-N") 'goto-line)
(global-set-key (kbd "M-a") 'recenter)
;; (global-set-key (kbd "M-k") 'compile)
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-M-l") nil)
(global-set-key (kbd "M-l") nil)
(global-set-key (kbd "M-h") nil)
(global-set-key (kbd "M-H") 'conf--scroll-left)
(global-set-key (kbd "M-L") 'conf--scroll-right)
(global-set-key (kbd "M-u") nil)
(global-set-key (kbd "C-S-x C-S-c") 'save-buffers-kill-terminal)

(global-set-key (kbd "C-c e") 'kmacro-end-and-call-macro)

;; remap registers
(define-key global-map (kbd "C-c r") ctl-x-r-map)

;; (global-set-key (kbd "<mouse-3>") 'xref-find-definitions)
;; (global-set-key (kbd "<mouse-4>") 'xref-go-back)

;; Disable mouse highlighting when typing
;; (setq mouse-highlight 1)

(defun conf--disable-keys (map keys)
  (dolist (key keys)
    (define-key map (kbd key) nil)))

(defun move-up (amount)
  (deactivate-mark)
  (condition-case nil
      (scroll-down amount)
    (error nil))
  (previous-line amount))
(defun move-down (amount)
  (deactivate-mark)
  (condition-case nil
      (scroll-up amount)
    (error nil))
  (next-line amount))

(global-set-key (kbd "M-<up>")   (lambda () (interactive) (move-up 4)))
(global-set-key (kbd "M-<down>") (lambda () (interactive) (move-down 4)))
;; Should move this to meow-motion map? -- this would only be used on motion map
(global-set-key (kbd "M-j")   (lambda () (interactive) (move-down 4)))
(global-set-key (kbd "M-k") (lambda () (interactive) (move-up 4)))
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

(global-set-key (kbd "M-q M-h")  'windmove-left)
(global-set-key (kbd "M-q M-j")  'windmove-down)
(global-set-key (kbd "M-q M-k")    'windmove-up)
(global-set-key (kbd "M-q M-l") 'windmove-right)

(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "M-,") 'Info-history-back)
  (define-key Info-mode-map (kbd "C-M-,") 'Info-history-forward)
  (define-key Info-mode-map (kbd "e") nil))

;; fix some coding systems
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'utf8 'utf-8)

(defun conf--region-more-than-one-line-p ()
  "Return t if the selected region spans more than one line, nil otherwise.
Returns nil if there is no active region."
  (when (use-region-p)
    (save-excursion
      (let* ((begin (region-beginning))
             (end (region-end))
             (begin-line (progn (goto-char begin) (line-number-at-pos)))
             (end-line (progn (goto-char end) (line-number-at-pos))))
        (/= begin-line end-line)))))

;; Duplicate region
(defun duplicate-line-or-region (&optional n)
  (interactive "*p")
  (let ((use-region (and (use-region-p)
                         (conf--region-more-than-one-line-p))))
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
  (define-key c-mode-base-map (kbd "C-d") nil)
  (define-key c-mode-base-map (kbd "M-e") nil))
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
        "\\*help\\*"
        "\\*Ediff.*\\*"
        "*Shell Command Output*"))

(defun is-buffer-valid (buffer-name)
  (not (cl-loop for boring-buffer in boring-buffers
                thereis (string-match boring-buffer buffer-name))))

(defun conf--skip-temp-buffers (func)
  (interactive)
  (let ((bread-crumb (buffer-name)))
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

;; better process performance
(setq read-process-output-max (* 1024 1024))

;; Testing this setting, greatly improve the compilation output speed
(setq process-adaptive-read-buffering nil)

(defun conf--backward-delete-word ()
  "Delete word backwards, and delete matching pair if at point."
  (interactive)
  (let ((pt (point)))
    (when (electric-pair-mode)
      (while (and (char-before)
                  (char-after)
                  (or (eq (matching-paren (char-before)) (char-after))
                      (and (eq (char-before) (char-after))
                           (eq (char-before) (string-to-char "\"")))))
        (delete-char 1)
        (delete-char -1))
      (delete-region (point) (progn (backward-word 1) (point))))))

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

;; Do the same thing with embark
(defun conf--undo ()
  (interactive)
  (if (equal 'select (car-safe (meow--selection-type)))
      (let ((undo-fu-allow-undo-in-region nil))
        (call-interactively 'undo-fu-only-undo))
    (call-interactively 'undo-fu-only-undo)))

(use-package undo-fu
  :custom
  (undo-fu-ignore-keyboard-quit nil)
  (undo-fu-allow-undo-in-region t)
  :config
  (global-unset-key (kbd "M-z"))
  (global-set-key (kbd "M-z")   'conf--undo)
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

(use-package password-store)
(use-package pass)
(auth-source-pass-enable)
(setq epa-file-select-keys 'silent)
(setq epa-file-encrypt-to '("C37350DE46EE427FC9FA5ADFF63419C720EB67CE"))

(defun conf--toggle-flymake-error-color ()
  "Toggle the flymake-error face between its original red underline and no color."
  (interactive)
  (let ((current-underline (face-attribute 'flymake-error :underline)))
    (if (and current-underline
             (listp current-underline)
             (string= (plist-get current-underline :color) "Red1"))
        ;; Currently has red color, remove it
        (progn
          (set-face-attribute 'flymake-error nil :underline nil)
          (message "Flymake error color disabled"))
      ;; Currently has no color or different color, restore original
      (progn
        (set-face-attribute 'flymake-error nil :underline '(:style wave :color "Red1"))
        (message "Flymake error color enabled")))))

(use-package flymake
  :bind
  (("C-c i f" . flymake-mode)
   ("C-c i t" . conf--toggle-flymake-error-color)
   ;; ("C-c i r" . conf--toggle-flymake-end-of-line)
   :map flymake-mode-map
   ("C-c i l" . flymake-show-buffer-diagnostics)
   ("C-c i p" . flymake-goto-prev-error)
   ("C-c i n" . flymake-goto-next-error))
  :custom
  (flymake-indicator-type 'fringes)
  ;; (flymake-no-changes-timeout nil)
  )

(defun conf--org-open-link-maybe()
  (interactive)
  (if (eq (car (org-element-context)) 'link)
      (call-interactively 'org-open-at-point)
    (call-interactively 'org-meta-return)))

(use-package org
  :straight (:type built-in)
  :bind
  (:map org-mode-map
        ("M-."        . org-open-at-point)
        ("M-<return>" . conf--org-open-link-maybe)
        ("M-<up>"     . (lambda () (interactive) (move-up 4)))
        ("M-<down>"   . (lambda () (interactive) (move-down 4)))
        ("M-H"        . org-shiftmetaleft)
        ("M-L"        . org-shiftmetaright)
        ("C-c /"      . nil)
        ("C-c C-u"    . nil)
        ("M-,"        . org-mark-ring-goto)
        ("M-h"        . nil)
        :map org-read-date-minibuffer-local-map
        ("M-h"        . org-calendar-backward-day)
        ("M-j"        . org-calendar-forward-week)
        ("M-k"        . org-calendar-backward-week)
        ("M-l"        . org-calendar-forward-day))
  :config
  (setq org-startup-folded t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select t)
  (setq org-special-ctrl-a/e t)
  (setq org-src-preserve-indentation t)

  (setq org-M-RET-may-split-line '((default . nil)))
  ;; (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-tags-column -90)

  (defun conf--org-table-align-after-yank (&rest _args)
    "Align org table after yanking if point is in a table."
    (when (and (eq major-mode 'org-mode)
               (org-at-table-p))
      (org-table-align)))

  (advice-add 'yank :after #'conf--org-table-align-after-yank)
  (advice-add 'yank-pop :after #'conf--org-table-align-after-yank)

  (modify-syntax-entry ?= "." org-mode-syntax-table)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t) (python . t) (sql . t)))
  (setq org-babel-default-header-args:sql
        '((:engine . "postgresql")))
  (setq org-babel-default-header-args:elisp
        '((:lexical . t)))
  (add-to-list 'org-src-lang-modes '("json" . js-json))
  (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("jsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("typescript" . tsx-ts)))

(use-package org-agenda
  :straight (:type built-in)
  :bind
  (("C-c A"      . org-agenda)
   ("C-c Z"      . org-capture)
   ;; :map org-agenda-mode-map
   ;; ("L"          . nil)
   )
  :config
  (setq org-directory "~/Dropbox/denotes/")
  (setq org-agenda-files (list org-directory "~/Dropbox/todo.org"))
  (setq org-default-notes-file "~/Dropbox/todo.org")

  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file "")
           "* TODO %?\n%a"))))

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

(add-to-list 'electric-pair-pairs '(?` . ?`))
(add-to-list 'insert-pair-alist '(?` ?`))

(use-package vundo
  :bind
  (("C-x u" . vundo)
   ("C-x C-u" . vundo)))

(defun delete-until-slash ()
  (interactive)
  (let ((start-pos (if (minibufferp)
                       (minibuffer-prompt-end)
                     (point-min))))
    ;; Only delete the character before if it's past the start position
    (when (and (memq (char-before) '(?/ ?:))
               (> (point) start-pos))
      (delete-char -1))
    ;; Search backwards for separator, but don't go past the start position
    (if (re-search-backward "[:/]" start-pos t)
        ;; Found separator: delete everything after it and reinsert the separator
        (let ((separator (buffer-substring (point) (1+ (point)))))
          (delete-region (point) (point-max))
          (insert separator))
      ;; No separator found: delete everything until start position
      (delete-region start-pos (point-max)))))

(defun delete-until-slash-maybe ()
  (interactive)
  (if (eq 'file (vertico--metadata-get 'category))
      (delete-until-slash)
    (conf--backward-delete-word)))


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

(use-package cmake-mode
  :if (not (eq system-type 'windows-nt))
  :defer t
  :bind
  (:map cmake-mode-map
        ;; dump-jump doesn't work on cmake
        ("M-." . conf--consult-ripgrep))
  :config
  (setq cmake-tab-width 4))

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

(use-package phi-search
  :config
  (setq phi-search-limit 10000))

(setq mc/list-file (expand-file-name "mc-lists.el" (file-name-directory load-file-name)))
(use-package multiple-cursors
  :bind
  (("M-m"      . mc/mark-next-like-this)
   ;; ("C-M"      . newline)
   ;; ("C-M-m"    . newline)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("<return>" . nil)
   ("M-n" . mc/skip-to-next-like-this)
   ("M-N" . mc/unmark-next-like-this)   ; not sure about this one
   ("M-v" . nil))
  :config
  (add-to-list 'mc/unsupported-minor-modes 'electric-indent-mode)
  (add-to-list 'mc/unsupported-minor-modes 'corfu-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (format "%s/snippets" conf--base-dir)))
  (yas-global-mode 1))

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x v l" . magit-log-buffer-file)
   ("C-x v f" . magit-find-file)
   ("C-x v s" . magit-show-commit)
   ("C-x v g" . magit-blame-addition)
   :map magit-status-mode-map
   ("M-p" . nil)
   :map magit-diff-mode-map
   ("M-p" . nil)
   :map magit-hunk-section-map
   ("C-j" . nil))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-visit-avoid-head-blob t)
  (magit-auto-revert-immediately t)
  (magit-bury-buffer-function (lambda (_) (magit-mode-quit-window t)))
  (vc-display-status nil)
  (magit-diff-visit-prefer-worktree t))

;; This git is faster got some reason
(let ((git-path "/Applications/Xcode.app/Contents/Developer/usr/bin/git"))
  (when (file-exists-p git-path)
    (setq magit-git-executable git-path)))

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

(setq github-token (password-store-get "github-token"))

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

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c d") #'conf--visit-circle-ci)
  (define-key magit-mode-map (kbd "C-c p") #'conf--visit-pull-request-url-github))

(use-package git-timemachine
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine")
  :config
  (add-hook 'git-timemachine-mode-hook #'font-lock-ensure)
  (add-hook 'git-timemachine-mode-hook #'meow--switch-to-motion))

(use-package magit-delta
  :straight (:fork (:host github :repo "Azkae/magit-delta"))
  :if (executable-find "delta")
  :bind
  :custom
  (magit-delta-default-dark-theme "Monokai Extended")
  (magit-delta-default-light-theme "Github")
  (magit-delta-hide-plus-minus-markers nil)
  (magit-delta-max-size 200000)
  :config
  (add-hook 'magit-mode-hook #'(lambda () (magit-delta-mode +1))))

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
                (read-only-mode +1)))))

(use-package yaml-mode
  :hook
  (yaml-mode . toggle-truncate-lines))

(use-package swift-mode
  :defer t)

(use-package cython-mode
  :defer t)

(use-package glsl-mode
  :defer t)

(use-package jinja2-mode
  :defer t)

(use-package nhexl-mode)

(define-key hexl-mode-map (kbd "M-X") nil)
(define-key hexl-mode-map (kbd "M-q") nil)
(define-key hexl-mode-map (kbd "C-x C-h") nil)
(define-key hexl-mode-map (kbd "C-x C-j") nil)
(define-key hexl-mode-map (kbd "C-x C-k") nil)
(define-key hexl-mode-map (kbd "C-x C-l") nil)
(define-key hexl-mode-map (kbd "M-f") nil)

(use-package dockerfile-mode)

(defun conf--xref-find-definitions ()
  (interactive)
  (let ((this-command 'xref-find-definitions))
    (deactivate-mark)
    (call-interactively 'xref-find-definitions)))

(defun conf--xref-find-references ()
  (interactive)
  (let ((this-command 'xref-find-references))
    (deactivate-mark)
    (call-interactively 'xref-find-references)))

(use-package eglot
  :straight (:type built-in)
  :bind
  (("M-." . conf--xref-find-definitions)
   ("M-?" . conf--xref-find-references)
   :map eglot-mode-map
   ("<mouse-3>" . eglot-code-actions-at-mouse))
  :hook
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . (lambda () (setq-local eglot-ignored-server-capabilities '(:inlayHintProvider))))
  ((typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  (rust-mode . eglot-ensure)
  :custom
  (eglot-report-progress t)
  ;; help with perf:
  (eglot-events-buffer-size 0)
  (eglot-code-action-indicator "h")
  :config

  ;; (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--completion-style=detailed")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--completion-style=detailed" "--header-insertion-decorators=0" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs `(web-mode . ,(eglot-alternatives
                                                     '(("vscode-html-language-server" "--stdio")
                                                       ("html-languageserver" "--stdio")))))
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; Disable auto indent after '}' on cpp mode, may break a few things..
  ;; (remove-hook 'post-self-insert-hook 'eglot--post-self-insert-hook t)

  (fset #'jsonrpc--log-event #'ignore)

  (add-to-list 'eglot-stay-out-of 'company-backends)
  ;; Enable flymake only on save:
  ;; This allows to trigger flymake only when the sever published diagnostics
  (defun conf--eglot-publishDiagnostics (server method &rest args)
    (when (eq method 'textDocument/publishDiagnostics)
      (let ((uri (plist-get args :uri)))
        (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
          (with-current-buffer buffer
            (when (bound-and-true-p sideline-mode)
              (sideline--reset)
              (sideline-render-this)))))))

  (advice-add 'eglot-handle-notification :after #'conf--eglot-publishDiagnostics))



(el-patch-feature eglot)

;; Fix highlighting of eldoc parameters, fixed in emacs31
;; See 04a8faef0948f46b16172855ee337f59819f22a7
(with-eval-after-load 'eglot
  (el-patch-defun eglot--sig-info (sig &optional sig-active briefp)
    (eglot--dbind ((SignatureInformation)
                   ((:label siglabel))
                   ((:documentation sigdoc)) parameters activeParameter)
        sig
      (with-temp-buffer
        (insert siglabel)
        ;; Add documentation, indented so we can distinguish multiple signatures
        (when-let (doc (and (not briefp) sigdoc (eglot--format-markup sigdoc)))
          (goto-char (point-max))
          (insert "\n" (replace-regexp-in-string "^" "  " doc)))
        ;; Try to highlight function name only
        (let (first-parlabel)
          (cond ((and (cl-plusp (length parameters))
                      (vectorp (setq first-parlabel
                                     (plist-get (aref parameters 0) :label))))
                 (save-excursion
                   (goto-char (elt first-parlabel 0))
                   (skip-syntax-backward "^w")
                   (add-face-text-property (point-min) (point)
                                           'font-lock-function-name-face)))
                ((save-excursion
                   (goto-char (point-min))
                   (looking-at "\\([^(]*\\)([^)]*)"))
                 (add-face-text-property (match-beginning 1) (match-end 1)
                                         'font-lock-function-name-face))))
        ;; Now to the parameters
        (cl-loop
         with active-param = (el-patch-swap (or sig-active activeParameter)
                                            (or activeParameter sig-active))
         for i from 0 for parameter across parameters do
         (eglot--dbind ((ParameterInformation)
                        ((:label parlabel))
                        ((:documentation pardoc)))
             parameter
           ;; ...perhaps highlight it in the formals list
           (when (eq i active-param)
             (save-excursion
               (goto-char (point-min))
               (pcase-let
                   ((`(,beg ,end)
                     (if (stringp parlabel)
                         (let ((case-fold-search nil))
                           (and (search-forward parlabel (line-end-position) t)
                                (list (match-beginning 0) (match-end 0))))
                       (mapcar #'1+ (append parlabel nil)))))
                 (if (and beg end)
                     (add-face-text-property
                      beg end
                      'eldoc-highlight-function-argument)))))
           ;; ...and/or maybe add its doc on a line by its own.
           (let (fpardoc)
             (when (and pardoc (not briefp)
                        (not (string-empty-p
                              (setq fpardoc (eglot--format-markup pardoc)))))
               (insert "\n  "
                       (propertize
                        (if (stringp parlabel) parlabel
                          (apply #'substring siglabel (mapcar #'1+ parlabel)))
                        'face (and (eq i active-param) 'eldoc-highlight-function-argument))
                       ": " fpardoc)))))
        (buffer-string)))))

(use-package js
  :straight (:type built-in)
  :hook
  (js-json-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package rust-mode
  :defer t)

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

;; (use-package emojify
;;   :config
;;   (emojify-set-emoji-styles '(unicode))
;;   (setq emojify-display-style 'unicode))

;; (use-package solidity-mode
;;   :config
;;   (add-hook 'solidity-mode-hook '--set-tab-with))

(use-package sqlformat
  :defer t
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g"))

  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :straight (vterm :fork (:host github :repo "Azkae/emacs-libvterm"))
  :init
  (setq vterm-keymap-exceptions '("M-q" "C-q" "C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "M-z" "M-X" "M-O" "M-e" "M-E" "M-l" "M-h"))
  :bind
  (:map vterm-mode-map
  ("M-z" . 'vterm-copy-mode)
  ("<mouse-1>" . 'vterm-copy-mode)
  ("M-f" . (lambda () (interactive) (vterm-copy-mode 1) (conf--consult-line)))
  ("M-a" . (lambda () (interactive) (vterm-send "C-l")))
  :map vterm-copy-mode-map
  ("M-z" . 'vterm-copy-mode)
  ("M-v" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-yank)))
  ("C-c C-c" . (lambda () (interactive) (vterm-copy-mode -1) (vterm--self-insert)))
  ("DEL" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-send-backspace)))
  ("SPC" . 'vterm-copy-mode))
  :config
  (define-key vterm-copy-mode-map [remap self-insert-command] #'(lambda() (interactive) (vterm-copy-mode -1)
                                                                  (vterm--self-insert)))
  (setq vterm-timer-delay 0.05)
  (setq vterm-ignore-cursor-change t)
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

(defun conf--vterm-toggle-project()
  (interactive)
  (let* ((dir (expand-file-name (project-root (project-current))))
         (cd-cmd (concat " cd " (shell-quote-argument dir))))
    (setq conf--vterm-cd-command cd-cmd))

  (if (and (not (derived-mode-p 'vterm-mode))
           (vterm-toggle--get-window))
      (vterm-toggle-show)
    (if (derived-mode-p 'vterm-mode)
        (conf--vterm-hide)
      (vterm-toggle))))

(use-package vterm-toggle
  :if (not (eq system-type 'windows-nt))
  :bind
  (("M-e" . conf--vterm-toggle)
   ("M-E" . (lambda () (interactive) (conf--vterm-save-cd) (vterm-toggle-cd)))
   :map vterm-mode-map
   ("M-E" . conf--vterm-insert-cd)
   ("<M-right>" . vterm-toggle-forward)
   ("<M-left>" . vterm-toggle-backward)
   ("M-l" . vterm-toggle-forward)
   ("M-h" . vterm-toggle-backward)
   :map vterm-copy-mode-map
   ("M-E" . (lambda () (interactive) (vterm-copy-mode -1) (conf--vterm-insert-cd)))
   ("<M-right>" . vterm-toggle-forward)
   ("<M-left>" . vterm-toggle-backward)
   ("M-l" . vterm-toggle-forward)
   ("M-h" . vterm-toggle-backward))
  :config
  (setq vterm-toggle-hide-method 'bury-all-vterm-buffer)
  (setq vterm-toggle-reset-window-configration-after-exit nil)
  :autoload vterm-toggle-cd-show vterm-toggle--get-window)

(use-package multi-vterm
  :if (not (eq system-type 'windows-nt))
  :bind
  (:map vterm-mode-map
        ("M-t" . multi-vterm)
        :map vterm-copy-mode-map
        ("M-t" . multi-vterm)))

(use-package pyvenv)

(use-package kotlin-mode
  :defer t
  :config
  (modify-syntax-entry ?< "(>" kotlin-mode-syntax-table)
  (modify-syntax-entry ?> ")<" kotlin-mode-syntax-table))

(use-package groovy-mode
  :defer t)

(use-package string-inflection
  :bind
  ("C-c _" . string-inflection-toggle))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 4))

(with-eval-after-load 'tsx-ts-mode
  (modify-syntax-entry ?` "\"" tsx-ts-mode-syntax-table))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package markdown-mode
  :bind
  (:map markdown-mode-map
   ("M-p" . nil))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("jsx" . tsx-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("html" . mhtml-mode))
  (add-to-list 'markdown-code-lang-modes '("json" . js-json-mode)))

(use-package with-editor
  :hook
  (vterm-mode . with-editor-export-editor))

(use-package treemacs
  :config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package eldoc-box
  :bind
  (("M-§" . eldoc-box-help-at-point))
  :custom
  (eldoc-idle-delay 0.2)
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))

(defun conf--corfu-active-p ()
  (and corfu-mode completion-in-region-mode))

(defun conf--corfu-reset()
  (interactive)
  (corfu-quit)
  (corfu--auto-complete-deferred))

;; (defun conf--corfu-post-command()
;;   "Refresh completion when prefix length is 3 and no candidates are found."
;;   (when (and corfu-mode completion-in-region-mode)
;;     (let* ((input (car corfu--input))
;;            (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
;;            (len (length str))
;;            (candidates corfu--candidates))
;;       (when (and (= len 3)
;;                  ;; (>= len 3)
;;                  ;; (= (% len 3) 0)
;;                  (not (try-completion str candidates)))
;;         (conf--corfu-reset)))))

(el-patch-feature corfu)

;; Disable completion starting with [ with pyright
(with-eval-after-load 'corfu
  (el-patch-defun corfu--capf-wrapper (fun &optional prefix)
  "Wrapper for `completion-at-point' FUN.
The wrapper determines if the Capf is applicable at the current position
and performs sanity checking on the returned result.  For non-exclusive
Capfs, the wrapper checks if the current input can be completed.  PREFIX
is a prefix length override, which is t for manual completion."
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
          (cons fun res))))))

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
        ("C-<return>" . corfu-insert)
        ("M-RET" . conf--corfu-reset))
  :hook
  (corfu-mode . (lambda ()
                  (add-hook 'yas-keymap-disable-hook 'conf--corfu-active-p nil t)
                  ;; (add-hook 'post-command-hook #'conf--corfu-post-command)
                  ))
  (git-commit-mode . (lambda () (corfu-mode -1)))
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
  (add-hook 'after-save-hook #'corfu-quit))

(add-to-list 'completion-styles-alist
             '(tab completion-basic-try-completion ignore
               "Completion style which provides TAB completion only."))


(use-package orderless
  :init
  (setq completion-styles '(tab orderless basic)))

(when (< emacs-major-version 29)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package async)

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
         (project-files project))

       (lambda (result)
         (puthash root
                  (mapcar (lambda (f) (file-relative-name f root)) result)
                  conf--project-files-cache))))
    (gethash root conf--project-files-cache)))

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

(defun conf--cape-dabbrev-and-enable-corfu ()
  (interactive)
  (when (not corfu-mode)
    (corfu-mode))
  (call-interactively 'cape-dabbrev))

(use-package cape
  :bind
  (("M-*" . conf--cape-dabbrev-and-enable-corfu))
  :init
  (add-hook 'completion-at-point-functions #'cape-file)

  (defun conf--dabbrev-buffers ()
    (cape--buffer-list
     (lambda (buf)
       (let ((mode (buffer-local-value 'major-mode buf)))
         (and (or (provided-mode-derived-p mode #'text-mode)
                  (provided-mode-derived-p mode #'prog-mode))
              (< (buffer-size buf) 200000))))))

  (setq cape-dabbrev-buffer-function 'conf--dabbrev-buffers)

  (defun conf--setup-comint-mode-completions ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-prefix-length (cape-capf-super
                                           'conf--project-files-capf
                                           #'cape-dabbrev
                                           'conf--aidermacs-keywords-completion-at-point)
                                          3)
                 #'cape-dabbrev
                 'comint-completion-at-point t)))

  (add-hook 'comint-mode-hook 'conf--setup-comint-mode-completions)

  ;; Disable ispell completion on text buffers
  (setq text-mode-ispell-word-completion nil))

(use-package apheleia
  :hook
  ((python-mode python-ts-mode) . apheleia-mode)
  ((c++-mode c++-ts-mode) . apheleia-mode)
  (terraform-mode . apheleia-mode)
  ((typescript-ts-mode tsx-ts-mode) . apheleia-mode))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :custom
  (fancy-compilation-override-colors nil))
(define-key compilation-mode-map (kbd "M-p") nil)

(conf--disable-keys diff-mode-map '("M-p" "M-h" "M-j" "M-k" "M-l"))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package zygospore
  :bind
  (("C-x 1" . zygospore-toggle-delete-other-windows))
  :init
  (setq zygospore-spore-formation-register-name ?📘))

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

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  ;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'git-commit-post-finish-hook 'diff-hl-magit-post-refresh)
  (add-hook 'magit-post-commit-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-update-async t)
  (diff-hl-disable-on-remote t))

;; Poetry/UV project tracking

(defvar conf--python-current-root nil)
(defun conf--python-track-venv()
  (interactive)
  (when (not (file-remote-p buffer-file-name))
    (let ((root (locate-dominating-file default-directory "pyproject.toml")))
      (when (and root (not (string= root conf--python-current-root)))
        (message "checking for root %s" root)
        (if-let* ((venv-path (expand-file-name ".venv" root))
                  ((file-directory-p venv-path)))
            (progn
              (message "Applying venv: %s (.venv)" venv-path)
              (setq conf--python-current-root root)
              (pyvenv-activate venv-path))
          (let ((process-environment (cl-remove-if
                                    (lambda (element) (string-prefix-p "VIRTUAL_ENV=" element))
                                    process-environment)))
          (let ((venv (string-trim (shell-command-to-string "poetry env info --path"))))
            (message "Applying venv: %s (poetry)" venv)
            (setq conf--python-current-root root)
            (pyvenv-activate venv))))))))

(add-hook 'find-file-hook 'conf--python-track-venv)

(use-package python
  :straight (:type built-in)
  :bind
  (:map python-ts-mode-map
        ("C-c C-l" . nil))
  :config
  (defun my-remove-python-completion-at-point ()
    "Remove python-completion-at-point from completion-at-point-functions."
    (setq completion-at-point-functions
          (remove #'python-completion-at-point completion-at-point-functions)))

  (add-hook 'python-mode-hook #'my-remove-python-completion-at-point)
  (add-hook 'python-ts-mode-hook #'my-remove-python-completion-at-point))

;; (define-minor-mode conf--poetry-tracking-mode
;;   "Global mode to track poetry projects"
;;   :global t
;;   :group 'poetry
;;   (if conf--poetry-tracking-mode
;;       (add-hook 'projectile-after-switch-project-hook 'conf--poetry-track-virtualenv)
;;     (remove-hook 'projectile-after-switch-project-hook 'conf--poetry-track-virtualenv)))
;; (conf--poetry-tracking-mode)

(use-package treesit
  :straight (:type built-in)
  :preface
  (defun my--setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               ;; (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               ;; (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
	           ;; (wgsl . ("https://github.com/szebniok/tree-sitter-wgsl" "master"))
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (my--setup-install-grammars))

(setq-default css-indent-offset 2)

(require 'windmove)
(defun conf--move-buffer (dir)
  (let* ((buf (current-buffer))
         (window (windmove-find-other-window dir))
         (window-point (point))
         (window-start (window-start)))
    (when (and window (not (window-minibuffer-p window)))
      (conf--prev-buffer)
      (windmove-do-window-select dir)
      (switch-to-buffer buf)
      (set-window-start nil window-start)
      (goto-char window-point))))

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

(global-set-key (kbd "C-M-h") 'conf--move-buffer-left)
(global-set-key (kbd "C-M-l") 'conf--move-buffer-right)
(global-set-key (kbd "C-M-k") 'conf--move-buffer-up)
(global-set-key (kbd "C-M-j") 'conf--move-buffer-down)

(repeat-mode 1)
(put 'other-window 'repeat-map nil)

(use-package dape
  :init
  (setq dape-key-prefix "\C-cd")
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  :config
  ;; Allow setting breakpoint with mouse
  (dape-breakpoint-global-mode)
  (set-face-attribute 'dape-exception-description-face nil :foreground "black")

  (add-hook 'dape-start-hook 'delete-other-windows)

  ;; customize dape repeat
  (defvar dape-global-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" #'dape-pause)
      (define-key map "c" #'dape-continue)
      (define-key map "n" #'dape-next)
      (define-key map "s" #'dape-step-in)
      (define-key map "o" #'dape-step-out)
      (define-key map "r" #'dape-restart)
      (define-key map "u" #'dape-until)
      (define-key map "b" #'dape-breakpoint-toggle)
      (define-key map "B" #'dape-breakpoint-remove-all)
      (define-key map "t" #'dape-select-thread)
      (define-key map "S" #'dape-select-stack)
      (define-key map ">" #'dape-stack-select-down)
      (define-key map "<" #'dape-stack-select-up)
      (define-key map "q" #'dape-quit)
      map))

  (dolist (cmd '(dape
                 dape-pause
                 dape-continue
                 dape-next
                 dape-step-in
                 dape-step-out
                 dape-restart
                 dape-restart-frame
                 dape-until
                 dape-breakpoint-log
                 dape-breakpoint-expression
                 dape-breakpoint-hits
                 dape-breakpoint-toggle
                 dape-breakpoint-remove-all
                 dape-stack-select-up
                 dape-stack-select-down
                 dape-select-stack
                 dape-select-thread
                 dape-watch-dwim
                 dape-evaluate-expression))
    (put cmd 'repeat-map 'dape-global-repeat-map))

  (add-to-list 'dape-configs
    '(debugpy-attach-port
       modes (python-mode python-ts-mode)
       port 8787
       :request "attach"
       :type "python"
       :justMyCode nil
       :showReturnValue t))

  (defun conf--dape-read-pid ()
    "Read pid of active processes if possible."
    (if-let ((pids (list-system-processes)))
        (let ((collection
               (mapcar (lambda (pid)
                         (let ((args (alist-get 'args (process-attributes pid))))
                           (cons (concat
                                  (format "%d" pid)
                                  (when args
                                    (format ": %s" args)))
                                 pid)))
                       pids)))
          (alist-get (completing-read "Pid: " collection)
                     collection nil nil 'equal))
      (read-number "Pid: ")))

  (defun conf--dape-config-autopid (config)
    (if (eq (plist-get config :processId) :autopid)
      (let ((pid (conf--dape-read-pid)))
        (plist-put config :processId pid))
      config))

  (add-to-list 'dape-default-config-functions 'conf--dape-config-autopid)

  (add-to-list 'dape-configs
    '(debugpy-attach-pid
       modes (python-mode python-ts-mode)
       command "python3"
       command-args ("-m" "debugpy.adapter")
       :request "attach"
       :type "python"
       :processId :autopid
       :justMyCode nil
       :showReturnValue t)))

;; Build and run in debugger:
;;   codelldb-cc :cwd "/path/base_dir" :program "/path/base_dir/program" :args ["test"] compile "make -k"

;; Attach running process:
;;   codelldb-cc :cwd "/path/base_dir" :program "/path/base_dir/program" :request "attach" :pid <pid>

(use-package gdscript-mode
  :defer t
  :hook (gdscript-mode . eglot-ensure))

(use-package org-modern
  :config
  (global-org-modern-mode)

  ; (setq  ;; Org styling, hide markup etc.
  ;  org-hide-emphasis-markers t
  ;  org-pretty-entities t
  ;  org-ellipsis "…")
  (setq org-modern-fold-stars '(("▸" . "▾") ("▹" . "▿") ("▸" . "▾") ("▹" . "▿") ("▸" . "▾"))))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (setq org-startup-indented t)
  (setq org-modern-hide-stars nil)
  (setq org-indent-indentation-per-level 1)
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  ;; (set-face-attribute 'org-modern-bracket-line nil :family "Menlo")
  )

(use-package terraform-mode
  :defer t
  :hook
  (terraform-mode . eglot-ensure))

;; Disable M-o key in html
(with-eval-after-load 'mhtml-mode
  (add-hook 'mhtml-mode-hook #'(lambda() (bind-key "M-o" nil mhtml-mode-map))))
(with-eval-after-load 'html-ts-mode
  (define-key html-ts-mode-map (kbd "M-o") nil))

(with-eval-after-load 'nxml-mode
  (modify-syntax-entry ?< "." nxml-mode-syntax-table)
  (modify-syntax-entry ?> "." nxml-mode-syntax-table)
  (modify-syntax-entry ?/ "." nxml-mode-syntax-table)
  (add-hook 'nxml-mode-hook #'(lambda () (when (fboundp 'jinx-mode)
                                           (jinx-mode -1))))
  (add-hook 'nxml-mode-hook #'(lambda () (visual-line-mode -1))))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode))

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . (lambda () (visual-line-mode -1))))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defun conf--insert-emoji ()
  (interactive)
  (when (and (boundp 'meow-mode) (not (meow-insert-mode-p)))
    (meow-insert))
  (ns-do-show-character-palette))

(use-package gptel
  :hook
  (gptel-mode . corfu-mode)
  (gptel-mode . (lambda () (setq-local completion-at-point-functions '(gptel-preset-capf))))
  :bind
  (:map gptel-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-<return>" . gptel-send))
  :config
  (global-set-key (kbd "C-c , g") 'gptel)
  (global-set-key (kbd "C-c , m") 'gptel-menu)
  (global-set-key (kbd "C-c , a") 'gptel-add)
  (global-set-key (kbd "C-c , ,") 'gptel-menu)
  (global-set-key (kbd "C-c , t") 'gptel-tools)
  (when-let ((anthropic-api-key (password-store-get "anthropic-api-key")))
    (setq
     gptel-model 'claude-sonnet-4-20250514
     gptel-backend (gptel-make-anthropic "Claude"
				     :stream t
				     :key anthropic-api-key)))

  (defun conf--gptel-start-rewrite-session ()
    (interactive)
    (let ((buffer-name (generate-new-buffer-name "*gptel-rewrite*")))
      (gptel buffer-name nil "* @rewrite ")
      (switch-to-buffer buffer-name)))

  (global-set-key (kbd "C-c , r") 'gptel-rewrite)
  (global-set-key (kbd "C-c , R") 'conf--gptel-start-rewrite-session)

  (defun conf--gptel-start-quick-session ()
    (interactive)
    (let ((buffer-name (generate-new-buffer-name "*gptel-quick*")))
      (gptel buffer-name nil "* ")
      (switch-to-buffer buffer-name)))

  (global-set-key (kbd "C-c , q") 'conf--gptel-start-quick-session)

  (setq gptel-default-mode 'org-mode)
  (setq gptel-include-tool-results t)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* ")

  (add-hook 'gptel-mode-hook
            (lambda () (when (derived-mode-p 'org-mode)
                         (setq-local require-final-newline nil))))

  (add-to-list 'gptel-directives '(critical . "Prioritize substance, clarity, and depth. Challenge all my proposals, designs, and conclusions as hypotheses to be tested. Sharpen follow-up questions for precision, surfacing hidden assumptions, trade offs, and failure modes early. Default to terse, logically structured, information-dense responses unless detailed exploration is required. Skip unnecessary praise unless grounded in evidence. Explicitly acknowledge uncertainty when applicable. Always propose at least one alternative framing. Accept critical debate as normal and preferred. Treat all factual claims as provisional unless cited or clearly justified. Cite when appropriate. Acknowledge when claims rely on inference or incomplete information. Favor accuracy over sounding certain."))

  (add-to-list 'gptel-directives '(reword . "You are a text enhancement assistant. Your task is to rewrite the user's text with improved grammar, syntax, and expression while preserving their natural voice. Follow these guidelines:

1. *Fix errors*: Correct grammar, punctuation, spelling, and sentence structure
2. *Enhance clarity*: Improve word choice, sentence flow, and overall readability
3. *Preserve voice*: Maintain the user's original tone, style, and level of formality
4. *Keep meaning intact*: Ensure the core message and intent remain unchanged
5. *Subtle improvements*: Make enhancements feel natural, not forced or over-polished
6. *Formatting guideline*: Do not use em dashes (—) in your enhanced text. Use alternative punctuation such as commas, semicolons, parentheses, or periods instead.

Provide only the improved version unless the user requests explanations or has specific questions about the changes."))

  (gptel-make-preset 'rewrite
    :description "Text improvement and grammar correction"
    :system 'reword)

  (gptel-make-tool
   :name "elisp_eval"
   :function (lambda (code)
               (condition-case err
                   (let ((result (eval (read code))))
                     (format "Result: %S" result))
                 (error
                  (format "Error: %S" err))))
   :description "Evaluate elisp code and return the result"
   :args (list '(:name "code"
                       :type string
                       :description "The elisp code to evaluate"))
   :confirm t
   :include t
   :category "emacs-modify")

  ;; (gptel-make-tool
  ;;  :name "read_buffer"
  ;;  :function (lambda (buffer)
  ;;              (unless (buffer-live-p (get-buffer buffer))
  ;;                (error "error: buffer %s is not live." buffer))
  ;;              (with-current-buffer  buffer
  ;;                (buffer-substring-no-properties (point-min) (point-max))))
  ;;  :description "return the contents of an emacs buffer"
  ;;  :args (list '(:name "buffer"
  ;;                      :type string
  ;;                      :description "the name of the buffer whose contents are to be retrieved"))
  ;;  :confirm t
  ;;  :include t
  ;;  :category "emacs")

  (gptel-make-tool
   :name "elisp_describe_variable"
   :function (lambda (variable)
               (save-window-excursion (describe-variable (intern variable))))
   :description "describe variable of the current emacs session"
   :args (list '(:name "variable"
                       :type string
                       :description "variable name"))
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_describe_face"
   :function (lambda (variable)
               (save-window-excursion (describe-face (intern variable))))
   :description "describe variable of the current emacs session"
   :args (list '(:name "variable"
                       :type string
                       :description "face name"))
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_describe_function"
   :function (lambda (variable)
               (save-window-excursion (describe-function (intern variable))))
   :description "describe elisp function"
   :args (list '(:name "variable"
                       :type string
                       :description "function name"))
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_prefix_completion"
   :function (lambda (prefix limit page type show-private)
               (let* ((completions '())
                      (case-fold-search nil)
                      (max-results (or limit 50))
                      (page-num (or page 1))
                      (skip-count (* (1- page-num) max-results)))
                 (mapatoms
                  (lambda (symbol)
                    (when (and (string-prefix-p prefix (symbol-name symbol))
                               (or (eq show-private t)
                                   (not (string-match-p "--" (symbol-name symbol))))
                               (cond
                                ((string= type "function") (fboundp symbol))
                                ((string= type "variable") (boundp symbol))
                                ((string= type "face") (facep symbol))
                                (t (or (fboundp symbol) (boundp symbol) (facep symbol)))))
                      (push (symbol-name symbol) completions))))
                 (let* ((sorted-completions (sort completions #'string<))
                        (total-count (length sorted-completions))
                        (total-pages (ceiling (/ (float total-count) max-results)))
                        (offset-completions (seq-drop sorted-completions skip-count))
                        (final-completions (seq-take offset-completions max-results)))
                   (if (and (> total-count skip-count)
                            (> (length offset-completions) max-results))
                       (append final-completions
                               (list (format "... page %d of %d (total: %d symbols)"
                                             page-num total-pages total-count)))
                     final-completions))))
   :description "Get completion candidates for elisp symbols matching a prefix"
   :args (list '(:name "prefix"
                       :type string
                       :description "The partial symbol name to complete")
               '(:name "limit"
                       :type integer
                       :description "Maximum number of results to return (default: 50)"
                       :optional t)
               '(:name "page"
                       :type integer
                       :description "Page number to display (default: 1, starts from 1)"
                       :optional t)
               '(:name "type"
                       :type string
                       :enum ["function" "variable" "face"]
                       :description "Filter by symbol type: function, variable, or face. If not specified, returns all types"
                       :optional t)
               '(:name "show-private"
                       :type boolean
                       :description "If t, show private symbols. If nil, hide private symbols (the default)"
                       :optional t))
   :confirm nil
   :include t
   :category "emacs")

  (gptel-make-tool
   :name "elisp_regexp_completion"
   :function (lambda (regexp limit page type show-private)
               (let* ((completions '())
                      (case-fold-search nil)
                      (max-results (or limit 50))
                      (page-num (or page 1))
                      (skip-count (* (1- page-num) max-results)))
                 (mapatoms
                  (lambda (symbol)
                    (when (and (string-match-p regexp (symbol-name symbol))
                               (or (eq show-private t)
                                   (not (string-match-p "--" (symbol-name symbol))))
                               (cond
                                ((string= type "function") (fboundp symbol))
                                ((string= type "variable") (boundp symbol))
                                ((string= type "face") (facep symbol))
                                (t (or (fboundp symbol) (boundp symbol) (facep symbol)))))
                      (push (symbol-name symbol) completions))))
                 (let* ((sorted-completions (sort completions #'string<))
                        (total-count (length sorted-completions))
                        (total-pages (ceiling (/ (float total-count) max-results)))
                        (offset-completions (seq-drop sorted-completions skip-count))
                        (final-completions (seq-take offset-completions max-results)))
                   (if (and (> total-count skip-count)
                            (> (length offset-completions) max-results))
                       (append final-completions
                               (list (format "... page %d of %d (total: %d symbols)"
                                             page-num total-pages total-count)))
                     final-completions))))
   :description "Get completion candidates for elisp symbols matching a regexp"
   :args (list '(:name "regexp"
                       :type string
                       :description "The partial symbol name to complete")
               '(:name "limit"
                       :type integer
                       :description "Maximum number of results to return (default: 50)"
                       :optional t)
               '(:name "page"
                       :type integer
                       :description "Page number to display (default: 1, starts from 1)"
                       :optional t)
               '(:name "type"
                       :type string
                       :enum ["function" "variable" "face"]
                       :description "Filter by symbol type: function, variable, or face. If not specified, returns all types"
                       :optional t)
               '(:name "show-private"
                       :type boolean
                       :description "If t, show private symbols. If nil, hide private symbols (the default)"
                       :optional t))
   :confirm nil
   :include t
   :category "emacs")

  (gptel-make-preset 'visible-buffers
    :description "Include the full text of all buffers visible in the frame."
    :context
    '(:eval (mapcar #'window-buffer (remove (selected-window)
                                            (window-list (selected-frame))))))

  (gptel-make-preset 'visible-text
    :description "Include visible text from all windows in the frame."
    :context
    '(:eval (mapcar (lambda (win) ;; Create (<buffer> :bounds ((start . end)))
                      `(,(window-buffer win)
                        :bounds ((,(window-start win) . ,(window-end win)))))
                    (remove (selected-window) (window-list (selected-frame))))))

  (defun gptel-review-code ()
    "Send current region or buffer for code review in a dedicated buffer."
    (interactive)
    (let* ((review-buffer (get-buffer-create "*Code Review*"))
           (code-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (buffer-substring-no-properties (point-min) (point-max))))
           (source-info (format "Code review for %s:\n" (buffer-name)))
           (prompt (concat "Please review this code:\n\n=\n" code-text "\n=")))

      ;; Setup the review buffer
      (with-current-buffer review-buffer
        (erase-buffer)
        (markdown-mode)
        (insert source-info)
        (insert "=" (make-string 50 ?=) "=\n\n")
        (insert "Requesting code review...\n\n"))

      ;; Send request directly to the review buffer
      (gptel-request prompt
                     :buffer review-buffer
                     :position (with-current-buffer review-buffer (point-max))
                     :system "You are a code reviewer. Provide a concise review focusing on critical issues, bugs, and immediate improvements. Keep responses brief."
                     :stream t)

      ;; Show the buffer
      (pop-to-buffer review-buffer))))

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

(defun conf--vertico-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

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
        ;; ("M-r" . nil)
        ("M-<up>" . (lambda() (interactive) (scroll-other-window-down 5)))
        ("M-<down>" . (lambda() (interactive) (scroll-other-window 5)))
        ("M-k" . (lambda() (interactive) (scroll-other-window-down 5)))
        ("M-j" . (lambda() (interactive) (scroll-other-window 5)))
        ("C-SPC" . (lambda () (interactive) (call-interactively 'embark-select) (vertico-next)))
        ("C-j" . next-line)
        ("C-k" . previous-line)
        ("C-<return>" . vertico-exit)
        ("C-h" . left-char)
        ("C-l" . right-char)
        ("C-d" . conf--vertico-preview))
        ("M-t" . vertico-buffer-mode)
        ("M-RET" . vertico-exit)
        (:map minibuffer-local-map
              ("C-h" . left-char)
              ("C-l" . right-char)
              ("C-p" . previous-history-element)
              ("C-n" . next-history-element))
  :init
  (vertico-mode)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (defun conf--minibuffer-complete-or-insert-directory ()
    (interactive)
    (if (or (eq (vertico--metadata-get 'category) 'file) crm-completion-table)
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
  :bind
  (("C-c t" . eglot-code-actions))
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

;; TODO: this could be replaced by just rebinding C-g to `abort-minibuffers`
(defun conf--deactivate-region-before-quit ()
  "Deactivate region before keyboard quit if delete-selection-mode is active."
  (when (and delete-selection-mode (region-active-p))
    (deactivate-mark)))

(advice-add 'minibuffer-keyboard-quit :before #'conf--deactivate-region-before-quit)

(defun conf--minibuffer-candidate ()
  (if (and (bound-and-true-p vertico--input) (minibufferp))
      (cons (vertico--metadata-get 'category) (vertico--candidate))
    nil))

(defun conf--minibuffer-selected-directory-maybe ()
  (let ((minibuffer-candidate (conf--minibuffer-candidate)))
    (when minibuffer-candidate
      (pcase (car minibuffer-candidate)
        ('project-file (project-root
                        (project-current nil (expand-file-name (cdr minibuffer-candidate)))))
        ('file (let ((path (expand-file-name (substitute-in-file-name (cdr minibuffer-candidate)))))
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
    (conf--exit-minibuffer-and-execute (lambda () (let ((this-command 'consult-ripgrep))
                                                    (consult-ripgrep path))))))

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

(defun conf--find-in-open-buffers ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'consult-line-multi)))

(use-package consult
  :bind
  (
   ("M-f"         . conf--consult-line)
   ("M-F"         . conf--find-in-open-buffers)
   ("C-x b"       . consult-buffer)
   ("M-R"         . conf--select-directory-and-ripgrep)
   ("M-X"         . execute-extended-command)
   ("M-o"         . find-file)
   ("M-O"         . consult-buffer)
   ("C-x r r"     . consult-bookmark)
   ("C-x r s"     . bookmark-set)
   :map prog-mode-map
   ;; ("M-."         . conf--consult-ripgrep)
   :map minibuffer-local-map
   ([M-backspace] . delete-until-slash-maybe)
   ("C-x g" . conf--magit-in-selected-directory)
   ("M-e" . conf--vterm-in-selected-directory)
   )
  :custom
  (consult-async-input-debounce 0.1)
  (consult-async-input-throttle 0.3)
  (consult-line-start-from-top 't)
  (consult-narrow-key "<")
  :config
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)

  (add-to-list 'consult-preview-excluded-files "\\.gpg\\'")
  (require 'consult-compile))

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-file-register
 consult--source-recent-file consult--source-project-recent-file
 conf--consult-ripgrep conf--select-directory-and-ripgrep
 project-switch-project conf--find-in-open-buffers consult-line-multi
 :preview-key '(:debounce 0.1 any)) ;; Option 1: Delay preview

;; (consult-customize
;;  find-file
;;  :preview-key '(:debounce 0.01 any))


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

;; Idea: We could add a wrapper to embark-act that deactivate the region if
;; the selection was made via meow-next-word (not symbol?) and maybe
;; don't deactivate region with universal prefix
(use-package embark
  :bind
  (("M-/" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-help-key "?")
  ;; (setq embark-target-finders (delete 'embark-target-active-region embark-target-finders))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-default-action-overrides '(file . find-file))
  (define-key embark-region-map "h" nil))
(require 'embark-consult)

(defvar consult--previous-point nil
  "Location of point before entering minibuffer.
    Used to preselect nearest headings and imenu items.")

(defvar consult--vertico-updated-selection nil
  "Used to see if vertico recomputed candidates.")

(defun consult--set-previous-point (&optional arg1 arg2)
  "Save location of point. Used before entering the minibuffer."
  (setq consult--previous-point (point)))

(defun vertico--set-updated-selection(&rest _)
  "Used to re-trigger a candidate selection after a new input"
  (setq consult--vertico-updated-selection t))

(defun my/closest-candidates-index (target candidates predicate)
  "Return the index of the closest candidate to TARGET using PREDICATE to extract candidate position."
  (cl-loop for candidate in candidates
           for index from 0
           for candidate-pos = (funcall predicate candidate)
           do (when (> candidate-pos target)
                (cl-return (max 0 (- index 1))))
           finally return (- index 1)))

(defun vertico--update-selected-candidate-maybe (orig-fun &rest args)
  "Pick the nearest candidate rather than the first after updating candidates."
  (setq consult--vertico-updated-selection nil)
  (let ((result (apply orig-fun args)))

    (when (and consult--vertico-updated-selection
               consult--previous-point vertico--candidates
               (memq current-minibuffer-command
                     '(consult-org-heading consult-outline consult-line conf--consult-line)))
      (setq vertico--index
            (max 0
                 (or (my/closest-candidates-index
                      consult--previous-point
                      vertico--candidates
                      (pcase current-minibuffer-command
                        ((or 'consult-outline 'consult-line 'conf--consult-line)
                         (lambda (cand) (car (consult--get-location cand))))
                        ('consult-org-heading
                         (lambda (cand) (get-text-property 0 'consult--candidate cand)))
                        (_ (error (format "Unsupported command in vertico--update-selected-candidate-maybe: %s" current-minibuffer-command)))))
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


(advice-add #'consult-org-heading :before #'consult--set-previous-point)
(advice-add #'consult-outline :before #'consult--set-previous-point)
(advice-add #'consult-line :before #'consult--set-previous-point)
(advice-add #'vertico--update :around #'vertico--update-selected-candidate-maybe)
(advice-add #'vertico--recompute :after #'vertico--set-updated-selection)

(setq consult-line-start-from-top t)

(require 'vertico-multiform)
(vertico-multiform-mode +1)
(define-key vertico-multiform-map (kbd "M-R") nil)
(define-key vertico-multiform-map (kbd "M-B") nil)
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

(defun embark--rename-file-and-buffer (old-name)
  "Rename OLD-NAME to NEW-NAME, updating associated buffer if it exists."
  (let ((new-name (read-file-name (format "Rename '%s' to file: " (file-name-nondirectory old-name)) (file-name-directory old-name))))

    (when (not (file-directory-p (file-name-directory new-name)))
      (if (y-or-n-p (format "Create directory '%s'? "
                            (file-name-directory new-name)))
          (make-directory (file-name-directory new-name))
        (error "Cancelled")))

    (rename-file old-name new-name)

    (let ((buf (find-buffer-visiting old-name)))
      (when buf
        (with-current-buffer buf
          (set-visited-file-name new-name nil t)
          (rename-buffer (file-name-nondirectory new-name))
          (set-buffer-modified-p nil)
          (message "Renamed buffer associated with '%s' to '%s'" old-name new-name))))))

(define-key embark-file-map "r" #'embark--rename-file-and-buffer)
(add-to-list 'embark-post-action-hooks '(embark--rename-file-and-buffer embark--restart))

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

(define-key embark-identifier-map "o" #'xref-find-definitions-other-window)

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

(define-key embark-identifier-map "R" #'eglot-rename)
(push 'embark--allow-edit (alist-get 'eglot-rename embark-target-injection-hooks))

(defun xref-with-dumb-jump ()
  "Use xref with only dumb-jump backend."
  (interactive)
  (let ((xref-backend-functions '(dumb-jump-xref-activate))
        (xref-prompt-for-identifier nil))
    (call-interactively 'xref-find-definitions)))

(define-key embark-identifier-map "f" #'xref-with-dumb-jump)
(add-to-list 'embark-target-injection-hooks '(xref-with-dumb-jump embark--ignore-target))

(define-key embark-identifier-map "s" #'conf--consult-ripgrep)
(add-to-list 'embark-target-injection-hooks '(conf--consult-ripgrep embark--ignore-target))

;; (define-key grep-mode-map (kbd "TAB") 'compilation-display-error)
;; (define-key compilation-mode-map (kbd "TAB") 'compilation-display-error)

(define-key grep-mode-map (kbd "TAB") 'next-error-no-select)
(define-key grep-mode-map (kbd "S-<tab>") 'previous-error-no-select)
(define-key compilation-mode-map (kbd "TAB") 'next-error-no-select)
(define-key compilation-mode-map (kbd "S-<tab>") 'previous-error-no-select)

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
  (global-set-key (kbd "C-c /") nil)
  ;; (global-set-key (kbd "C-c _") nil)

  (setq meow-next-thing-include-syntax '((word "" "") (symbol "" "")))

  (setq meow-use-clipboard t)

  ;; Disable going to secondary selection when no region is active during pop
  (setq meow-selection-command-fallback
        (assoc-delete-all 'meow-pop-selection meow-selection-command-fallback))

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

  (defun conf--meow-change-and-mark ()
    (interactive)
    (when (region-active-p)
      (meow--push-search (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
    (if (meow-beacon-mode-p)
        (meow-beacon-change)
      (meow-change)))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("h" . meow-left)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("l" . meow-right)
     '("C-h" . left-word)
     '("C-j" . forward-paragraph)
     '("C-k" . backward-paragraph)
     '("C-l" . right-word)
     '("<escape>" . ignore)
     '("C-SPC" . (lambda () (interactive) (meow-left-expand) (meow-right-expand))))
    (meow-leader-define-key
     '("&" . meow-digit-argument)
     '("é" . meow-digit-argument)
     '("\"" . meow-digit-argument)
     '("'" . meow-digit-argument)
     '("(" . meow-digit-argument)
     '("-" . meow-digit-argument)
     '("è" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     '("o" . other-window)
     )
    (meow-normal-define-key
     '("à" . meow-expand-0)
     '("&" . meow-expand-1)
     '("é" . meow-expand-2)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     ;; '("\"" . meow-expand-3)
     ;; '("'" . meow-expand-4)
     ;; '("(" . meow-expand-5)
     ;; '("-" . meow-expand-6)
     ;; '("è" . meow-expand-7)
     ;; '("_" . meow-expand-8)
     ;; '("ç" . meow-expand-9)

     '("(" . insert-pair)
     '("\"" . insert-pair)
     '("'" . insert-pair)
     '("{" . insert-pair)
     '("[" . insert-pair)
     '("`" . insert-pair)

     '("!" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '(")" . meow-beginning-of-thing)
     '("=" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . conf--meow-change-and-mark)
     '("C" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . (lambda () (interactive) (let ((current-prefix-arg -1))
                                        (call-interactively 'meow-find))))
     '("g" . (lambda () (interactive) (if (> (mc/num-cursors) 1)
                                          (mc/keyboard-quit)
                                        (meow-cancel-selection))))
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . (lambda () (interactive) (if (derived-mode-p 'org-mode)
                                          (org-shiftleft)
                                        (meow-left-expand))))
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("C-h" . left-word)
     '("C-j" . (lambda () (interactive) (if (and corfu-mode completion-in-region-mode)
                                            (corfu-next)
                                          (forward-paragraph))))
     '("C-k" . (lambda () (interactive) (if (and corfu-mode completion-in-region-mode)
                                            (corfu-previous)
                                          (backward-paragraph))))
     '("C-S-J" . (lambda () (interactive) (meow-left-expand) (meow-right-expand) (forward-paragraph)))
     '("C-S-K" . (lambda () (interactive) (meow-left-expand) (meow-right-expand) (backward-paragraph)))
     '("C-l" . right-word)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("M-k" . (lambda () (interactive) (move-up 4)))
     '("M-j" . (lambda () (interactive) (move-down 4)))
     '("M-S-k" . (lambda () (interactive) (move-up 4)))
     '("M-S-j" . (lambda () (interactive) (move-down 4)))
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . (lambda () (interactive) (if (derived-mode-p 'org-mode)
                                          (org-shiftright)
                                        (meow-right-expand))))
     '("m" . meow-join)
     '("M" . (lambda () (interactive) (meow-join -1)))
     '("n" . meow-search)
     '("N" . meow-reverse)
     '("o" . ignore)
     '("O" . meow-block)
     '("p" . meow-yank)
     '("q" . nil)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)            ;meow-replace
     '("s" . meow-kill)
     '("t" . meow-till)
     '("T" . (lambda () (interactive) (let ((current-prefix-arg -1))
                                        (call-interactively 'meow-till))))
     '("u" . ignore)
     '("U" . meow-undo)
     '("M-U" . undo-fu-only-redo)
     '("v" . nil)                       ;meow-visit
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-change-save)
     '("z" . meow-pop-selection)
     '("<escape>" . ignore)
     '("C-SPC" . (lambda () (interactive) (meow-left-expand) (meow-right-expand))))
    (meow-define-keys
        'insert
      '("C-h" . meow-left)
      '("C-j" . meow-next)
      '("C-k" . meow-prev)
      '("C-l" . meow-right)
      ;; '("M-h" . meow-left)
      '("M-k" . (lambda () (interactive) (move-up 4)))
      '("M-j" . (lambda () (interactive) (move-down 4)))
      ;; '("M-l" . meow-right)
      ))
  (meow-setup)
  (add-hook 'git-commit-setup-hook 'meow-insert-mode)
  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (add-hook 'vterm-mode-hook 'meow-insert-mode)
  (add-hook 'meow-global-mode-hook (lambda () (setq delete-active-region t)))
  (add-to-list 'meow-mode-state-list '(Custom-mode . normal))
  (add-to-list 'meow-mode-state-list '(eww-mode . normal))
  (meow-global-mode))

(el-patch-feature meow)

(defun conf--treesit-string-bounds-at-point ()
  (when-let* ((node (treesit-node-at (point)))
              (parent (treesit-node-parent node))
              (is-string (member (treesit-node-type parent) '("string" "template_string"))))
    (cons (treesit-node-start parent)
          (treesit-node-end parent))))

;; Fix bound of string in tsx mode, uses treesitter instead of bounds-of-thing-at-point
(el-patch-defun meow--bounds-of-string-1 ()
  "Return the bounds of the string under the cursor.

The thing `string' is not available in Emacs 27.'"
  (if (version< emacs-version "28")
      (when (meow--in-string-p)
        (let (beg end)
          (save-mark-and-excursion
            (while (meow--in-string-p)
              (backward-char 1))
            (setq beg (point)))
          (save-mark-and-excursion
            (while (meow--in-string-p)
              (forward-char 1))
            (setq end (point)))
          (cons beg end)))
    (el-patch-swap (bounds-of-thing-at-point 'string)
                   (if (treesit-language-at (point))
                       (conf--treesit-string-bounds-at-point)
                     (bounds-of-thing-at-point 'string)))))

(el-patch-defun meow--bounds-of-string (&optional inner)
  (when-let* ((bounds (meow--bounds-of-string-1)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (cons
       (save-mark-and-excursion
         (goto-char beg)
         (el-patch-add
           (when (and inner (looking-at "f"))
             (forward-char)))
         (funcall (if inner #'skip-syntax-forward #'skip-syntax-backward) "\"|")
         (point))
       (save-mark-and-excursion
         (goto-char end)
         (funcall (if inner #'skip-syntax-backward #'skip-syntax-forward) "\"|")
         (point))))))

(meow-thing-register 'xml
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))
(add-to-list 'meow-char-thing-table '(?< . xml))

;; Disable meow-select-on-change when using multiple cursors
(defvar conf--meow-select-on-change-original nil
  "Store the original value of meow-select-on-change")

(defvar conf--meow-mc-enabled nil)

(defun conf--meow-handle-multiple-cursors ()
  "Adjust meow-select-on-change based on multiple-cursors-mode state."
  (if multiple-cursors-mode
      (progn
        (unless conf--meow-mc-enabled
          (setq conf--meow-select-on-change-original meow-select-on-change)
          (setq meow-select-on-change nil)
          (setq conf--meow-mc-enabled t)))
    (setq meow-select-on-change conf--meow-select-on-change-original)
    (setq conf--meow-mc-enabled nil)))

;; Add hook to handle multiple-cursors-mode changes
(add-hook 'multiple-cursors-mode-hook #'conf--meow-handle-multiple-cursors)

(use-package meow-vterm
  :if (not (eq system-type 'windows-nt))
  :straight (meow-vterm :type git :host github :repo "accelbread/meow-vterm")
  :bind ((:map meow-vterm-normal-mode-map
               ("M-t"       . multi-vterm)
               ("M-E"       . (lambda () (interactive)
                                (meow-insert)
                                (conf--vterm-insert-cd)))
               ("<M-right>" . vterm-toggle-forward)
               ("<M-left>"  . vterm-toggle-backward)
               ("M-l" . vterm-toggle-forward)
               ("M-h" . vterm-toggle-backward)))
  :init
  (meow-vterm-enable))

(use-package meow-tree-sitter
  :init
  (meow-tree-sitter-register-defaults))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode))
  :config

  ;; There is a conflict between web-mode and electric-pair-mode, disable one:
  ;; (add-hook 'web-mode-hook (lambda () (electric-pair-local-mode -1)))
  (setq web-mode-enable-auto-pairing nil))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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

;; (use-package recursion-indicator
;;   :config
;;   (recursion-indicator-mode))

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package tramp
  :straight (:type built-in)
  :config
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")
  (setq tramp-copy-size-limit (* 1024 1024)) ;; 1MB
  (setq tramp-verbose 3)
  (setq tramp-kubernetes-namespace nil)

  ;; see https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='/tmp/tramp.%%C' -o ControlPersist=3600")
  (tramp-set-completion-function
   "ssh" (append (tramp-get-completion-function "ssh") '((tramp-parse-sconfig "~/.ssh/config"))))
  (tramp-set-completion-function
   "scp" (append (tramp-get-completion-function "scp") '((tramp-parse-sconfig "~/.ssh/config"))))
  ;; see https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html#Improving-performance-of-asynchronous-remote-processes-1
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t))))

(defun conf--diff-and-save-buffer ()
  "View diff and optionally save the buffer."
  (interactive)
  (diff-buffer-with-file))

(setq save-some-buffers-action-alist (assq-delete-all ?d save-some-buffers-action-alist))
(push '(?d (lambda (buff) (with-current-buffer buff (conf--diff-and-save-buffer)) nil) "Show diff")
      save-some-buffers-action-alist)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  ;; (dirvish-peek-mode)
  )

(defun conf--jump-to-same-base-name (arg)
  "Jump to another file with same base name in current directory.
With universal argument ARG, open in another window."
  (interactive "P")
  (let* ((current-file (buffer-file-name))
         (base-name (car (split-string (file-name-nondirectory current-file) "\\.")))
         (dir (file-name-directory current-file))
         (files (remove current-file
                       (directory-files dir t (concat "^" (regexp-quote base-name) "\\."))))
         (find-func (if arg #'find-file-other-window #'find-file)))
    (unless files
      (setq files (remove current-file
                          (directory-files dir t "[^.].*"))))
    (cond
     ((null files)
      (message "No other files with same base name found"))
     ((= (length files) 1)
      (funcall find-func (car files)))
     (t
      (funcall find-func (completing-read "Select file: " files nil t))))))

(defun conf--consult-compile-error-in-compilation ()
  "Run consult-compile-error in the compilation buffer."
  (interactive)
  (if-let ((comp-buffer (get-buffer "*compilation*")))
      (with-current-buffer comp-buffer
        (consult-compile-error))
    (message "No compilation buffer found")))

(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p s") 'consult-ripgrep)
(global-set-key (kbd "C-c p k") 'project-compile)
(global-set-key (kbd "C-c p e") 'conf--consult-compile-error-in-compilation)
(global-set-key (kbd "C-c p a") 'conf--jump-to-same-base-name)

(global-set-key (kbd "C-c f") 'find-file)

(setq project-switch-commands '((project-find-file "Find file" "f")
                                (project-dired "Dired" "D")
                                (consult-ripgrep "ripgrep" "s")
                                (magit-project-status "Magit" "g")
                                (conf--vterm-toggle-project "Vterm" "e")
                                (project-compile "Compike" "k")))

;; (which-function-mode)

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
          (goto-char (window-start))
          (beginning-of-defun)
          (font-lock-ensure (point) (pos-eol))
          (buffer-substring (point) (pos-eol))))))

  (setf (alist-get nil topsy-mode-functions) 'conf--topsy--beginning-of-defun)
  (setf (alist-get 'emacs-lisp-mode topsy-mode-functions) 'conf--topsy--beginning-of-defun))

;; ;; checkout https://github.com/neeasade/stillness-mode.el or https://github.com/hkjels/mini-ontop.el instead
;; (use-package sinister
;;   :straight (:type built-in)
;;   :load-path "vendors")

(use-package crux
  :bind
  (("C-c C-u" . crux-upcase-region)
   ("C-c C-l" . crux-downcase-region)))

(use-package highlight-parentheses
  :hook
  (prog-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-colors '("firebrick1")))

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "q" #'gptel-quick)
  (add-to-list 'embark-post-action-hooks '(gptel-quick embark--unmark-target)))

(use-package cycle-quotes
  :bind
  (("C-c '" . cycle-quotes)))

(use-package aidermacs
  :if (not (eq system-type 'windows-nt))
  :bind
  (:map aidermacs-minor-mode
   ("C-c /" . insert-project-file-path))
  :config
  ;; (setq aidermacs-auto-commits t)
  ;; (setq aidermacs-show-diff-after-change nil)
  ;; (setq aidermacs-use-architect-mode t)
  (setq aidermacs-default-model "sonnet")
  (when-let ((anthropic-api-key (password-store-get "anthropic-api-key")))
    (setenv "ANTHROPIC_API_KEY" anthropic-api-key))
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (add-to-list 'aidermacs-language-name-map '("tsx" . "tsx-ts"))
  (add-to-list 'aidermacs-language-name-map '("typescript" . "tsx-ts")))

(defun insert-project-file-path ()
  "Select a file from current project and insert its relative path at point."
  (interactive)
  (let* ((project (project-current))
         (root (project-root project))
         (files (project-files project))
         (relative-files (mapcar (lambda (f) (file-relative-name f root)) files))
         (selected (completing-read-multiple "Insert file: " relative-files nil t)))
    (insert (string-join selected " "))))

(defun conf--aidermacs-run-advice (orig-fun &rest args)
  (let ((default-directory (project-root (project-current))))
    (apply orig-fun args)))

(advice-add 'aidermacs-run :around #'conf--aidermacs-run-advice)

(add-to-list 'project-switch-commands '(aidermacs-run "Aider" "a"))

;; Maybe we should do the opposite, a whitelist instead of a blacklist
(defvar conf--use-default-history '(extended-command-history))

(defun conf--vertico-exit-advice (&optional arg)
  "Change history to use input instead of selected candidate"
  (let ((input (minibuffer-contents-no-properties))
        (history-var minibuffer-history-variable))
    (run-with-timer 0 nil
                    (lambda () (interactive)
                      (when (and (boundp history-var)
                                 (not (equal (symbol-value history-var) t))
                                 (not (memq history-var conf--use-default-history)))
                        ;; (message "memory var %s" history-var)
                        (set history-var (cdr (symbol-value history-var)))
                        (add-to-history history-var input))))))

(advice-add #'vertico-exit :before 'conf--vertico-exit-advice)

(use-package ediff
  :straight (:type built-in)
  :config
  (setq ediff-split-window-function	'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  ;; (winner-mode)
  ;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  
  (defun conf--ediff-hook ()
    (ediff-setup-keymap)
    ;; (define-key ediff-mode-map "C-j" 'ediff-next-difference)
    ;; (define-key ediff-mode-map "C-k" 'ediff-previous-difference)
    (define-key ediff-mode-map (kbd "M-j")
                (lambda () (interactive) (ediff-scroll-vertically -4)))
     (define-key ediff-mode-map (kbd "M-k")
                 (lambda () (interactive) (ediff-scroll-vertically 4))))

  (defun conf--disable-y-or-n-p (orig-fun &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (apply orig-fun args)))
  (advice-add 'ediff-quit :around #'conf--disable-y-or-n-p)

  (add-hook 'ediff-mode-hook 'conf--ediff-hook))

(defun conf--restart ()
  (interactive)
  (call-process "sh" nil nil nil "-c"
                (concat "emacsclient -e \"(kill-emacs)\" && "
                        "emacsclient -c -n -a \"\" && "
                        "sleep 1 && "
                        "emacsclient -e \"(progn (switch-to-buffer \\\"*Messages*\\\") (select-frame-set-input-focus (selected-frame)))\" &")))

(use-package repeat-fu
  :commands (repeat-fu-mode repeat-fu-execute)
  :straight (repeat-fu :type git :host codeberg :repo "ideasman42/emacs-repeat-fu")

  :config
  (setq repeat-fu-preset 'meow)

  :hook
  ((meow-mode)
   .
   (lambda ()
     (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
       (repeat-fu-mode)
       (define-key meow-normal-state-keymap (kbd "M-'") 'repeat-fu-execute)
       (define-key meow-insert-state-keymap (kbd "M-'") 'repeat-fu-execute)))))

(use-package minuet
  :bind
  (("C-c , c" . #'minuet-complete-with-minibuffer)
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ("M-p" . #'minuet-previous-suggestion)
   ("M-n" . #'minuet-next-suggestion)
   ("M-A" . #'minuet-accept-suggestion))
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

(use-package casual
  :custom
  (casual-timezone-datestamp-format "%a %b %-e %Y, %H:%M"))

(use-package org-download
  :hook
  (org-mode . org-download-enable))

(use-package macher
  :straight (:host github :repo "kmontag/macher")

  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

  :config
  (with-eval-after-load 'gptel (macher-install)))

(use-package magit-prime
  :straight (:type git :host github :repo "Azkae/magit-prime")
  :config
  (magit-prime-mode))

(use-package ws-butler
  :straight (:type git :host github :repo "lewang/ws-butler")
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

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

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n f" . denote-open-or-create))
  :config
  (setq denote-directory (expand-file-name "~/Dropbox/denotes/"))
  (setq denote-known-keywords '("carbon" "emacs" "personal" "temporary"))

  (setq denote-prompts-with-history-as-completion
        (remove 'denote-title-prompt denote-prompts-with-history-as-completion)))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n s" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(defun conf--denote-dired ()
  (interactive)
  (dired denote-directory))

(require 'project-local)

(use-package project
  :straight (:type built-in)
  :config
  (add-hook 'project-find-functions 'conf--project-try-local 90))

(midnight-mode)

(use-package shell-maker)
(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell"))

(use-package verb
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

;; TODO: test direnv

;; load graphic settings
(require 'graphics)
(require 'custom-python-highlighting)
