;; -*- lexical-binding: t; -*-

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

(global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-in)
(global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-out)

(setq save-interprogram-paste-before-kill t)

(cl-defun remove-from-list (list-var element &key key test)
  "Remove ELEMENT from the value of LIST-VAR if present.

This can be used as an inverse of `add-to-list'."
  (unless key (setq key #'identity))
  (unless test (setq test #'equal))
  (setf (symbol-value list-var)
        (cl-remove element
                   (symbol-value list-var)
                   :key key
                   :test test)))


(use-package project
  :straight (:type built-in))

;; Better comint settings
(use-package comint
  :straight (:type built-in)
  :config
  (setq comint-output-filter-functions (remove 'comint-postoutput-scroll-to-bottom comint-output-filter-functions))
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-show-maximum-output nil)
  (setq-default comint-prompt-read-only t)
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
(global-set-key (kbd "C-q") 'kill-current-buffer)
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
  (scroll-right 10)
  (unless (minibufferp)
    (move-to-column (+ (window-hscroll) (/ (window-width) 2)))))

(defun conf--scroll-right ()
  (interactive)
  (scroll-left 10)
  (unless (minibufferp)
    (move-to-column (+ (window-hscroll) (/ (window-width) 2)))))

(global-set-key (kbd "M-x") 'kill-region-maybe)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-N") 'goto-line)
(global-set-key (kbd "M-a") 'recenter-top-bottom)
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
(global-set-key (kbd "M-£") 'shell-command-on-region)

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

(global-set-key (kbd "M-q n")  'next-error)
(global-set-key (kbd "M-q p")  'previous-error)
(global-set-key (kbd "M-q M-n")  'next-error)
(global-set-key (kbd "M-q M-p")  'previous-error)

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

(defun kill-current-buffer-avoid-boring ()
  (interactive)
  (kill-current-buffer)
  (when (not (is-buffer-valid (buffer-name)))
    (conf--skip-temp-buffers 'previous-buffer)))

(global-set-key [remap next-buffer] 'conf--next-buffer)
(global-set-key [remap previous-buffer] 'conf--prev-buffer)
(global-set-key [remap kill-current-buffer] 'kill-current-buffer-avoid-boring)

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

(require 'gnus)
(setq gnus-select-method '(nntp "news.gmane.io"))

(define-key gnus-summary-mode-map (kbd "M-k") nil)
(define-key gnus-summary-mode-map (kbd "M-r") nil)

(when (executable-find "gman")
  (setq manual-program "gman"))

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

(defun conf--diff-and-save-buffer ()
  "View diff and optionally save the buffer."
  (interactive)
  (diff-buffer-with-file))

(setq save-some-buffers-action-alist (assq-delete-all ?d save-some-buffers-action-alist))
(push '(?d (lambda (buff) (with-current-buffer buff (conf--diff-and-save-buffer)) nil) "Show diff")
      save-some-buffers-action-alist)

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

(add-hook 'text-mode-hook 'visual-line-mode)
(setopt treesit-enabled-modes t)


(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)


;; Poetry/UV project tracking

(require 'windmove)
(winner-mode +1)

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


(provide 'conf-better-default)
