;; -*- lexical-binding: t; -*-

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
  ("DEL" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-send-backspace))))
  :config
  (define-key vterm-copy-mode-map [remap self-insert-command] #'(lambda() (interactive) (vterm-copy-mode -1)
                                                                  (vterm--self-insert)))
  (setq vterm-timer-delay 0.05)
  (setq vterm-ignore-cursor-change t)
  ;; (setq vterm-timer-delay 0.1)
  )

(defface conf--vterm-face
  '((t :family "Menlo" :height 130))
  "The basic fixed-pitch face."
  :group 'basic-faces)

(add-hook 'vterm-mode-hook
          (lambda ()
            (set
             (make-local-variable 'buffer-face-mode-face) 'conf--vterm-face)
            (setq-local nobreak-char-display nil)
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

(provide 'conf-vterm)
