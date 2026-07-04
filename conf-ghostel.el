;; -*- lexical-binding: t; -*-

(use-package ghostel
  :bind
  (:map ghostel-semi-char-mode-map
  ;; ("M-z" . 'ghostel-copy-mode)
  ;; ("<mouse-1>" . 'ghostel-copy-mode)
  ;; ("M-f" . (lambda () (interactive) (ghostel-copy-mode 1) (conf--consult-line)))
   ("M-a" . ghostel-clear))
  (:map ghostel-mode-map
  ;; ("M-z" . 'ghostel-copy-mode)
  ;; ("<mouse-1>" . 'ghostel-copy-mode)
  ;; ("M-f" . (lambda () (interactive) (ghostel-copy-mode 1) (conf--consult-line)))
   ("M-a" . ghostel-clear))
  ;; :map ghostel-copy-mode-map
  ;; ("M-z" . 'ghostel-copy-mode)
  ;; ("M-v" . (lambda () (interactive) (ghostel-copy-mode -1) (ghostel-yank)))
  ;; ("C-c C-c" . (lambda () (interactive) (ghostel-copy-mode -1) (ghostel--self-insert)))
  ;; ("DEL" . (lambda () (interactive) (ghostel-copy-mode -1) (ghostel-send-backspace))))
  ;; :map vterm-copy-mode-map
  ;; ("M-z" . 'vterm-copy-mode)
  ;; ("M-v" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-yank)))
  ;; ("C-c C-c" . (lambda () (interactive) (vterm-copy-mode -1) (vterm--self-insert)))
  ;; ("DEL" . (lambda () (interactive) (vterm-copy-mode -1) (vterm-send-backspace))))

  :config
  (setq ghostel-ignore-cursor-change t)
  (setopt ghostel-keymap-exceptions '("M-q" "C-q" "C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "M-z" "M-X" "M-O" "M-e" "M-E" "M-l" "M-h" "C-M-h" "C-M-l" "M-a" "M-:"))

  (defun ghostel-send-return ()
    (interactive)
    (ghostel-send-key "return"))

  (defvar meow-ghostel-normal-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'ghostel-send-return)
      map)
    "Keymap for ghostel in normal mode.")

  (defun meow-ghostel-insert-enter ()
    "Enable ghostel default binding in insert and set cursor."
    (use-local-map ghostel-semi-char-mode-map)
    ;; (ghostel-goto-char (point))
    )

  (defun meow-ghostel-insert-exit ()
    "Use regular bindings in normal mode."
    (use-local-map meow-ghostel-normal-mode-map))

  (defun meow-ghostel-setup ()
    "Configure insert mode for ghostel."
    (add-hook 'meow-insert-enter-hook #'meow-ghostel-insert-enter nil t)
    (add-hook 'meow-insert-exit-hook #'meow-ghostel-insert-exit nil t)
    (use-local-map meow-ghostel-normal-mode-map))

;;;###autoload
  (defun meow-ghostel-enable ()
    "Enable syncing ghostel keymap with current meow mode."
    ;; (setq ghostel-keymap-exceptions '("C-c"))
    (define-key ghostel-mode-map (kbd "C-c ESC") #'ghostel-send-escape)
    (dolist (c '((yank . ghostel-yank)
                 (xterm-paste . ghostel-xterm-paste)
                 (yank-pop . ghostel-yank-pop)
                 (mouse-yank-primary . ghostel-yank-primary)
                 (self-insert-command . ghostel--self-insert)
                 (beginning-of-defun . ghostel-previous-prompt)
                 (end-of-defun . ghostel-next-prompt)))
      (define-key meow-ghostel-normal-mode-map (vector 'remap (car c)) (cdr c)))
    (add-hook 'ghostel-mode-hook #'meow-ghostel-setup))

  ;; (defun conf--meow-ghostel-insert-enter ()
  ;;   "Enable vterm default binding in insert and set cursor."
  ;;   (ghostel-semi-char-mode)
  ;;   )

  ;; (defun conf--meow-ghostel-insert-exit ()
  ;;   "Use regular bindings in normal mode."
  ;;   (ghostel-line-mode))

  ;; (defun conf--meow-ghostel-setup ()
  ;;   "Configure insert mode for ghostel."
  ;;   (add-hook 'meow-insert-enter-hook #'conf--meow-ghostel-insert-enter nil t)
  ;;   (add-hook 'meow-insert-exit-hook #'conf--meow-ghostel-insert-exit nil t))

  ;; (add-hook 'ghostel-mode-hook #'conf--meow-ghostel-setup)
  )

(defun conf--ghostel-setup-font()
  (set (make-local-variable 'buffer-face-mode-face) 'conf--vterm-face)
  (setq-local nobreak-char-display nil)
  (buffer-face-mode t))

(add-hook 'ghostel-mode-hook 'conf--ghostel-setup-font)

;; See graphics.el
;; (set-fontset-font t 'symbol "Monaspace Neon Frozen" nil 'prepend)
;; (set-fontset-font t ?✳ "Menlo" nil 'prepend)
;; (set-fontset-font t ?✻ "Menlo" nil 'prepend)
;; (set-fontset-font t ?⏺ "Monaspace Neon Frozen" nil 'prepend)
;; (set-fontset-font t ?⏺ "Monaspace Neon Frozen" nil 'prepend)

;; (setq face-font-rescale-alist '(("-cdac$" . 1.3) ("Monaspace Neon Frozen" . 0.9)
;;                                 ("Everson Mono" . 0.6)))

;; (add-to-list 'face-font-rescale-alist '("Monaspace Neon Frozen" . 0.9) t)
;; (add-to-list 'face-font-rescale-alist '("Everson Mono" . 0.6) t)

(provide 'conf-ghostel)
