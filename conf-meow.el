;; -*- lexical-binding: t; -*-

(use-package meow
  :config
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

  (defun conf--meow-set-mark-command (arg)
    (interactive "P")
    (cond
     ((and (eq last-command 'pop-global-mark)
	       (not arg))
      (setq this-command 'pop-global-mark)
      (pop-global-mark))
     ((not arg)
      (meow-left-expand)
      (meow-right-expand))
     ((equal arg '(4))
      (setq this-command 'pop-global-mark)
      (pop-global-mark))
     (t
      (consult-global-mark))))

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
     '("C-SPC" . conf--meow-set-mark-command))
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
     '("M-K" . (lambda () (interactive) (move-up 4)))
     '("M-J" . (lambda () (interactive) (move-down 4)))
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
     '("C-SPC" . conf--meow-set-mark-command))
    (meow-define-keys
        'insert
      '("C-h" . meow-left)
      '("C-j" . meow-next)
      '("C-k" . meow-prev)
      '("C-l" . meow-right)
      '("M-k" . (lambda () (interactive) (move-up 4)))
      '("M-j" . (lambda () (interactive) (move-down 4)))))
  (meow-setup)
  (add-hook 'git-commit-mode-hook 'meow-insert-mode)
  (add-hook 'meow-insert-exit-hook 'corfu-quit)
  (add-hook 'vterm-mode-hook 'meow-insert-mode)
  (add-hook 'meow-global-mode-hook (lambda () (setq delete-active-region t)))
  (add-to-list 'meow-mode-state-list '(Custom-mode . normal))
  (add-to-list 'meow-mode-state-list '(eww-mode . normal))
  (add-hook 'vundo-mode-hook (lambda () (meow-mode -1)))
  (meow-global-mode))

(el-patch-feature meow)

(el-patch-defun meow--bounds-of-string (&optional inner)
  (when-let* ((bounds (meow--bounds-of-string-1)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (cons
       (save-mark-and-excursion
         (goto-char beg)
         (el-patch-add
           (when (and inner (looking-at "f"))
             (forward-char))
           (when (and (not inner) (looking-back "f" 1))
             (backward-char)))
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

(setq meow-select-on-change nil)

;; ;; Disable meow-select-on-change when using multiple cursors
;; (defvar conf--meow-select-on-change-original nil
;;   "Store the original value of meow-select-on-change")

;; (defvar conf--meow-mc-enabled nil)

;; (defun conf--meow-handle-multiple-cursors ()
;;   "Adjust meow-select-on-change based on multiple-cursors-mode state."
;;   (if multiple-cursors-mode
;;       (progn
;;         (unless conf--meow-mc-enabled
;;           (setq conf--meow-select-on-change-original meow-select-on-change)
;;           (setq meow-select-on-change nil)
;;           (setq conf--meow-mc-enabled t)))
;;     (setq meow-select-on-change conf--meow-select-on-change-original)
;;     (setq conf--meow-mc-enabled nil)))

;; ;; Add hook to handle multiple-cursors-mode changes
;; (add-hook 'multiple-cursors-mode-hook #'conf--meow-handle-multiple-cursors)


(use-package meow-vterm
  :if (not (eq system-type 'windows-nt))
  :after vterm
  :straight (meow-vterm :type git :host github :repo "accelbread/meow-vterm")
  :bind ((:map meow-vterm-normal-mode-map
               ("M-t"       . multi-vterm)
               ("M-f"       . (lambda () (interactive) (vterm-copy-mode 1) (conf--consult-line)))
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

(provide 'conf-meow)
