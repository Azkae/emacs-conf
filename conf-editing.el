;; -*- lexical-binding: t; -*-

(electric-pair-mode)
(electric-indent-mode)

(add-to-list 'electric-pair-pairs '(?` . ?`))
(add-to-list 'insert-pair-alist '(?` ?`))


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

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  ((typescript-ts-mode tsx-ts-mode) . (lambda () (rainbow-delimiters-mode -1))))

(use-package apheleia
  :hook
  ((python-mode python-ts-mode) . apheleia-mode)
  ((c++-mode c++-ts-mode) . apheleia-mode)
  (terraform-mode . apheleia-mode)
  ((typescript-ts-mode tsx-ts-mode) . apheleia-mode))

(provide 'conf-editing)
