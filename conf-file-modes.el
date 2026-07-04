;; -*- lexical-binding: t; -*-

(use-package web-mode
  :mode (("\\.html\\'" . web-mode))
  :config

  ;; There is a conflict between web-mode and electric-pair-mode, disable one:
  ;; (add-hook 'web-mode-hook (lambda () (electric-pair-local-mode -1)))
  (setq web-mode-enable-auto-pairing nil))

(setq-default css-indent-offset 2)

(use-package js
  :straight (:type built-in)
  :hook
  (js-json-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package rust-mode
  :defer t)

(use-package yaml-mode
  :hook
  (yaml-mode . toggle-truncate-lines))

(use-package swift-mode
  :defer t)

(use-package dockerfile-mode)

(use-package dotenv-mode)

(use-package kotlin-mode
  :defer t
  :config
  (modify-syntax-entry ?< "(>" kotlin-mode-syntax-table)
  (modify-syntax-entry ?> ")<" kotlin-mode-syntax-table))

(use-package groovy-mode
  :defer t)

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 4)
  :config

  ;; Fix bound of string on tsx
  (defun tsx-string-bounds-at-point ()
    "Get bounds of string at point in TSX mode, handling string_fragment."
    (when (treesit-available-p)
      (let* ((node (treesit-node-at (point)))
             (node-type (treesit-node-type node)))
        (cond
         ;; Handle string_fragment - get the parent string node
         ((equal node-type "string_fragment")
          (let ((parent (treesit-node-parent node)))
            (when (and parent
                       (string-match-p "string\\|template_string"
                                       (treesit-node-type parent)))
              (cons (treesit-node-start parent) (treesit-node-end parent)))))
         ;; Handle direct string nodes
         ((string-match-p "string\\|template_string\\|jsx_text" node-type)
          (cons (treesit-node-start node) (treesit-node-end node)))
         ;; Fallback: try parent
         (t (let ((parent (treesit-node-parent node)))
              (when (and parent
                         (string-match-p "string\\|template_string"
                                         (treesit-node-type parent)))
                (cons (treesit-node-start parent) (treesit-node-end parent)))))))))

  (defun tsx-setup-string-bounds ()
    "Setup proper string bounds detection for TSX mode."
    (setq-local bounds-of-thing-at-point-provider-alist
                (cons '(string . tsx-string-bounds-at-point)
                      (assq-delete-all 'string bounds-of-thing-at-point-provider-alist))))

  ;; Add to tsx-ts-mode hook
  (add-hook 'tsx-ts-mode-hook #'tsx-setup-string-bounds)
  (add-to-list 'typescript-ts-mode--sexp-nodes "object_type")
  (add-to-list 'typescript-ts-mode--sexp-nodes "interface_body")
  (add-to-list 'typescript-ts-mode--sexp-nodes "class_body")
  (add-to-list 'typescript-ts-mode--sexp-nodes "formal_parameters")
  (add-to-list 'typescript-ts-mode--sexp-nodes "statement_block")
  (add-to-list 'typescript-ts-mode--sexp-nodes "object")

  (defun my/point-on-jsx-angle-bracket-p ()
    "Return non-nil if point is on a JSX < or just after a JSX >."
    (and (bound-and-true-p treesit-primary-parser)
         (eq (treesit-language-at (point)) 'tsx)
         (let* ((ch-after  (char-after (point)))
                (ch-before (char-before (point)))
                (pos (cond ((eql ch-after  ?<) (point))
                           ((eql ch-before ?>) (1- (point)))
                           (t nil))))
           (when pos
             (treesit-node-top-level
              (treesit-node-at pos 'tsx)
              (rx bos "jsx")
              t)))))

  (defun my/show-paren--disable-in-jsx (orig-fn)
    "Disable `show-paren-mode' highlighting when point is in a JSX block."
    (and (funcall orig-fn)
         (not (my/point-on-jsx-angle-bracket-p))))

  (advice-add 'show-paren--enabled-p :around #'my/show-paren--disable-in-jsx))

(with-eval-after-load 'tsx-ts-mode
  (modify-syntax-entry ?` "\"" tsx-ts-mode-syntax-table))

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . (lambda () (visual-line-mode -1))))

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

(use-package terraform-mode
  :defer t
  :hook
  (terraform-mode . eglot-ensure))

(use-package markdown-mode
  :bind
  (:map markdown-mode-map
        ("M-p" . nil)
        ("C-c C-u" . nil))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("tsx" . tsx-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("ts" . typescript-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("jsx" . tsx-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("html" . mhtml-mode))
  (add-to-list 'markdown-code-lang-modes '("json" . js-json-mode)))

(provide 'conf-file-modes)
