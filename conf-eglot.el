;; -*- lexical-binding: t; -*-

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
  ((c-mode c++-mode c-ts-mode c++-ts-mode python-mode python-ts-mode) . (lambda () (setq-local eglot-ignored-server-capabilities '(:inlayHintProvider :semanticTokensProvider))))
  ((typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  (rust-mode . eglot-ensure)
  :custom
  (eglot-report-progress t)
  ;; help with perf:
  (eglot-events-buffer-size 0)
  (eglot-code-action-indicator "h")
  :config
  (fset #'jsonrpc--log-event #'ignore)

  ;; (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--completion-style=detailed")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--completion-style=detailed" "--header-insertion-decorators=0" "--header-insertion=never")))
  (add-to-list 'eglot-server-programs `(web-mode . ,(eglot-alternatives
                                                     '(("vscode-html-language-server" "--stdio")
                                                       ("html-languageserver" "--stdio")))))
  (add-to-list 'eglot-server-programs `(python-mode . ,(eglot-alternatives
                                                        '(("basedpyright-langserver" "--stdio")
                                                          ("pyright-langserver" "--stdio")))))

  (add-to-list 'eglot-ignored-server-capabilities :semanticTokensProvider)

  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-code-action-indications '(margin))
  (setq eglot-documentation-renderer 'gfm-view-mode)

  ;; Disable auto indent after '}' on cpp mode, may break a few things..
  ;; (remove-hook 'post-self-insert-hook 'eglot--post-self-insert-hook t)

  (add-to-list 'eglot-stay-out-of 'company-backends)
  ;; Enable flymake only on save:
  ;; This allows to trigger flymake only when the sever published diagnostics
  (defun conf--eglot-publishDiagnostics (server method &rest args)
    (when (eq method 'textDocument/publishDiagnostics)
      (let ((uri (plist-get args :uri)))
        (when-let* ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
          (with-current-buffer buffer
            (when (bound-and-true-p sideline-mode)
              (sideline--reset)
              (sideline-render-this)))))))

  (advice-add 'eglot-handle-notification :after #'conf--eglot-publishDiagnostics))


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
  :straight (:type built-in)
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
  (put 'flymake-goto-next-error 'repeat-map 'flymake-goto-error-repeat-map)
  (put 'flymake-goto-prev-error 'repeat-map 'flymake-goto-error-repeat-map)
  (defvar flymake-goto-error-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" #'flymake-goto-next-error)
      (define-key map "p" #'flymake-goto-prev-error)
      map)))

(defun conf--eldoc-box-at-point ()
  (interactive)
  (if (and eldoc-box--frame (frame-live-p eldoc-box--frame) (frame-visible-p eldoc-box--frame))
      (progn
        (eldoc-box-quit-frame)
        (switch-to-buffer-other-window (eldoc-doc-buffer)))
    (call-interactively 'eldoc-box-help-at-point)))

(use-package eldoc-box
  :bind
  (("M-§" . conf--eldoc-box-at-point))
  :custom
  (eldoc-idle-delay 0.2)
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))

(provide 'conf-eglot)
