(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-bookmark-names-plist
   '(:last-refile "org-refile-last-stored" :last-capture-marker
                  "org-capture-last-stored-marker"))
 '(safe-local-variable-values
   '((eval and (fboundp 'gptel-mode) (gptel-mode 1))
     (editorconfig-lisp-use-default-indent . t)
     (eval and buffer-file-name
           (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (checkdoc-allow-quoting-nil-and-t . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-line-number-wrapped ((t (:inherit consult-line-number-prefix))))
 '(flymake-note-echo-at-eol ((t (:inherit flymake-end-of-line-diagnostics-face :foreground "gray42"))))
 '(org-block ((t (:extend t :foreground "grey82"))))
 '(org-verbatim ((t (:inherit font-lock-constant-face))))
 '(which-func ((t (:foreground "tan1")))))
