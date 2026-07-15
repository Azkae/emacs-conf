;; -*- lexical-binding: t; -*-

(defun conf--org-open-link-maybe()
  (interactive)
  (if (eq (car (org-element-context)) 'link)
      (call-interactively 'org-open-at-point)
    (call-interactively 'org-meta-return)))

(defun conf--org-meta-return-split()
  (interactive)
  (let ((org-insert-heading-respect-content nil))
    (call-interactively 'org-meta-return)))

(use-package org
  :straight (:type built-in)
  :bind
  (:map org-mode-map
        ("M-."          . org-open-at-point)
        ("M-<return>"   . conf--org-open-link-maybe)
        ("C-M-<return>" . conf--org-meta-return-split)
        ("M-<up>"       . (lambda () (interactive) (move-up 4)))
        ("M-<down>"     . (lambda () (interactive) (move-down 4)))
        ("M-H"          . org-shiftmetaleft)
        ("M-L"          . org-shiftmetaright)
        ("C-c /"        . nil)
        ("C-c C-u"      . nil)
        ("M-,"          . org-mark-ring-goto)
        ("M-h"          . nil)
        :map org-read-date-minibuffer-local-map
        ("M-h"          . org-calendar-backward-day)
        ("M-j"          . org-calendar-forward-week)
        ("M-k"          . org-calendar-backward-week)
        ("M-l"          . org-calendar-forward-day))
  :config
  (setq org-startup-folded t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-support-shift-select t)
  (setq org-special-ctrl-a/e t)
  (setq org-src-preserve-indentation t)
  (setq org-hide-emphasis-markers t)

  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (add-hook 'org-attach-after-change-hook #'(lambda (dir) (run-with-timer 0.1 nil 'org-display-inline-images)))

  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-link-file-path-type 'relative)
  ;; (setq org-tags-column -90)

  (defun conf--org-table-align-after-yank (&rest _args)
    "Align org table after yanking if point is in a table."
    (when (and (eq major-mode 'org-mode)
               (org-at-table-p))
      (org-table-align)))

  (advice-add 'yank :after #'conf--org-table-align-after-yank)
  (advice-add 'yank-pop :after #'conf--org-table-align-after-yank)

  ;; fold heading when task is done
  (defun my/org-fold-done-tasks (plist)
    "Fold heading when changed to DONE state."
    (when (and (eq (plist-get plist :type) 'todo-state-change)
               (member (plist-get plist :to) org-done-keywords))
      (save-excursion
        (org-back-to-heading t)
        (outline-hide-subtree))))

  (add-hook 'org-trigger-hook #'my/org-fold-done-tasks)

  (modify-syntax-entry ?= "." org-mode-syntax-table)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t) (python . t) (sql . t) (shell . t)))
  (setq org-babel-default-header-args:sql
        '((:engine . "postgresql")))
  (setq org-babel-default-header-args:python
        '((:results . "output")))
  (setq org-babel-default-header-args:elisp
        '((:lexical . t)))
  (add-to-list 'org-src-lang-modes '("json" . js-json))
  (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("jsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("typescript" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("hcl" . terraform)))

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
  (setq org-agenda-files (list org-directory "~/Dropbox/todo.org" "~/Dropbox/archive-todo.org"))
  (setq org-default-notes-file "~/Dropbox/todo.org")

  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file "")
           "* TODO %?\n%a"))))

(setq org-archive-location "archive-%s::")

(defun org-archive-all-done ()
  "Archive all DONE items in the current buffer."
  (interactive)
  (let ((done-positions '())
        (archived-count 0))
    ;; First pass: collect all DONE item positions
    (org-map-entries
     (lambda ()
       (push (point) done-positions))
     "TODO=\"DONE\"")
    ;; Second pass: archive from bottom to top (so positions stay valid)
    (dolist (pos (sort done-positions '>))
      (goto-char pos)
      (when (org-entry-is-done-p)  ; Double-check it's still DONE
        (org-archive-subtree)
        (setq archived-count (1+ archived-count))))
    (message "Archived %d DONE items" archived-count)))

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
  (setq denote-known-keywords '("work" "emacs" "personal" "temporary"))

  (setq denote-prompts-with-history-as-completion
        (remove 'denote-title-prompt denote-prompts-with-history-as-completion))

  (setq denote-directory-get-files-function
      (lambda ()
        (sort (denote-directory-get-files)
              (lambda (a b)
                (string> (file-name-nondirectory a)
                         (file-name-nondirectory b)))))))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c n s" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(defun conf--denote-dired ()
  (interactive)
  (dired denote-directory))

(use-package org-download
  :hook
  (org-mode . org-download-enable))

(use-package org-modern
  :config
  (global-org-modern-mode)
  (setq org-modern-star nil)
  (setq org-ellipsis "…")
  ;; (setq org-modern-fold-stars '(("▸" . "▾") ("▹" . "▿") ("▸" . "▾") ("▹" . "▿") ("▸" . "▾")))
  )

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (setq org-startup-indented t)
  (setq org-modern-hide-stars nil)
  (setq org-indent-indentation-per-level 1)
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  ;; (set-face-attribute 'org-modern-bracket-line nil :family "Menlo")
  )

(provide 'conf-org)
