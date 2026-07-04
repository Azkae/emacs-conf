;; -*- lexical-binding: t; -*-

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
  (if-let* ((comp-buffer (get-buffer "*compilation*")))
      (with-current-buffer comp-buffer
        (consult-compile-error))
    (message "No compilation buffer found")))

(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p s") 'consult-ripgrep)
(global-set-key (kbd "C-c p k") 'project-compile)
(global-set-key (kbd "C-c p e") 'conf--consult-compile-error-in-compilation)
(global-set-key (kbd "C-c p a") 'conf--jump-to-same-base-name)
(global-set-key (kbd "C-c p &") 'project-async-shell-command)

(global-set-key (kbd "C-c f") 'find-file)

(setq project-switch-commands '((project-find-file "Find file" "f")
                                (project-dired "Dired" "D")
                                (consult-ripgrep "ripgrep" "s")
                                (magit-project-status "Magit" "g")
                                (conf--vterm-toggle-project "Vterm" "e")
                                (project-compile "Compile" "k")
                                (project-async-shell-command "Command" "&")))

;; setup compile regexp for pyright
(require 'compile)

(add-to-list 'compilation-error-regexp-alist-alist
  '(pyright
    "^[[:space:]]*\\([^
:]+\\):\\([0-9]+\\):\\([0-9]+\\) - \\(?:\\(warning\\)\\|\\(?:error\\|information\\)\\): "
    1 2 3 (4)))

(add-to-list 'compilation-error-regexp-alist 'pyright)

(require 'grep)
(define-key grep-mode-map (kbd "TAB") 'next-error-no-select)
(define-key grep-mode-map (kbd "S-<tab>") 'previous-error-no-select)
(define-key compilation-mode-map (kbd "TAB") 'next-error-no-select)
(define-key compilation-mode-map (kbd "S-<tab>") 'previous-error-no-select)

(require 'transient)

(defvar my/command-last-project-root nil)

(defun my/command-run (command &optional comint)
  "Run COMMAND via `compile` in a dedicated *command* buffer."
  (let* ((compilation-buffer-name-function (lambda (_) "*command*"))
         (default-directory (or my/command-last-project-root default-directory)))
    (compile command comint)
    (select-window (get-buffer-window "*command*"))))

(transient-define-prefix my/alembic-menu ()
  "Alembic migration commands."
  ["Alembic"
   ("r" "revision --autogenerate -m"
    (lambda ()
      (interactive)
      (let ((msg (read-string "Migration message: ")))
        (if (string-blank-p msg)
            (user-error "Migration message must not be empty")
          (my/command-run (format "alembic revision --autogenerate -m %S" msg))))))
   ("u" "upgrade head"
    (lambda ()
      (interactive)
      (my/command-run "alembic upgrade head")))
   ("d" "downgrade -1"
    (lambda ()
      (interactive)
      (when (yes-or-no-p "Downgrade the database by 1 revision? ")
        (my/command-run "alembic downgrade -1"))))])

(transient-define-prefix my/uv-menu ()
  "uv menu commands"
  ["uv"
   ("s" "uv sync"
    (lambda ()
      (interactive)
      (my/command-run (format "uv sync"))))
   ("p" "uv lock --upgrade-package $1"
    (lambda ()
      (interactive)
      (let ((msg (read-string "Package: ")))
        (if (string-blank-p package)
            (user-error "Missing package")
          (my/command-run (format "uv lock --upgrade-package $S" package))))))])

(transient-define-prefix my/npm-menu ()
  "npm menu commands"
  ["npm"
   ("i" "npm install"
    (lambda ()
      (interactive)
      (my/command-run (format "npm install"))))
   ("b" "npm run build"
    (lambda ()
      (interactive)
      (my/command-run (format "npm run build"))))
   ("t" "npm run test"
    (lambda ()
      (interactive)
      (my/command-run (format "npm run test"))))])

(transient-define-prefix my/git-menu ()
  "git menu commands"
  ["git"
   ("p" ".git/hooks/pre-commit"
    (lambda ()
      (interactive)
      (my/command-run (format ".git/hooks/pre-commit"))))])

(transient-define-prefix my/command-menu ()
  "Main command menu."
  ["Tools"
   ("a" "alembic" my/alembic-menu)
   ("u" "uv" my/uv-menu)
   ("n" "npm" my/npm-menu)
   ("g" "git" my/git-menu)]
  [("²" "run command"
    (lambda ()
      (interactive)
      (let ((default-directory my/command-last-project-root))
        (call-interactively 'project-async-shell-command))))])

(defun my/command-menu-in-project-root ()
  (interactive)
  (setq my/command-last-project-root (project-root (project-current)))
  (my/command-menu))

(global-set-key (kbd "M-²") 'my/command-menu-in-project-root)
(add-to-list 'project-switch-commands '(my/command-menu-in-project-root "Commands" "²"))

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


(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :custom
  (fancy-compilation-override-colors nil))
(define-key compilation-mode-map (kbd "M-p") nil)

(add-to-list 'compilation-transform-file-match-alist '("\\`<string>\\'" nil))

(conf--disable-keys diff-mode-map '("M-p" "M-h" "M-j" "M-k" "M-l"))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(provide 'conf-compile)
