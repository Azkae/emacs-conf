;; -*- lexical-binding: t; -*-

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x v l" . magit-log-buffer-file)
   ("C-x v f" . magit-find-file)
   ("C-x v s" . magit-show-commit)
   ("C-x v g" . magit-blame-addition)
   ("C-c G"   . magit-dispatch)
   :map magit-status-mode-map
   ("M-p" . nil)
   :map magit-diff-mode-map
   ("M-p" . nil)
   :map magit-hunk-section-map
   ("C-j" . nil))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-visit-avoid-head-blob t)
  (magit-auto-revert-immediately t)
  (vc-display-status nil)
  (magit-diff-visit-prefer-worktree t))

(defun my/kill-worktree-buffers (worktree)
  "Kill all buffers visiting files under WORKTREE directory."
  (let ((worktree-dir (file-name-as-directory (expand-file-name worktree))))
    (dolist (buf (buffer-list))
      (when (string-prefix-p worktree-dir
                             (expand-file-name
                              (with-current-buffer buf default-directory)))
        (kill-buffer buf)))))

(define-advice magit-worktree-delete (:around (orig worktree) kill-buffers)
  "Kill worktree buffers only if deletion is confirmed and succeeds."
  (let ((result (funcall orig worktree)))
    (my/kill-worktree-buffers worktree)
    result))

;; This git is faster got some reason
(let ((git-path "/Applications/Xcode.app/Contents/Developer/usr/bin/git"))
  (when (file-exists-p git-path)
    (setq magit-git-executable git-path)))

(defun my/forget-project-after-worktree-delete (worktree)
  "Remove WORKTREE from project.el's known projects after deletion."
  (when (require 'project nil t)
    (project-forget-project (expand-file-name worktree))))

(advice-add 'magit-worktree-delete :after #'my/forget-project-after-worktree-delete)

(setq smerge-command-prefix "\C-cv")
(setq smerge-refine-shadow-cursor nil)

(use-package request)

(defun conf--create-pull-request-github (repo branch)
  "Create a new PR on Github."
  (browse-url
   (format "https://github.com/%s/pull/new/%s" repo branch)))

(defun conf--show-pull-request-github (repo number)
  "Visit the current branch's PR on Github."
  (browse-url
   (format "https://github.com/%s/pull/%s" repo number)))

(defun conf--get-current-repo ()
  (replace-regexp-in-string
   "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
   (magit-get "remote"
              (magit-get-push-remote)
              "url")))

(defun conf--visit-circle-ci ()
  (interactive)
  (let ((repo (conf--get-current-repo))
        (branch (magit-get-current-branch)))
    (browse-url (format "https://app.circleci.com/pipelines/github/%s?branch=%s" repo branch))))

(setq github-token (password-store-get "github-token"))

(defun conf--visit-pull-request-url-github ()
  (interactive)
  (lexical-let ((repo (conf--get-current-repo))
         (branch (magit-get-current-branch))
         (commit (magit-rev-parse
                  (and magit-copy-revision-abbreviated "--short")
                  "HEAD")))
    (request (format "https://api.github.com/repos/%s/commits/%s/pulls"
                     repo commit)
      :headers `(("Authorization" . ,(concat "Bearer " github-token)))
      :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "Got error: %S" error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (if (not (equal (length data) 0))
                      (let* ((id (alist-get 'number (aref data 0))))
                        (conf--show-pull-request-github repo id))
                    (conf--create-pull-request-github repo branch)))))))

(defun conf--visit-pull-request-url-gitlab ()
  "Visit the current branch's PR on Gitlab."
  (interactive)
  (browse-url
   (format "https://%s/%s/-/merge_requests/new?merge_request%%5Bsource_branch%%5D=%s"
           (replace-regexp-in-string
            "\\`.+@\\(.+\\):.+\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (replace-regexp-in-string
            "\\`.+:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (url-hexify-string (magit-get-current-branch)))))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c d") #'conf--visit-circle-ci)
  (define-key magit-mode-map (kbd "C-c p") #'conf--visit-pull-request-url-github))

(use-package magit-delta
  :straight (:fork (:host github :repo "Azkae/magit-delta" :branch "fix-magit-log-buffer-file"))
  :if (executable-find "delta")
  :bind
  :custom
  (magit-delta-default-dark-theme "Monokai Extended")
  (magit-delta-default-light-theme "Github")
  (magit-delta-hide-plus-minus-markers nil)
  (magit-delta-max-size 200000)
  :config
  (add-hook 'magit-mode-hook #'(lambda () (magit-delta-mode +1))))

(defun smerge-next-safe ()
  (condition-case err
      (not (smerge-next))
    ('error
     nil)))

(defun goto-next-conflict ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (not (smerge-next-safe))
      (vc-find-conflicted-file)
      (if (eq buffer (current-buffer))
          (message "No conflicts found")
        (goto-char 0)
        (smerge-next-safe)))))

(global-set-key (kbd "M-q e") 'goto-next-conflict)

(defun start-process@use-pipe (fn &rest args)
    ;; checkdoc-params: (fn args)
    "Advice to ensure that `start-process' uses a pipe rather than
a pty for the compilation command. This increases performance on OSX
by a factor of 10, as the default pty size is a pitiful 1024 bytes."
    (let ((process-connection-type nil))
      (apply fn args)))
(setq magit-process-connection-type nil)

(use-package magit-prime
  :straight (:type git :host github :repo "Azkae/magit-prime")
  :config
  (magit-prime-mode))

(provide 'conf-magit)
