;; -*- lexical-binding: t; -*-

(require 'helm)
(require 'helm-grep)
(require 'cl)

(defvar ya-helm-ag-last-cmd-line nil)
(defvar ya-helm-ag-history nil)

(defcustom ya-helm-ag-command
  "ag --line-numbers -S --color --nogroup"
  "Command to use"
  :group 'ya-helm-ag
  :type 'string)

(defun ya-helm-ag-prepare-cmd-line (pattern targets)
  "Prepare AG command line to search PATTERN in TARGETS."
  (let* ((base-patterns (split-string pattern))
         (patterns (remove-if (lambda (x) (string-prefix-p "-" x)) base-patterns))
         (options (remove-if-not (lambda (x) (string-prefix-p "-" x)) base-patterns))
         (option (mapconcat 'shell-quote-argument options " "))
         (pipe-cmd (format "ag -S --color %s" option))
         (cmd (format "%s %s %s %s"
                      ya-helm-ag-command
                      option
                      (shell-quote-argument (car patterns))
                      (mapconcat 'shell-quote-argument targets " "))))
    (helm-aif (cdr patterns)
        (concat cmd (cl-loop for p in it concat
                             (format " | %s %s"
                                     pipe-cmd (shell-quote-argument p))))
      cmd)))

(defun ya-helm-ag-init (targets)
  "Start AG process in TARGETS."
  (let ((default-directory (or helm-ff-default-directory
                               (helm-default-directory)
                               default-directory))
        (cmd-line (ya-helm-ag-prepare-cmd-line helm-pattern targets))
        (start-time (float-time))
        (process-name "ag"))
    (set (make-local-variable 'ya-helm-ag-last-cmd-line) cmd-line)
    (helm-log "Starting %s process" process-name)
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " cmd-line "\n\n"))
    (prog1
        (start-file-process-shell-command
         process-name helm-buffer cmd-line)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err      (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (insert (concat "* Exit with code 1, no result found,"
                                    " command line was:\n\n "
                                    (propertize ya-helm-ag-last-cmd-line
                                                'face 'helm-grep-cmd-line)))
                    (setq mode-line-format
                          `(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished - (no results)] "
                                     ,(upcase process-name))
                                    'face 'helm-grep-finish))))))
                 ((string= event "finished\n")
                  (helm-log "%s process finished with %s results in %fs"
                              process-name
                              (helm-get-candidate-number)
                              (- (float-time) start-time))
                  (helm-maybe-show-help-echo)
                  (with-helm-window
                    (setq mode-line-format
                          `(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format
                                     "[%s process finished in %.2fs - (%s results)] "
                                     ,(upcase process-name)
                                     ,(- (float-time) start-time)
                                     (helm-get-candidate-number))
                                    'face 'helm-grep-finish))))
                    (force-mode-line-update)
                    (when helm-allow-mouse
                      (helm--bind-mouse-for-selection helm-selection-point))))
                 (t (helm-log
                     "Error: %s %s"
                     process-name
                     (replace-regexp-in-string "\n" "" event))))))))))

(defclass ya-helm-ag-class (helm-source-async)
  ((nohighlight :initform t)
   (pcre :initarg :pcre :initform t
         :documentation
         "  Backend is using pcre regexp engine when non--nil.")
   (keymap :initform helm-grep-map)
   (history :initform 'ya-helm-ag-history)
   (help-message :initform 'helm-grep-help-message)
   (filter-one-by-one :initform 'helm-grep-filter-one-by-one)
   (persistent-action :initform 'helm-grep-persistent-action)
   (persistent-help :initform "Jump to line (`C-u' Record in mark ring)")
   (candidate-number-limit :initform 99999)
   (requires-pattern :initform 2)
   (nomark :initform t)
   (action :initform 'helm-grep-actions)
   (group :initform 'helm-grep)))

(defvar ya-helm-ag-source nil)

(defmethod helm--setup-source ((source ya-helm-ag-class))
  (call-next-method)
  (helm-aif (and helm-follow-mode-persistent
                 ya-helm-ag-source
                 (assoc-default 'follow ya-helm-ag-source))
      (setf (slot-value source 'follow) it)))

(defun ya-helm-ag (targets)
  "Start helm ag in DIRECTORY."
  (setq ya-helm-ag-source
        (helm-make-source "AG" 'ya-helm-ag-class
          :header-name (lambda (name)
                         (format "%s [%s]"
                                 name (mapconcat 'abbreviate-file-name targets " ")))
          :candidates-process
          (lambda () (ya-helm-ag-init targets))))
  (helm :sources 'ya-helm-ag-source
        :keymap helm-grep-map
        :history 'ya-helm-ag-history
        :truncate-lines t
        :buffer "*helm ag*"))

;;;###autoload
(defun ya-helm-do-ag ()
  (interactive)
  (ya-helm-ag (mapcar (lambda (x) (if (not (file-directory-p x))
                                      (file-name-directory x)
                                    x))
                      (helm-read-file-name
                       "Search in file(s): "
                       :default default-directory
                       :marked-candidates t :must-match t))))

;;;###autoload
(defun ya-helm-do-ag-buffers ()
  (interactive)
  (let ((bufs (cl-loop for buf in (buffer-list)
                       when (buffer-file-name buf)
                       collect it)))
    (ya-helm-ag bufs)))

(provide 'ya-helm-ag)
