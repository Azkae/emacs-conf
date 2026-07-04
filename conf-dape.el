;; -*- lexical-binding: t; -*-

(use-package dape
  :init
  (setq dape-key-prefix "\C-cd")
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  :config
  ;; Allow setting breakpoint with mouse
  (dape-breakpoint-global-mode)
  (set-face-attribute 'dape-exception-description-face nil :foreground "black")

  (add-hook 'dape-start-hook 'delete-other-windows)

  ;; customize dape repeat
  (defvar dape-global-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" #'dape-pause)
      (define-key map "c" #'dape-continue)
      (define-key map "n" #'dape-next)
      (define-key map "s" #'dape-step-in)
      (define-key map "o" #'dape-step-out)
      (define-key map "r" #'dape-restart)
      (define-key map "u" #'dape-until)
      (define-key map "b" #'dape-breakpoint-toggle)
      (define-key map "B" #'dape-breakpoint-remove-all)
      (define-key map "t" #'dape-select-thread)
      (define-key map "S" #'dape-select-stack)
      (define-key map ">" #'dape-stack-select-down)
      (define-key map "<" #'dape-stack-select-up)
      (define-key map "q" #'dape-quit)
      map))

  (dolist (cmd '(dape
                 dape-pause
                 dape-continue
                 dape-next
                 dape-step-in
                 dape-step-out
                 dape-restart
                 dape-restart-frame
                 dape-until
                 dape-breakpoint-log
                 dape-breakpoint-expression
                 dape-breakpoint-hits
                 dape-breakpoint-toggle
                 dape-breakpoint-remove-all
                 dape-stack-select-up
                 dape-stack-select-down
                 dape-select-stack
                 dape-select-thread
                 dape-watch-dwim
                 dape-evaluate-expression))
    (put cmd 'repeat-map 'dape-global-repeat-map))

  (add-to-list 'dape-configs
    '(debugpy-attach-port
       modes (python-mode python-ts-mode)
       port 8787
       :request "attach"
       :type "python"
       :justMyCode nil
       :showReturnValue t))

  (defun conf--dape-read-pid ()
    "Read pid of active processes if possible."
    (if-let* ((pids (list-system-processes)))
        (let ((collection
               (mapcar (lambda (pid)
                         (let ((args (alist-get 'args (process-attributes pid))))
                           (cons (concat
                                  (format "%d" pid)
                                  (when args
                                    (format ": %s" args)))
                                 pid)))
                       pids)))
          (alist-get (completing-read "Pid: " collection)
                     collection nil nil 'equal))
      (read-number "Pid: ")))

  (defun conf--dape-config-autopid (config)
    (if (eq (plist-get config :processId) :autopid)
      (let ((pid (conf--dape-read-pid)))
        (plist-put config :processId pid))
      config))

  (add-to-list 'dape-default-config-functions 'conf--dape-config-autopid)

  (add-to-list 'dape-configs
    '(debugpy-attach-pid
       modes (python-mode python-ts-mode)
       command "python3"
       command-args ("-m" "debugpy.adapter")
       :request "attach"
       :type "python"
       :processId :autopid
       :justMyCode nil
       :showReturnValue t)))

(provide 'conf-dape)
