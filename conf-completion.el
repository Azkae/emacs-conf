;; -*- lexical-binding: t; -*-

(defun conf--vertico-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

;; Enable vertico
(use-package vertico
  :custom
  (vertico-count 25)
  (vertico-scroll-margin 5)
  (vertico-resize nil)
  (projectile-completion-system 'default)
  :bind
  ("C-z" . vertico-suspend)
  ("M-b" . vertico-repeat)
  ("M-B" . vertico-repeat-select)
  (:map vertico-map
        ;; ("M-r" . nil)
        ("M-<up>" . (lambda() (interactive) (scroll-other-window-down 5)))
        ("M-<down>" . (lambda() (interactive) (scroll-other-window 5)))
        ("M-k" . (lambda() (interactive) (scroll-other-window-down 5)))
        ("M-j" . (lambda() (interactive) (scroll-other-window 5)))
        ("C-SPC" . (lambda () (interactive) (call-interactively 'embark-select) (vertico-next)))
        ("C-j" . next-line)
        ("C-k" . previous-line)
        ("C-<return>" . vertico-exit)
        ("C-h" . left-char)
        ("C-l" . right-char)
        ("C-d" . conf--vertico-preview))
        ("M-t" . vertico-buffer-mode)
        ("M-RET" . vertico-exit)
        (:map minibuffer-local-map
              ("C-h" . left-char)
              ("C-l" . right-char)
              ("C-p" . previous-history-element)
              ("C-n" . next-history-element))
  :init
  (vertico-mode)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (defun conf--minibuffer-complete-or-insert-directory ()
    (interactive)
    (if (or (eq (vertico--metadata-get 'category) 'file) crm-completion-table)
        (vertico-insert)
      (minibuffer-complete)))

  (keymap-set vertico-map "TAB" #'conf--minibuffer-complete-or-insert-directory)

  (require 'vertico-multiform)
  (vertico-multiform-mode +1)
  (define-key vertico-multiform-map (kbd "M-R") nil)
  (define-key vertico-multiform-map (kbd "M-B") nil)
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  ;; (defun +vertico-highlight-unsaved-buffer (buffer-name)
  ;;   (let ((buffer (get-buffer buffer-name)))
  ;;     (if (and buffer
  ;;                (buffer-live-p buffer)
  ;;                (buffer-modified-p buffer))
  ;;         (propertize buffer-name 'face 'font-lock-constant-face)
  ;;       buffer-name)))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
      file))

  ;; function to highlight enabled modes similar to counsel-M-x
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd)))

  (add-to-list 'vertico-multiform-categories
               '(file
                 ;; (vertico-sort-function . sort-directories-first)
                 (+vertico-transform-functions . +vertico-highlight-directory)))

  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 ;; reverse
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

  (add-to-list 'vertico-multiform-commands
               '(denote-open-or-create (vertico-sort-function . nil)))

  (require 'vertico-sort)
  (vertico-sort--define (reverse-alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string> string>)

  (defun conf--vertico-sort-by-mtime (files)
    "Sort FILES by modification time, most recent first."
    (let ((cache (make-hash-table :test #'equal)))
      (cl-flet ((mtime (f)
                  (or (gethash f cache)
                      (puthash f
                               (file-attribute-modification-time
                                (file-attributes
                                 (concat vertico--base f)))
                               cache))))
        (sort files (lambda (a b) (time-less-p (mtime b) (mtime a)))))))

  (defun conf--vertico-toggle-sort ()
    (interactive)
    (setq-local vertico-sort-override-function
                (pcase vertico-sort-override-function
                  ('conf--vertico-sort-by-mtime (message "Sorting by reverse alpha") 'vertico-sort-reverse-alpha)
                  ('vertico-sort-reverse-alpha (message "Reverting to default sort") nil)
                  (_ (message "Sorting by time") 'conf--vertico-sort-by-mtime))
                vertico--input t))

  (keymap-set vertico-map "M-S" #'conf--vertico-toggle-sort))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :bind
  (("C-c t" . eglot-code-actions))
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(defun conf--consult-ripgrep ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

;; TODO: this could be replaced by just rebinding C-g to `abort-minibuffers`
(defun conf--deactivate-region-before-quit ()
  "Deactivate region before keyboard quit if delete-selection-mode is active."
  (when (and delete-selection-mode (region-active-p))
    (deactivate-mark)))

(advice-add 'minibuffer-keyboard-quit :before #'conf--deactivate-region-before-quit)

(defun conf--minibuffer-candidate ()
  (if (and (bound-and-true-p vertico--input) (minibufferp))
      (cons (vertico--metadata-get 'category) (vertico--candidate))
    nil))

(defun conf--minibuffer-selected-directory-maybe ()
  (let ((minibuffer-candidate (conf--minibuffer-candidate)))
    (when minibuffer-candidate
      (pcase (car minibuffer-candidate)
        ('project-file (project-root
                        (project-current nil (expand-file-name (cdr minibuffer-candidate)))))
        ('file (let ((path (expand-file-name (substitute-in-file-name (cdr minibuffer-candidate)))))
                 (if (not (file-directory-p path))
                     (file-name-directory path)
                   path)))))))

(defun conf--exit-minibuffer-and-execute (func)
  (run-with-timer 0 nil func)
  (abort-recursive-edit))

(defun conf--select-directory-and-ripgrep ()
  (interactive)
  (let ((path (or (conf--minibuffer-selected-directory-maybe)
                  (read-directory-name "Select directory: "))))
    (conf--exit-minibuffer-and-execute (lambda () (let ((this-command 'consult-ripgrep))
                                                    (consult-ripgrep path))))))

(defun conf--magit-in-selected-directory ()
  (interactive)
  (if-let* ((selected-path (conf--minibuffer-selected-directory-maybe)))
      (conf--exit-minibuffer-and-execute
       (lambda () (let ((default-directory selected-path))
                    (magit))))
    (conf--exit-minibuffer-and-execute (lambda () (magit)))))

(defun conf--vterm-in-selected-directory ()
  (interactive)
  (if-let* ((selected-path (conf--minibuffer-selected-directory-maybe)))
      (conf--exit-minibuffer-and-execute
       (lambda () (let ((default-directory selected-path))
                    (conf--vterm-toggle-insert-cd))))
    (conf--exit-minibuffer-and-execute (lambda () (conf--vterm-toggle)))))

(defun conf--debug-category()
  (interactive)
  (message (format "Current category: %s" (vertico--metadata-get 'category))))

(defun conf--find-in-open-buffers ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'consult-line-multi)))

(defun delete-until-slash ()
  (interactive)
  (let ((start-pos (if (minibufferp)
                       (minibuffer-prompt-end)
                     (point-min))))
    ;; Only delete the character before if it's past the start position
    (when (and (memq (char-before) '(?/ ?:))
               (> (point) start-pos))
      (delete-char -1))
    ;; Search backwards for separator, but don't go past the start position
    (if (re-search-backward "[:/]" start-pos t)
        ;; Found separator: delete everything after it and reinsert the separator
        (let ((separator (buffer-substring (point) (1+ (point)))))
          (delete-region (point) (point-max))
          (insert separator))
      ;; No separator found: delete everything until start position
      (delete-region start-pos (point-max)))))

(defvar conf--delete-slash-blacklisted-commands '(denote-open-or-create))

(defun delete-until-slash-maybe ()
  (interactive)
  (if (and (eq 'file (vertico--metadata-get 'category))
           (not (memq embark--command conf--delete-slash-blacklisted-commands)))
      (delete-until-slash)
    (conf--backward-delete-word)))

(use-package consult
  :bind
  (
   ("M-f"         . conf--consult-line)
   ("M-F"         . conf--find-in-open-buffers)
   ("C-x b"       . consult-buffer)
   ("M-R"         . conf--select-directory-and-ripgrep)
   ("M-X"         . execute-extended-command)
   ("M-o"         . find-file)
   ("M-O"         . consult-buffer)
   ("C-x r r"     . consult-bookmark)
   ("C-x r s"     . bookmark-set)
   :map prog-mode-map
   ;; ("M-."         . conf--consult-ripgrep)
   :map minibuffer-local-map
   ([M-backspace] . delete-until-slash-maybe)
   ("C-x g" . conf--magit-in-selected-directory)
   ("M-e" . conf--vterm-in-selected-directory)
   )
  :custom
  (consult-async-input-debounce 0.1)
  (consult-async-input-throttle 0.3)
  (consult-line-start-from-top 't)
  (consult-narrow-key "<")
  :config
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)

  (add-to-list 'consult-preview-excluded-files "\\.gpg\\'")
  (require 'consult-compile))

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult-source-bookmark consult-source-file-register
 consult-source-recent-file consult-source-project-recent-file
 conf--consult-ripgrep conf--select-directory-and-ripgrep
 project-switch-project conf--find-in-open-buffers consult-line-multi
 :preview-key '(:debounce 0.1 any)) ;; Option 1: Delay preview

;; (consult-customize
;;  find-file
;;  :preview-key '(:debounce 0.01 any))


(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Idea: We could add a wrapper to embark-act that deactivate the region if
;; the selection was made via meow-next-word (not symbol?) and maybe
;; don't deactivate region with universal prefix
(use-package embark
  :bind
  (("M-/" . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-help-key "?")
  ;; (setq embark-target-finders (delete 'embark-target-active-region embark-target-finders))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-default-action-overrides '(file . find-file))
  (define-key embark-region-map "h" nil))
(require 'embark-consult)

(defvar consult--previous-point nil
  "Location of point before entering minibuffer.
    Used to preselect nearest headings and imenu items.")

(defvar consult--vertico-updated-selection nil
  "Used to see if vertico recomputed candidates.")

(defun consult--set-previous-point (&optional arg1 arg2)
  "Save location of point. Used before entering the minibuffer."
  (setq consult--previous-point (point)))

(defun vertico--set-updated-selection(&rest _)
  "Used to re-trigger a candidate selection after a new input"
  (setq consult--vertico-updated-selection t))

(defun my/closest-candidates-index (target candidates predicate)
  "Return the index of the closest candidate to TARGET using PREDICATE to extract candidate position."
  (cl-loop for candidate in candidates
           for index from 0
           for candidate-pos = (funcall predicate candidate)
           do (when (> candidate-pos target)
                (cl-return (max 0 (- index 1))))
           finally return (- index 1)))

(defun vertico--update-selected-candidate-maybe (orig-fun &rest args)
  "Pick the nearest candidate rather than the first after updating candidates."
  (setq consult--vertico-updated-selection nil)
  (let ((result (apply orig-fun args)))

    (when (and consult--vertico-updated-selection
               consult--previous-point vertico--candidates
               (memq current-minibuffer-command
                     '(consult-org-heading consult-outline consult-line conf--consult-line)))
      (setq vertico--index
            (max 0
                 (or (my/closest-candidates-index
                      consult--previous-point
                      vertico--candidates
                      (pcase current-minibuffer-command
                        ((or 'consult-outline 'consult-line 'conf--consult-line)
                         (lambda (cand) (car (consult--get-location cand))))
                        ('consult-org-heading
                         (lambda (cand) (get-text-property 0 'consult--candidate cand)))
                        (_ (error (format "Unsupported command in vertico--update-selected-candidate-maybe: %s" current-minibuffer-command)))))
                     (length vertico--candidates)))))

    result))

(defun conf--consult-line (&optional initial start)
  "Call `consult-line` with candidates filtered by the symbol at point by default."
  (interactive)
  (run-with-timer 0 nil (lambda () (interactive)
                          (when (minibufferp)
                            (goto-char (minibuffer-prompt-end))
                            (push-mark (point-max) nil t))))
  (consult-line (thing-at-point 'symbol t)))


(advice-add #'consult-org-heading :before #'consult--set-previous-point)
(advice-add #'consult-outline :before #'consult--set-previous-point)
(advice-add #'consult-line :before #'consult--set-previous-point)
(advice-add #'vertico--update :around #'vertico--update-selected-candidate-maybe)
(advice-add #'vertico--recompute :after #'vertico--set-updated-selection)

(setq consult-line-start-from-top t)

(defun conf--embark-consult-export-xref (items)
  "Create a grep-like buffer listing ITEMS from xref."
  (let ((project-path (when-let* ((project (project-current)))
                        (project-root project))))
    (embark-consult--export-grep
     :header "Exported xref results:\n\n"
     :lines items
     :insert
     (lambda (items)
       (let ((count 0))
         (dolist (item items)
           (let* ((xref (get-text-property 0 'consult-xref item))
                  (loc (xref-item-location xref))
                  (file (or (xref-file-location-file loc) ""))
                  (line (xref-location-line loc))
                  (summary (xref-item-summary xref)))
             (insert (format "%s:%d:%s\n" (file-relative-name file project-path) line summary))
             (cl-incf count)))
         count))
     :footer (lambda ()
               (when project-path
                 (setq default-directory project-path))))))

;; export grep result with consult-line
(setf (alist-get 'consult-location embark-exporters-alist)
      #'embark-consult-export-location-grep)
(setf (alist-get 'consult-xref embark-exporters-alist)
      #'conf--embark-consult-export-xref)

(define-key embark-file-map "g" #'magit)
(add-to-list 'embark-pre-action-hooks '(magit embark--universal-argument))
(add-to-list 'embark-around-action-hooks '(magit embark--cd))

(define-key embark-file-map "M-e" #'conf--vterm-toggle)
(add-to-list 'embark-pre-action-hooks '(conf--vterm-toggle embark--universal-argument))
(add-to-list 'embark-around-action-hooks '(conf--vterm-toggle embark--cd))

(defun embark--rename-file-and-buffer (old-name)
  "Rename OLD-NAME to NEW-NAME, updating associated buffer if it exists."
  (let ((new-name (read-file-name (format "Rename '%s' to file: " (file-name-nondirectory old-name)) (file-name-directory old-name))))

    (when (not (file-directory-p (file-name-directory new-name)))
      (if (y-or-n-p (format "Create directory '%s'? "
                            (file-name-directory new-name)))
          (make-directory (file-name-directory new-name))
        (error "Cancelled")))

    (rename-file old-name new-name)

    (let ((buf (find-buffer-visiting old-name)))
      (when buf
        (with-current-buffer buf
          (set-visited-file-name new-name nil t)
          (rename-buffer (file-name-nondirectory new-name))
          (set-buffer-modified-p nil)
          (message "Renamed buffer associated with '%s' to '%s'" old-name new-name))))))

(define-key embark-file-map "r" #'embark--rename-file-and-buffer)
(add-to-list 'embark-post-action-hooks '(embark--rename-file-and-buffer embark--restart))

(defun delete-file-and-buffer (filename delete-buffer)
  "Delete the file FILENAME and its associated buffer, if any."
  (interactive
   (let* ((current-file (and (buffer-file-name) (file-name-nondirectory (buffer-file-name))))
          (filename (read-file-name (if current-file
                                        (format "Delete file ('%s' by default): " current-file)
                                      "Delete file: ")
                                    nil (buffer-file-name) t))
          (buffer (find-buffer-visiting filename))
          (delete-buffer (and buffer
                              (y-or-n-p (format "Delete buffer '%s' too? "
                                                (buffer-name buffer))))))
     (list filename delete-buffer)))
  (when filename
    (delete-file filename)
    (message "Deleted file %s" filename)
    (when delete-buffer
      (kill-buffer (find-buffer-visiting filename))
      (message "Deleted buffer associated with %s" filename))))

(define-key embark-file-map "d" #'delete-file-and-buffer)
(add-to-list 'embark-post-action-hooks '(delete-file-and-buffer embark--restart))
(add-to-list 'embark-pre-action-hooks '(delete-file-and-buffer embark--confirm))

(define-key embark-identifier-map "o" #'xref-find-definitions-other-window)

(defun copy-file-in-directory (old-name new-name &optional ok-if-already-exists)
  "Rename OLD-NAME to NEW-NAME, updating associated buffer if it exists."
  (interactive
   (let* ((old (read-file-name (format "Copy file ('%s' by default): "
                                       (file-name-nondirectory (buffer-file-name)))
                               nil (buffer-file-name) t))
          (new (read-file-name (format "Copy '%s' to file: " old) (file-name-directory old))))
     (list old new current-prefix-arg)))

  (when (not (file-directory-p (file-name-directory new-name)))
    (if (y-or-n-p (format "Create directory '%s'? "
                          (file-name-directory new-name)))
        (make-directory (file-name-directory new-name))
      (error "Cancelled")))

  (copy-file old-name new-name ok-if-already-exists))

(define-key embark-file-map "c" #'copy-file-in-directory)
(add-to-list 'embark-post-action-hooks '(copy-file-in-directory embark--restart))

(define-key embark-identifier-map "R" #'eglot-rename)
(push 'embark--allow-edit (alist-get 'eglot-rename embark-target-injection-hooks))

(defun xref-with-dumb-jump ()
  "Use xref with only dumb-jump backend."
  (interactive)
  (let ((xref-backend-functions '(dumb-jump-xref-activate))
        (xref-prompt-for-identifier nil))
    (call-interactively 'xref-find-definitions)))

(define-key embark-identifier-map "f" #'xref-with-dumb-jump)
(add-to-list 'embark-target-injection-hooks '(xref-with-dumb-jump embark--ignore-target))

(define-key embark-identifier-map "s" #'conf--consult-ripgrep)
(add-to-list 'embark-target-injection-hooks '(conf--consult-ripgrep embark--ignore-target))

;; (define-key grep-mode-map (kbd "TAB") 'compilation-display-error)
;; (define-key compilation-mode-map (kbd "TAB") 'compilation-display-error)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


;; Maybe we should do the opposite, a whitelist instead of a blacklist
(defvar conf--use-default-history '(extended-command-history))

(defun conf--vertico-exit-advice (&optional arg)
  "Change history to use input instead of selected candidate"
  (let ((input (minibuffer-contents-no-properties))
        (history-var minibuffer-history-variable))
    (run-with-timer 0 nil
                    (lambda () (interactive)
                      (when (and (boundp history-var)
                                 (not (equal (symbol-value history-var) t))
                                 (not (memq history-var conf--use-default-history)))
                        ;; (message "memory var %s" history-var)
                        (set history-var (cdr (symbol-value history-var)))
                        (add-to-history history-var input))))))

(advice-add #'vertico-exit :before 'conf--vertico-exit-advice)

(provide 'conf-completion)
