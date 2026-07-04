;; -*- lexical-binding: t; -*-

(defun conf--corfu-active-p ()
  (and corfu-mode completion-in-region-mode))

;; (defun conf--corfu-reset()
;;   (interactive)
;;   (corfu-quit)
;;   (corfu--auto-complete-deferred))

;; (defun conf--corfu-post-command()
;;   "Refresh completion when prefix length is 3 and no candidates are found."
;;   (when (and corfu-mode completion-in-region-mode)
;;     (let* ((input (car corfu--input))
;;            (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
;;            (len (length str))
;;            (candidates corfu--candidates))
;;       (when (and (= len 3)
;;                  ;; (>= len 3)
;;                  ;; (= (% len 3) 0)
;;                  (not (try-completion str candidates)))
;;         (conf--corfu-reset)))))

(el-patch-feature corfu)

;; Disable completion starting with [ with pyright
(with-eval-after-load 'corfu
  (el-patch-defun corfu--capf-wrapper (fun &optional prefix)
    "Wrapper for `completion-at-point' FUN.
The wrapper determines if the Capf is applicable at the current
position, performs sanity checking on the returned result and computes
the initial completion state.  PREFIX is the minimum prefix length."
    (pcase (funcall fun)
      (`(,beg ,end ,table . ,plist)
       (and (integer-or-marker-p beg) ;; Valid Capf result
            (<= beg (point) end)      ;; Sanity checking
            ;; Check minimal prefix length if given.
            (or (not prefix)
                (let ((len (or (el-patch-swap
                                 (plist-get plist :company-prefix-length)
                                 (and (not (eq (char-before) (string-to-char "[")))
                                      (plist-get plist :company-prefix-length)))
                               (- (point) beg))))
                  (or (eq len t) (>= len prefix))))
            (let* ((str (buffer-substring-no-properties beg end))
                   (pt (- (point) beg))
                   (pred (plist-get plist :predicate))
                   (state (corfu--compute (cons str pt) table pred)))
              (cond ((alist-get 'corfu--candidates state)
                     `(,fun ,beg ,end ,table :corfu--state ,state ,@plist))
                    ;; Stop with empty result for exclusive Capf.
                    ((not (eq 'no (plist-get plist :exclusive)))
                     '(nil)))))))))

(use-package corfu
  :bind
  (("M-RET" . completion-at-point))
  (:map corfu-map
        ("C-a" . nil)
        ("C-e" . nil)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)
        ("M-j" . corfu-next)
        ("M-k" . corfu-previous)
        ("<remap> <move-beginning-of-line>" . nil)
        ("<remap> <move-end-of-line>" . nil)
        ("C-s" . corfu-insert-separator)
        ("TAB" . corfu-expand)
        ("<tab>" . corfu-expand)
        ("RET" . corfu-insert)
        ("<ret>" . corfu-insert)
        ("C-<return>" . corfu-insert))
  :hook
  (corfu-mode . (lambda ()
                  (add-hook 'yas-keymap-disable-hook 'conf--corfu-active-p nil t)
                  ;; (add-hook 'post-command-hook #'conf--corfu-post-command)
                  ))
  (git-commit-mode . (lambda () (corfu-mode -1)))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.5 . 0.0))
  (corfu-on-exact-match nil)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (add-hook 'after-save-hook #'corfu-quit))

(add-to-list 'completion-styles-alist
             '(tab completion-basic-try-completion ignore
               "Completion style which provides TAB completion only."))


(use-package orderless
  :init
  (setq completion-styles '(tab orderless basic)))

(when (< emacs-major-version 29)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


(defun conf--cape-dabbrev-and-enable-corfu ()
  (interactive)
  (when (not corfu-mode)
    (corfu-mode))
  (call-interactively 'cape-dabbrev))

(use-package cape
  :bind
  (("M-d" . conf--cape-dabbrev-and-enable-corfu))
  :init
  (require 'dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))

  (setq dabbrev-abbrev-char-regexp "\\sw\\|_\\|-")

  (defun conf--dabbrev-buffers ()
    (cape--buffer-list
     (lambda (buf)
       (let ((mode (buffer-local-value 'major-mode buf)))
         (and (or (provided-mode-derived-p mode #'text-mode)
                  (provided-mode-derived-p mode #'prog-mode))
              (< (buffer-size buf) 200000))))))

  (setq cape-dabbrev-buffer-function 'conf--dabbrev-buffers)

  (defun conf--setup-comint-mode-completions ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-prefix-length (cape-capf-super
                                           'conf--project-files-capf
                                           #'cape-dabbrev
                                           'conf--aidermacs-keywords-completion-at-point)
                                          3)
                 (cape-capf-prefix-length #'cape-dabbrev 3)
                 'comint-completion-at-point t)))

  (add-hook 'comint-mode-hook 'conf--setup-comint-mode-completions)

  ;; Disable ispell completion on text buffers
  (setq text-mode-ispell-word-completion nil))

(provide 'conf-corfu)
