;;; emacs-conf --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; ------------------
;; bootstrap straight
;; ------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-demand t)
(setq package-enable-at-startup nil)

;; set load path
(setq conf--base-dir (file-name-directory (or load-file-name default-directory)))
(add-to-list 'custom-theme-load-path conf--base-dir)
(add-to-list 'load-path conf--base-dir)

(setq custom-file (concat conf--base-dir "custom.el"))
(load custom-file)

(defun load-if-exists (f)
  "Load file F from `conf--base-dir' if it exists.
Use `load' rather than `require': straight.el only records the
packages whose `use-package' forms actually run in the current
session, and on a reload of the init file `require' would be a
no-op for features already provided, so `straight-freeze-versions'
would only see the packages declared in init.el itself."
  (let ((path (expand-file-name f conf--base-dir)))
    (when (file-exists-p path)
      (load path nil 'nomessage))))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package el-patch
  :custom
  (el-patch-use-aggressive-defvar t))

(use-package password-store)
(use-package pass)
(auth-source-pass-enable)
(setq epa-file-select-keys 'silent)
(setq epa-file-encrypt-to '("C37350DE46EE427FC9FA5ADFF63419C720EB67CE"))

(use-package diminish)

(diminish 'gcmh-mode)
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))
(diminish 'subword-mode)

(load-if-exists "conf-better-default.el")
(load-if-exists "conf-editing.el")
(load-if-exists "conf-meow.el")

(load-if-exists "conf-completion.el")
(load-if-exists "conf-corfu.el")
(load-if-exists "conf-eglot.el")
(load-if-exists "conf-magit.el")
(load-if-exists "conf-dape.el")

(load-if-exists "conf-vterm.el")
(load-if-exists "conf-ghostel.el")

(load-if-exists "conf-compile.el")
(load-if-exists "conf-file-modes.el")
(load-if-exists "conf-python.el")

(load-if-exists "conf-org.el")

(load-if-exists "conf-tools.el")
(load-if-exists "conf-gptel.el")
(load-if-exists "conf-agent-shell.el")

(load-if-exists "package-review.el")

(load-if-exists "graphics.el")
