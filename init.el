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
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

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

(require 'conf-better-default)
(require 'conf-editing)
(require 'conf-meow)

(require 'conf-completion)
(require 'conf-corfu)
(require 'conf-eglot)
(require 'conf-magit)
(require 'conf-dape)

(require 'conf-vterm)
(require 'conf-ghostel)

(require 'conf-compile)
(require 'conf-file-modes)
(require 'conf-python)

(require 'conf-org)

(require 'conf-tools)
(require 'conf-gptel)
(require 'conf-agent-shell)

(require 'package-review)

(require 'graphics)
