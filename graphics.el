;;  -*- lexical-binding: t; -*-

(load-theme 'custom-monokai t)

;; (setq conf--font-name "CommitMono")
(setq conf--font-name "Fira Mono")

(defun conf--setup-font ()
  (if (find-font (font-spec :name conf--font-name))
      (progn
        (message "Using %s" conf--font-name)
        (set-frame-font (format "%s-14" conf--font-name) nil t)
        (add-to-list 'default-frame-alist `(font . ,conf--font-name)))
    (warn (format "%s not found" conf--font-name))))

(defun conf--setup-font-for-frame (frame)
  (with-selected-frame frame
    (conf--setup-font)
    (add-to-list 'default-frame-alist '(cursor-color . "white"))
    (set-face-attribute 'default nil :height 140)))

(add-hook 'after-make-frame-functions 'conf--setup-font-for-frame)

(menu-bar-mode -1)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun corfu--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'corfu-border nil :background "#F8F8F2")))

(eval-after-load 'corfu
  '(progn
     (corfu--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'corfu--set-faces)))

(defun yascroll--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'yascroll:thumb-fringe    nil :foreground "gray25" :background "gray25")
    (set-face-attribute 'yascroll:thumb-text-area nil :background "gray25")))

(eval-after-load 'yascroll
  '(progn
     (yascroll--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'yascroll--set-faces)))

(defun mode-line--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'mode-line    nil :background "OliveDrab4" :foreground "black")))

(mode-line--set-faces (selected-frame))
(add-hook 'after-make-frame-functions 'mode-line--set-faces)

(defun basic--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'show-paren-match              nil :background "steelblue3")
    (set-face-attribute 'ansi-color-slow-blink         nil :box nil)
    (set-face-attribute 'pulse-highlight-face          nil :background "#49483E")
    (set-face-attribute 'pulse-highlight-start-face    nil :background "#49483E")
    (when (>= emacs-major-version 29)
      (set-face-attribute 'font-lock-property-use-face nil :inherit nil))
    (when (>= emacs-major-version 30)
      (set-face-attribute 'flymake-note-echo-at-eol    nil :foreground "gray42"))
    (set-face-attribute 'highlight                     nil :background "#49483E")
    (set-face-attribute 'gptel-context-highlight-face  nil :background "#2d3142")))

(basic--set-faces (selected-frame))
(add-hook 'after-make-frame-functions 'basic--set-faces)

;; (defun --set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (if (eq system-type 'darwin)
;;       ;; For NS/Cocoa
;;       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;     ;; For Linux
;;     (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; (--set-emoji-font nil)
;; (add-hook 'after-make-frame-functions '--set-emoji-font)

(provide 'graphics)
