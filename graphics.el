;;  -*- lexical-binding: t; -*-

(load-theme 'custom-monokai t)

(defun conf--setup-font ()
  (if (or (find-font (font-spec :name "Fira Mono")) t)
      (progn
        (message "Using Fira Mono")
        (set-frame-font "Fira Mono-13" nil t)
        (add-to-list 'default-frame-alist '(font . "Fira Mono")))
    (message "Fira Mono not found"))

  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (set-face-attribute 'default nil :height 140))

(conf--setup-font)
;; (add-hook 'after-init-hook 'conf--setup-font)

(menu-bar-mode -1)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (defun helm--set-faces (frame)
;;   (with-selected-frame frame
;;     (set-face-attribute 'helm-ff-directory nil :foreground "Cyan" :weight 'bold
;; 			:background nil)
;;     (set-face-attribute 'helm-ff-dotted-directory nil :foreground "White"
;; 			:background nil)
;;     (set-face-attribute 'helm-ff-dotted-symlink-directory nil :foreground "Magenta"
;; 			:background nil)
;;     (set-face-attribute 'helm-ff-executable nil :foreground "Orange"
;; 			:background nil)
;;     (set-face-attribute 'helm-ff-file nil :foreground "White"
;; 			:background nil)
;;     (set-face-attribute 'helm-ff-symlink nil :foreground "Magenta"
;; 			:background nil)
;;     (set-face-attribute 'helm-locate-finish nil :foreground "Black" :weight 'bold
;; 			:background "Yellow")
;;     (set-face-attribute 'helm-moccur-buffer nil :foreground "BlueViolet"
;; 			:background nil :underline nil)))

;; (eval-after-load 'helm
;;   '(progn
;;      (helm--set-faces (selected-frame))
;;      (add-hook 'after-make-frame-functions 'helm--set-faces)))

(defun corfu--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'corfu-border nil :background "#F8F8F2")
    ))

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
    (set-face-attribute 'show-paren-match    nil :background "steelblue3")
    (set-face-attribute 'ansi-color-slow-blink nil :box nil)
    (set-face-attribute 'pulse-highlight-face    nil :background "#49483E")
    (set-face-attribute 'pulse-highlight-start-face    nil :background "#49483E")
    (when (>= emacs-major-version 29)
      (set-face-attribute 'font-lock-property-use-face nil :inherit nil))
    (when (>= emacs-major-version 30)
      (set-face-attribute 'flymake-note-echo-at-eol nil :foreground "gray42"))
    (set-face-attribute 'highlight nil :background "#49483E")))

(basic--set-faces (selected-frame))
(add-hook 'after-make-frame-functions 'basic--set-faces)

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

(--set-emoji-font nil)
(add-hook 'after-make-frame-functions '--set-emoji-font)

(provide 'graphics)
