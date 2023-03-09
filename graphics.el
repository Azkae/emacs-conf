(load-theme 'monokai t)

;; (set-default-font "Fira Mono-13")
(add-to-list 'default-frame-alist '(font . "Fira Mono"))
(add-to-list 'default-frame-alist '(cursor-color . "white"))

(set-face-attribute 'default nil :height 160)

(menu-bar-mode -1)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-unset-key (kbd "C-z"))
(normal-erase-is-backspace-mode 1)

(defun git-gutter-fringe-plus--set-faces (frame)
  (with-selected-frame frame
    (git-gutter-fr+-minimal)
    (set-face-attribute 'git-gutter-fr+-added    nil :foreground "gray25")
    (set-face-attribute 'git-gutter-fr+-deleted  nil :foreground "gray25")
    (set-face-attribute 'git-gutter-fr+-modified nil :foreground "gray25")))

(eval-after-load 'git-gutter-fringe+
  '(progn
     (git-gutter-fringe-plus--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'git-gutter-fringe-plus--set-faces)))


;; (defun git-gutter-fringe--set-faces (frame)
;;   (with-selected-frame frame
;;     (git-gutter-fr+-minimal)
;;     (set-face-attribute 'git-gutter-fr:added    nil :foreground "gray25" :background nil)
;;     (set-face-attribute 'git-gutter-fr:deleted  nil :foreground "gray25" :background nil)
;;     (set-face-attribute 'git-gutter-fr:modified nil :foreground "gray25" :background nil)))

;; (eval-after-load 'git-gutter-fringe
;;   '(progn
;;      (git-gutter-fringe--set-faces (selected-frame))
;;      (add-hook 'after-make-frame-functions 'git-gutter-fringe--set-faces)))

(defun helm--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'helm-ff-directory nil :foreground "Cyan" :weight 'bold
			:background nil)
    (set-face-attribute 'helm-ff-dotted-directory nil :foreground "White"
			:background nil)
    (set-face-attribute 'helm-ff-dotted-symlink-directory nil :foreground "Magenta"
			:background nil)
    (set-face-attribute 'helm-ff-executable nil :foreground "Orange"
			:background nil)
    (set-face-attribute 'helm-ff-file nil :foreground "White"
			:background nil)
    (set-face-attribute 'helm-ff-symlink nil :foreground "Magenta"
			:background nil)
    (set-face-attribute 'helm-locate-finish nil :foreground "Black" :weight 'bold
			:background "Yellow")
    (set-face-attribute 'helm-moccur-buffer nil :foreground "BlueViolet"
			:background nil :underline nil)))

(eval-after-load 'helm
  '(progn
     (helm--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'helm--set-faces)))


(defun company--set-faces (frame)
  (with-selected-frame frame
    (set-face-attribute 'company-echo-common nil :underline t :foreground nil)
    (set-face-attribute 'company-preview nil :inherit 'shadow :foreground nil :background nil)
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :underline t :background "LightSteelBlue3" :foreground "dark slate gray")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "SteelBlue3")
    (set-face-attribute 'company-scrollbar-fg nil :background "DeepSkyBlue4")
    (when (fboundp 'company-template-field)
      (set-face-attribute 'company-template-field nil :background "#49483E" :foreground nil))
    (set-face-attribute 'company-tooltip nil :background "LightSteelBlue1" :foreground "dark slate gray")
    (set-face-attribute 'company-tooltip-annotation nil :inherit 'company-tooltip :foreground "slate gray")
    (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :underline t :foreground nil)
    (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection :underline t :foreground nil)
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "LightSteelBlue3")
    (set-face-attribute 'company-tooltip-mouse nil :inherit 'company-tooltip :background "LightSteelBlue2")))

(eval-after-load 'company
  '(progn
     (company--set-faces (selected-frame))
     (add-hook 'after-make-frame-functions 'company--set-faces)))

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
    (set-face-attribute 'ansi-color-slow-blink nil :box nil)))

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
