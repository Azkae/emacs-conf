(use-package cmake-mode
  :if (not (eq system-type 'windows-nt))
  :defer t
  :bind
  (:map cmake-mode-map
        ;; dump-jump doesn't work on cmake
        ("M-." . conf--consult-ripgrep))
  :config
  (setq cmake-tab-width 4))

(provide 'extra-cpp)
