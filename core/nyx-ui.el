;;; nyx-ui.el -*- lexical-binding: t; -*-

;;; Code:
;; (straight-use-package 'chocolate-theme)
;; (straight-use-package 'zenburn-theme)
;; (straight-use-package 'dracula-theme)
(straight-use-package 'doom-themes)

;; (require 'chocolate-theme)
(load-theme 'doom-dracula t)

(setq global-hl-line-mode t
      global-visual-line-mode t)

(blink-cursor-mode -1)

(setq ring-bell-function 'ignore
      use-short-answers t)

(straight-use-package 'which-key)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode +1))

(set-face-attribute 'default nil :font "Fira Code-11.5")

(provide 'nyx-ui)
;;; nyx-ui.el ends here
