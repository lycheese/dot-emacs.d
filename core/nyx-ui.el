;;; nyx-ui.el -*- lexical-binding: t; -*-

;;; Code:
(nyx-ensure-packages '(chocolate-theme))

(blink-cursor-mode -1)

(setq ring-bell-function 'ignore
      use-short-answers t)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode +1))

(set-face-attribute 'default nil :font "Fira Code-11.5")

(provide 'nyx-ui)
;;; nyx-ui.el ends here
