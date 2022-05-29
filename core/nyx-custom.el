;;; nyx-custom.el -*- lexical-binding: t; -*-

(customize-set-variable 'custom-file
  (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'nyx-custom)
;;; nyx-custom.el ends here
