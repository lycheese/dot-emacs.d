;;; nyx-core.el -*- lexical-binding: t; -*-

;;; Code:

(auto-insert-mode 1)
(define-auto-insert
  (cons (concat (expand-file-name user-emacs-directory) "\\(core\\|modules\\)/nyx-.*\\.el")
	"Nyx Module Template")
  '("Testing"
    ";;; " (file-name-nondirectory (buffer-file-name)) " -*- lexical-binding: t; -*-"
    '(setq lexical-binding t)
    "\n\n"
    ";;; Code:"
    "\n\n\n\n"
    "(provide '" (file-name-base (buffer-file-name)) ")\n"
    ";;; " (file-name-nondirectory (buffer-file-name)) "ends here"))

(provide 'nyx-core)
;;; nyx-core.elends here
