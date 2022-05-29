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

(defun nyx-sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (if (file-remote-p buffer-file-name)
	(let* ((remote-path (file-remote-p buffer-file-name))
	       (host (substring remote-path (+ (string-match "@\\(.*\\):$" remote-path) 1) -1))
	       (file-path (substring buffer-file-name (string-match "[^:]*$" buffer-file-name))))
	  (find-alternate-file
	   (concat (substring remote-path 0 -1)
		   "|sudo:" host ":" file-path)))
      (find-alternate-file
       (concat "/sudo:root@localhost:"
	       buffer-file-name)))))



(provide 'nyx-core)
;;; nyx-core.elends here
