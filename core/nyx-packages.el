;;; nyx-packages.el -*- lexical-binding: t; -*-

;;; Code:
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

(defvar nyx-core-packages
  '(helpful
    corfu
    puni
    ;;tempel ;; try `skeletons' first
    link-hint
    puni
    chocolate-theme
    hyperbole
    which-key))

(dolist (package nyx-core-packages)
  (straight-use-package package))

(provide 'nyx-packages)
;;; nyx-packages.el ends here
