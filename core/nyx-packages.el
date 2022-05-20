;;; nyx-packages.el -*- lexical-binding: t; -*-

;;; Code:
(require 'package)


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defmacro nyx-ensure-packages (packages)
  `(dolist (package ,packages)
     (when (not (package-installed-p package))
       (package-install package))))

(defvar nyx-core-packages
  '(helpful
    corfu
    puni
    ;;tempel ;; try `skeletons' first
    link-hint
    puni
    cider
    chocolate-theme
    hyperbole
    which-key))

(nyx-ensure-packages nyx-core-packages)


(provide 'nyx-packages)
;;; nyx-packages.el ends here
