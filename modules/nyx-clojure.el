;;; nyx-clojure.el -*- lexical-binding: t; -*-

;;; Code:
(dolist (package '(clojure-mode cider))
  (straight-use-package package))

(add-hook 'clojure-mode-hook #'puni-disable-puni-mode)
(add-hook 'clojure-mode-hook #'enable-lispy)

(provide 'nyx-clojure)
;;; nyx-clojure.elends here
