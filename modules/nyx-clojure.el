;;; nyx-clojure.el -*- lexical-binding: t; -*-

;;; Code:
(straight-use-package 'cider)

(setq cider-doc-auto-select-buffer nil
      clojure-toplevel-inside-comment-form t)

(add-hook 'clojure-mode-hook #'puni-disable-puni-mode)
(add-hook 'clojure-mode-hook #'enable-lispy)
(add-hook 'clojure-mode-hook #'eglot-ensure)

(provide 'nyx-clojure)
;;; nyx-clojure.elends here
