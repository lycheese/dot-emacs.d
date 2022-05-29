;;; init.el -*- lexical-binding: t; -*-

(defvar nyx-core-dir (expand-file-name "core" user-emacs-directory))
(defvar nyx-modules-dir (expand-file-name "modules" user-emacs-directory))
(defvar nyx-modules-file (expand-file-name (concat (system-name) "-modules.el") user-emacs-directory))
(defvar nyx-local-dir (expand-file-name ".local" user-emacs-directory))

(make-directory nyx-local-dir t)

(add-to-list 'load-path nyx-core-dir)
(add-to-list 'load-path nyx-modules-dir)

(require 'nyx-packages)
(require 'nyx-custom)
(require 'nyx-ui)
(require 'nyx-core)
(require 'nyx-editor)

(defvar nyx-host-modules nil
  "List of modules to be installed for host.")

(if (file-exists-p nyx-modules-file)
    (progn (load nyx-modules-file)
	   (when nyx-host-modules
	     (dolist (module nyx-host-modules)
	       (require (intern (concat "nyx-" (symbol-name module)))))))
  (message (concat "No modules file for host <" (system-name) ">!")))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
