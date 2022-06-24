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

;; (set-face-attribute 'default nil :font "Fira Code-11.5")
(set-face-attribute 'default nil :font "Iosevka Medium-11.5")


;;; From Doom
(defvar +emacs-lisp--face nil)
;;;###autoload
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)		; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)		; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq +emacs-lisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq +emacs-lisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

;; HACK Fontification is already expensive enough. We byte-compile
;;      `+emacs-lisp-highlight-vars-and-faces' and `+emacs-lisp-truncate-pin' to
;;      ensure they run as fast as possible:
(dolist (fn '(+emacs-lisp-highlight-vars-and-faces))
  (unless (byte-code-function-p (symbol-function fn))
    (with-no-warnings (byte-compile fn))))

;; Enhance elisp syntax highlighting, by highlighting Doom-specific
;; constructs, defined symbols, and truncating :pin's in `package!' calls.
(font-lock-add-keywords
 'emacs-lisp-mode
 (append ;; highlight defined, special variables & functions
         (when t
           `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))
;; End Doom


(provide 'nyx-ui)
;;; nyx-ui.el ends here
