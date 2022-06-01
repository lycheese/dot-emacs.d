;;; nyx-editor.el -*- lexical-binding: t; -*-

;;; Code:
(dolist (package '(cape
		   corfu
		   corfu-doc
		   eglot
		   helpful
		   magit
		   orderless
		   puni))
  (straight-use-package package))


;; store backup and auto-save files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Repeat mode
(setq repeat-mode t)


;;; isearch
(setq isearch-allow-motion t)


;;; Savehist:
(require 'savehist)

(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" nyx-local-dir))

(savehist-mode +1)


;;; Vertico&Co:
(straight-use-package '(vertico :files (:defaults "extensions/*")
				:includes (vertico-directory)))

(vertico-mode 1)

(defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)


;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Make directory movement better
(with-eval-after-load 'vertico
  (require 'vertico-directory)
  (define-key vertico-map (kbd "<backspace>") #'vertico-directory-delete-char))

;;; Corfu:
(require 'corfu)

;; Enable corfu
(global-corfu-mode 1)
;; `<TAB>' activates completion
(setq tab-always-indent 'complete
      corfu-auto t)
;; Orderless completion
(setq completion-styles '(orderless basic)
      corfu-quit-at-boundary nil)
;; Display docstrings for completion candidates
(add-hook 'corfu-mode-hook #'corfu-doc-mode)


;;; Eglot
(setq completion-category-overrides '((eglot (styles orderless))))


;;; Electric pair mode
(electric-pair-mode 1)


;;; Flymake
(straight-use-package '(flymake :type built-in))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))


;;; Puni
(require 'puni)
(puni-global-mode 1)
(add-hook 'term-mode-hook #'puni-disable-puni-mode)

(define-key puni-mode-map (kbd "C-{") #'puni-slurp-forward)
(define-key puni-mode-map (kbd "C-}") #'puni-barf-forward)


;;; Lispy (for LISPs)
(straight-use-package 'lispy)

(defun enable-lispy ()
  (lispy-mode 1))

(add-hook 'emacs-lisp-mode-hook #'puni-disable-puni-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-lispy)


;;; Helpful:

;; Helpful help buffers
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)


;;; Key Bindings:

;; More reachable ?-x on bone
(define-key key-translation-map (kbd "C-ü") (kbd "C-x"))
(define-key key-translation-map (kbd "M-ü") (kbd "M-x"))
(define-key key-translation-map (kbd "C-M-ü") (kbd "C-M-x"))

;; Make escape more immediate
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Yay ibuffer
(global-set-key [remap list-buffers] #'ibuffer)

;; bind imenu
(global-set-key (kbd "M-ä") #'imenu)

;; more useful M-<SPC>
(global-set-key [remap just-one-space] #'cycle-spacing)

;; bind `zap-up-to-char'
(global-set-key [remap zap-to-char] #'zap-up-to-char)


;;; Tab Bar:
(setq tab-bar-mode t
      tab-bar-history-mode t)

;; tab-bar-mode bindings
(global-set-key (kbd "M-[") #'tab-bar-history-back)
(global-set-key (kbd "M-]") #'tab-bar-history-forward)
(global-set-key (kbd "M-_") #'tab-undo)


(provide 'nyx-editor)
;;; nyx-editor.elends here
