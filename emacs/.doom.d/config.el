;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Elia Nolz"
      user-mail-address "eliarnolz@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka Fixed SS09" :size 11.5))
(setq doom-theme 'doom-dracula)
(setq display-line-numbers-type 'relative)

;;; mouse support
(setq mouse-highlight nil)

;;; auto-mode
(add-to-list 'auto-mode-alist '("\\xmobarrc" . haskell-mode))

;; ;; eshell
;; prompt
;; (setq eshell-prompt-function ...)

;;; authentification
;; THIS MUST BE A LIST!!!
(setq auth-sources '("~/.authinfo.gpg"))
(setq auth-source-debug 'trivia)

;; aliases
(after! eshell
  (set-eshell-alias!
   "la" "ls -al"
   "brctl" "brightnessctl set $1"
   "mymap" "xkbcomp -I/home/lycheese/.config/xkb /home/lycheese/.config/xkb/myMap $DISPLAY"))

;;; calc
(map! :leader
      :prefix "o"
      :n "c" 'calc-dispatch)

;;; org
;; setting the org root directory
(setq org-directory "~/Nextcloud/org/")

;; prettify org
(setq org-ellipsis "⤵")

;; modules
(after! org
  (add-to-list 'org-modules 'org-habit))

;; org files to be sourced for org-agenda
(after! org-agenda
  (setq org-agenda-files (directory-files-recursively org-directory "org$")))

;; org-agenda save org-files on quit and switching by RET
(general-advice-add '(org-agenda-quit org-agenda-switch-to)  :before 'org-save-all-org-buffers)

;; continue list when inserting a new line with =o=
(setq evil-org-special-o/O '(table-row item))

;; set a central archive location
(setq org-archive-location "~/Nextcloud/org/archive.org::* From %s")

;; use =g o= to insert new headline even when not on a headline and switch to insert mode
(defun org-new-heading-and-insert ()
  "Executes org-ctrl-c-ret and places pointer in insert mode"
  (interactive)
  (org-ctrl-c-ret)
  (evil-insert 0))
(map! :map org-mode-map
      (:prefix "g"
       :n "o" 'org-new-heading-and-insert))

;; org-bable
;; needed for #+BIND statements, but can be set locally
;; (setq org-export-allow-bind-keywords t)
;; enables code highlighting in latex exports
;; (setq org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; allows manipulation of verbatim blocks like #+RESULTS with a function
;; (setq org-export-filter-verbatim-functions '())
(setq org-latex-default-table-mode 'table)

;; anki
(use-package! anki-editor
  :after org
  :bind (:map org-mode-map
         ("<f12>" . anki-editor-cloze-region-auto-incr)
         ("<f11>" . anki-editor-cloze-region-dont-incr)
         ("<f10>" . anki-editor-reset-cloze-number)
         ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number)
  :config
  (setq anki-editor-create-decks t
        ankid-editor-org-tags-as-anki-tags t)
  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1."
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  (anki-editor-reset-cloze-number))

;; org-capture
(after! org-capture
  (setq org-capture-templates
        '(("t" "Tasks")
          ("tt" "Task for today" entry
           (file+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n SCHEDULED: %t\n%?\nAdded: %U")
          ("ts" "Scheduled task" entry
           (file+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n SCHEDULED: %^t\n%?\nAdded: %U")
          ("td" "Scheduled task with deadline" entry
           (file+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n DEADLINE: %^t\n%?\nAdded: %U")
          ("tn" "Not scheduled task" entry
           (file+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n%?\nAdded: %U")

          ("n" "Notes" entry
           (file+datetree "notes.org")
           "* %U %^{Title} %^G\n%?")

;; From org-contacts
;; :PROPERTIES:
;; :EMAIL: %(org-contacts-template-email)
;; :PHONE:
;; :ALIAS:
;; :NICKNAME:
;; :IGNORE:
;; :ICON:
;; :NOTE:
;; :ADDRESS:
;; :BIRTHDAY:
;; :END:")))
          ("c" "Contacts")
          ("cp" "Private contact" entry
           (file+olp "contacts.org" "Kontakte" "Privat")
           "* %^{Name}\n Email: %^{Email}\nTelephone: %^{Telephone number}\n** TODO Geburtstag von %\\1\nSCHEDULED: %^{Birthday}t"
           :immediate-finish t)
          ("cf" "Family contact" entry
           (file+olp "contacts.org" "Kontakte" "Familie")
           "* %^{Name}\n Email: %^{Email}\nTelephone: %^{Telephone number}\n** TODO Geburtstag von %\\1\nSCHEDULED: %^{Birthday}t"
           :immediate-finish t)
          ("cw" "Work contact" entry
           (file+olp "contacts.org" "Kontakte" "Arbeit")
           "* %^{Name}\n Email: %^{Email}\nTelephone: %^{Telephone number}\n** TODO Geburtstag von %\\1\nSCHEDULED: %^{Birthday}t"
           :immediate-finish t)
          ("co" "Other contacts" entry
           (file+olp "contacts.org" "Kontakte" "Andere")
           "* %^{Name}\n Email: %^{Email}\nTelephone: %^{Telephone number}\n** TODO Geburtstag von %\\1\nSCHEDULED: %^{Birthday}t"
           :immediate-finish t)

          ("j" "Japanese vocab")
          ("jn" "Japanese noun" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Noun")
           "* %^{Japanese} \[%^{Reading}\] %^g\n Translation: %^{English}\n New Kanji?: %^{New Kanji?|Yes|No}\nAdditional stuff: %?")
          ("jv" "Japanese verb" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Verb")
           "* %^{Japanese} \[%^{Reading}\] %^g\n Translation: %^{English}\n New Kanji?: %^{New Kanji?|Yes|No}\n Type: %^{Ichidan or Godan?|Ichidan|Godan} and %^{Transitivity|Transitive|Intransitive}\nAdditional stuff: %?")
          ("ja" "Japanese adjective" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Adjective")
           "* %^{Japanese} \[%^{Reading}\] %^g\n Translation: %^{English}\n New Kanji?: %^{New Kanji?|Yes|No}\n Type: %^{i-adj or na-adj|i-adj|na-adj}\nAdditional stuff: %?")
          ("jo" "Other japanese vocab" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Other")
           "* %^{Japanese} \[%^{Reading}\] %^g\n Translation: %^{English}\n New Kanji?: %^{New Kanji?|Yes|No}\nAdditional stuff: %?"))))

;; deft
(setq deft-directory "~/Nextcloud/org/"
      deft-recursive t)

;; writeroom
(setq writeroom-fullscreen-effect t)

;; projectile
(setq projectile-project-search-path '("~/dev" "~/Nextcloud"))



;; ;; elisp
(map! :map emacs-lisp-mode-map
      (:localleader
       :prefix "e"
       :n "t" 'ert))

(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

;; ;; mu4e
(map! :leader
      :prefix "o"
      :n "m" 'mu4e)

;; doom emacs mu4e use-package(to be modified)
;; (require 'mu4e)

(use-package! mu4e
  :commands mu4e mu4e-compose-new
  :init
  ;; (provide 'html2text)                  ; disable obsolete package
  (setq mu4e-root-maildir "~/Mail")
  (setq mu4e-attachment-dir "~/Downloads")
  :config
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-change-filenames-when-moving t)

  ;; since gmail handles saving sent mail only the uni email needs an exception
  (setq mu4e-sent-messages-behavior
        (lambda ()
          (if (string= (message-sendmail-envelope-from) "nb191@stud.uni-heidelberg.de")
              'sent 'delete)))
  (setq mu4e-update-interval nil
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t

        ;; makes text readable with dark themes
        shr-color-visible-luminance-min 80

        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800

        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls

        ;; composing options
        ;; mu4e-compose-in-new-frame t
        message-kill-buffer-on-exit t   ; close after sending

        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask
        ;; use helm/ivy
        mu4e-completing-read-function
        (cond ((featurep! :completion ivy) #'ivy-completing-read)
              ((featurep! :completion helm) #'completing-read)
              (t #'ido-completing-read))
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))

  ;; set mail user agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Use fancy icons
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "")
        mu4e-headers-passed-mark '("P" . "")
        mu4e-headers-replied-mark '("R" . "")
        mu4e-headers-seen-mark '("S" . "")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "")
        mu4e-headers-unread-mark '("u" . ""))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  (defadvice! +mu4e--refresh-current-view-a (&rest _)
    :after #'mu4e-mark-execute-all (mu4e-headers-rerun-search))

  ;; Wrap text in messages
  (setq-hook! 'mu4e-view-mode-hook truncate-lines nil)

  ;; Html mails might be better rendered in a browser
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (map! :localleader
        :map mu4e-compose-mode-map
        :desc "send and exit" "s" #'message-send-and-exit
        :desc "kill buffer"   "d" #'message-kill-buffer
        :desc "save draft"    "S" #'message-dont-send
        :desc "attach"        "a" #'mail-add-attachment)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "litschi246"
             :enter-func (lambda () (mu4e-message "Entering litschi246 context"))
             :leave-func (lambda () (mu4e-message "Leaving litschi246 context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "litschi246@gmail.com")))
             :vars '((user-mail-address           . "litschi246@gmail.com")
                     (user-full-name              . "Elia Nolz")
                     (smtpmail-smtp-user          . "litschi246@gmail.com")
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server        . "smtp.gmail.com")
                     (smtpmail-smtp-service       . 587)
                     (mu4e-sent-folder            . "/litschi246/[Gmail].All Mail")
                     (mu4e-drafts-folder          . "/litschi246/[Gmail].Drafts")
                     (mu4e-trash-folder           . "/litschi246/[Gmail].Trash")
                     (mu4e-refile-folder          . "/litschi246/[Gmail].All Mail")))
           ,(make-mu4e-context
             :name "uni"
             :enter-func (lambda () (mu4e-message "Entering uni context"))
             :leave-func (lambda () (mu4e-message "Leaving uni context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "nb191@stud.uni-heidelberg.de")))
             :vars '((user-mail-address           . "nb191@stud.uni-heidelberg.de")
                     (user-full-name              . "Elia Nolz")
                     (smtpmail-smtp-user          . "nb191@stud.uni-heidelberg.de")
                     (smtpmail-default-smtp-server . "mail.urz.uni-heidelberg.de")
                     (smtpmail-smtp-server        . "mail.urz.uni-heidelberg.de")
                     (smtpmail-smtp-service       . 587)
                     (mu4e-sent-folder            . "/nb191/Sent Mail")
                     (mu4e-drafts-folder          . "/nb191/Drafts")
                     (mu4e-trash-folder           . "/nb191/Trash")))
           ,(make-mu4e-context
             :name "eliarnolz"
             :enter-func (lambda () (mu4e-message "Entering eliarnolz context"))
             :leave-func (lambda () (mu4e-message "Leaving eliarnolz context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "eliarnolz@gmail.com")))
             :vars '((user-mail-address           . "eliarnolz@gmail.com")
                     (user-full-name              . "Elia Nolz")
                     (smtpmail-smtp-user          . "eliarnolz@gmail.com")
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server        . "smtp.gmail.com")
                     (smtpmail-smtp-service       . 587)
                     (mu4e-sent-folder            . "/eliarnolz/[Gmail].All Mail")
                     (mu4e-drafts-folder          . "/eliarnolz/[Gmail].Drafts")
                     (mu4e-trash-folder           . "/eliarnolz/[Gmail].Trash")
                     (mu4e-refile-folder          . "/eliarnolz/[Gmail].All Mail"))))))
