;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Elia Nolz"
      user-mail-address "elia@nolz.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "DaddyTimeMono Nerd Font Mono" :size 12.0))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :map ivy-minibuffer-map "<right>" 'ivy-alt-done)
(map! :g "C-&" 'other-window)
(map! :map key-translation-map "C-ü" "C-x")

(map! :g "C-c a" 'org-agenda-list
      :g "C-c c" 'org-capture)

(setq mouse-highlight nil)

;;; Projectile
(after! projectile
  (setq projectile-switch-project-action #'projectile-commander))

;;; Nix
(set-formatter! 'nixpkgs-fmt "nixpkgs-fmt" :modes '(nix-mode))

;;; Org
(after! org
  ;; General org settings
  (setq org-log-into-drawer t
        org-startup-folded t
        evil-org-special-o/O '(table-row item))

  ;; Org agenda settings
  (setq org-agenda-span 'day
        org-agenda-start-day nil)
  (add-to-list 'org-modules 'org-habit)
  (general-advice-add '(org-agenda-quit org-agenda-switch-to) :before 'org-save-all-org-buffers)

  (use-package org-super-agenda
    :hook (org-agenda-mode . org-super-agenda-mode)
    :init
    (setq org-super-agenda-groups
          '((:name "Habits"
             :habit t
             :order 2)
            (:name "Today"
             :time-grid t
             :todo "TODAY"
             :scheduled today
             :order 1)
            (:name "Important"
             :tag "Important"
             :tag "Family"
             :order 1)
            (:name "Critically overdue"
             :and (:deadline past
                   :not (:todo "FILE"))
             :order 0)
            (:name "Overdue Uni Stuff"
             :and (:scheduled past
                   :tag "Uni"
                   :not (:todo "FILE"))
             :order 3)
            (:name "Uni Stuff to be filed"
             :and (:tag "Uni"
                   :todo "FILE")
             :order 4)
            (:name "University Stuff"
             :tag "Uni"
             :order 3)
            (:name "Completed projects that still need to be filed away"
             :todo "FILE"
             :order 95)
            (:name "Scheduled Projects"
             :todo "PROJ"
             :order 97)
            (:name "Emacs Stuff"
             :tag "Emacs"
             :order 98)
            (:name "Reading"
             :tag "Books"
             :order 96)
            (:name "Overdue"
             :and  (:scheduled past
                    :not (:todo "PROJ"))
             :order 1)))
    :config
    (setq org-super-agenda-header-map (make-sparse-keymap)))

  ;; Org archive settings
  (setq org-archive-location "~/org/archive.org::* From %s")

  ;; Org capture templates
  (setq org-capture-templates
        '(("t" "Tasks")
          ("tt" "Task for today" entry
           (file+olp+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n SCHEDULED: %t\n%?\nAdded: %U")
          ("ts" "Scheduled task" entry
           (file+olp+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n SCHEDULED: %^t\n%?\nAdded: %U")
          ("td" "Scheduled task with deadline" entry
           (file+olp+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n DEADLINE: %^t\n%?\nAdded: %U")
          ("tn" "Not scheduled task" entry
           (file+olp+datetree "notes.org")
           "* %^{Select type|TODO|WAIT|HOLD|KILL|DONE} %^{Task} %^G\n%?\nAdded: %U")

          ("p" "Project" entry
           (file+olp+datetree "notes.org")
           "* PROJ %^{Project} %^G\n%?\nAdded: %U")

          ("n" "Notes" entry
           (file+olp+datetree "notes.org")
           "* %U %^{Title} %^G\n%?")

          ("a" "Anki Cards")
          ("ab" "Anki Basic Block I" entry
           (file+olp+datetree "anki.org" "Block I")
           "* %<%H:%M:%S>  %^g
:PROPERTIES:
:ANKI_NOTE_TYPE: Org-Import
:ANKI_DECK: Block I
:ANKI_TAGS: FS7
:END:
,** Front
%?
,** Back
%i")

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
          ("cn" "New contact template" entry
           (file+olp "contacts.org" "Kontakte" "Testbereich")
           "* %^{Name}
:PROPERTIES:
:EMAIL: %^{Email}
:PHONE: %^{Telefon}
:ALIAS: %^{Alias}
:ADDRESS: %^{Adresse}
:END:
,,** TODO Geburtstag von %\\1
SCHEDULED:%^{Geburtstag}t %?")

          ("j" "Japanese vocab")
          ("jn" "Japanese noun" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Noun")
           "* %^{Japanese} \[%^{Reading}\] %^g
Translation: %^{English}
New Kanji?: %^{New Kanji?|Yes|No}
Additional stuff: %?")
          ("jv" "Japanese verb" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Verb")
           "* %^{Japanese} \[%^{Reading}\] %^g
Translation: %^{English}
New Kanji?: %^{New Kanji?|Yes|No}
Type: %^{Ichidan or Godan?|Ichidan|Godan} and %^{Transitivity|Transitive|Intransitive}
Additional stuff: %?")
          ("ja" "Japanese adjective" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Adjective")
           "* %^{Japanese} \[%^{Reading}\] %^g
Translation: %^{English}\nNew Kanji?: %^{New Kanji?|Yes|No}
Type: %^{i-adj or na-adj|i-adj|na-adj}
Additional stuff: %?")
          ("jo" "Other japanese vocab" entry
           (file+olp+datetree "hobby/japanese.org" "Vocab" "Other")
           "* %^{Japanese} \[%^{Reading}\] %^g
Translation: %^{English}
New Kanji?: %^{New Kanji?|Yes|No}
Additional stuff: %?")))

  ;; Org todo keywords
  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "PROJ(p)"
                                      "STRT(s@)"
                                      "WAIT(w@/!)"
                                      "HOLD(h@/!)"
                                      "IDEA(i)"
                                      "FILE(f@)"
                                      "|"
                                      "DONE(d!)"
                                      "KILL(k@)")
                            (sequence "[ ](T)"
                                      "[-](S)"
                                      "[?](W)"
                                      "|"
                                      "[X](D)")
                            (sequence "|"
                                      "OKAY(o)"
                                      "YES(y)"
                                      "NO(n)")))
  (add-to-list 'org-todo-keyword-faces '("FILE" . +org-todo-active)))
