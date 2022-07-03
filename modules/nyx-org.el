;;; nyx-org.el --- Module for org-mode               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lycheese@nanoforge>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(straight-use-package '(org :type built-in))
(straight-use-package 'org-super-agenda)
(straight-use-package 'org-modern)
(straight-use-package 'org-roam)
(straight-use-package 'org-pomodoro)

;; personal
(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-directory "~/org"
      org-startup-indented t
      org-startup-folded t
      org-todo-keywords '((sequence "TODO(t)"
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
				    "NO(n)"))
      org-log-into-drawer t)

;;; latex
(setq org-preview-latex-default-process 'dvisvgm)


;;; org-agenda
(global-set-key (kbd "C-c n a") #'org-agenda)

(let ((org-files (directory-files-recursively org-directory "org$")))
    (setq org-agenda-files (append (cl-remove-if (lambda (x) (or (string-match "fs[567]/" x)
                                                                 (string-match "roam/" x)
                                                                 (string-match "org-roam.bak/" x)))
                                                 org-files)
                                   (cl-remove-if-not (lambda (x) (string-match "roam/daily/.+" x))
                                                     org-files)
                                   '())
          org-agenda-span 'day
          org-agenda-start-day nil))

(setq nyx-org-roam-daily-files (directory-files-recursively (concat org-directory "/roam/daily/") ".org$"))

(setq org-agenda-custom-commands
      '(("d" "All todos in daily notes" alltodo ""
	 ((org-agenda-files nyx-org-roam-daily-files)))))


;;; org-super-agenda
(setq org-super-agenda-groups
      '(;; (:name "Unscheduled"
	;;        :and (:file-path "org/roam/daily"
	;; 			:pred ((lambda (item) (when (s-matches? "2022-06" item) (setq a item))))
	;; 	     ))
	;; (:name "Overdue Dailies"
	;;        :and (:scheduled nil
	;; 			:not (:todo ("DONE" "KILL"))
	;; 			:deadline nil
	;; 			:file-path "org/roam/daily/.*\.org$")
	;;        :order 3)

        (:name "Habits"
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
               :and (:scheduled past
				:not (:todo "PROJ"))
               :order 1)))

(add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)


;;; org-capture
(setq org-capture-templates
      '(("p" "Project" entry
         (file+headline "gtd.org" "Geplante Projekte")
         "* PROJ %^{Project} %^G\n%?\nAdded: %U")
        ("j" "Japanese vocab")
        ("jn" "Japanese noun" entry
         (file+olp+datetree "hobby/japanese.org" "Vocab" "Noun")
         "* %^{Japanese} \[%^{Reading}\] %^g\nTranslation: %^{English}\nNew Kanji?: %^{New Kanji?|Yes|No}\nAdditional stuff: %?")
        ("jv" "Japanese verb" entry
         (file+olp+datetree "hobby/japanese.org" "Vocab" "Verb")
         "* %^{Japanese} \[%^{Reading}\] %^g\nTranslation: %^{English}\nNew Kanji?: %^{New Kanji?|Yes|No}\nType: %^{Ichidan or Godan?|Ichidan|Godan} and %^{Transitivity|Transitive|Intransitive}\nAdditional stuff: %?")
        ("ja" "Japanese adjective" entry
         (file+olp+datetree "hobby/japanese.org" "Vocab" "Adjective")
         "* %^{Japanese} \[%^{Reading}\] %^g\nTranslation: %^{English}\nNew Kanji?: %^{New Kanji?|Yes|No}\nType: %^{i-adj or na-adj|i-adj|na-adj}\nAdditional stuff: %?")
        ("jo" "Other japanese vocab" entry
         (file+olp+datetree "hobby/japanese.org" "Vocab" "Other")
         "* %^{Japanese} \[%^{Reading}\] %^g\nTranslation: %^{English}\nNew Kanji?: %^{New Kanji?|Yes|No}\nAdditional stuff: %?")))

;;; org-modern

;; Choose some fonts
;; (set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

;; Doom
(with-no-warnings
  (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

(dolist (keyword-face '(("FILE" . +org-todo-active)
			("[-]" . +org-todo-active)
			("STRT" . +org-todo-active)
			("[?]" . +org-todo-onhold)
			("WAIT" . +org-todo-onhold)
			("HOLD" . +org-todo-onhold)
			("PROJ" . +org-todo-project)
			("NO" . +org-todo-cancel)
			("KILL" . +org-todo-cancel)))
  (add-to-list 'org-todo-keyword-faces keyword-face))
;; Doom end

;; org-roam
(setq org-roam-directory "~/org/roam/")

(defun org-roam-node-count-zettels (_a)
  (+ (length (doom-files-in (concat org-roam-directory "zettelkasten"))) 1))

(setq org-roam-capture-templates
      '(("z" "Zettel" plain "%?"
         :target (file+head "zettelkasten/%<%Y%m%d%H%M%S>-${count-zettels}.org"
                            "#+title: ${count-zettels}\n")
         :unnarrowed t)
        ("t" "topical" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("h" "hierarchical" plain "%?"
         :target (file+head "%(read-directory-name \"topic: \" org-roam-directory)/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("l" "literature" plain "%?"
         :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("p" "project-associated" plain "%?"
         :target (file+head "projects/${project}/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

(with-eval-after-load 'org-roam
  ;; (require 'org-fold)
  (org-roam-db-autosync-mode))

;; roam keybindings
(global-set-key (kbd "C-c n r") #'org-roam-node-find)

(global-set-key (kbd "C-c n d .") #'org-roam-dailies-find-directory)
(global-set-key (kbd "C-c n d b") #'org-roam-dailies-goto-previous-note)
(global-set-key (kbd "C-c n d c") #'org-roam-dailies-goto-date)
(global-set-key (kbd "C-c n d d") #'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c n d f") #'org-roam-dailies-goto-next-note)
(global-set-key (kbd "C-c n d n") #'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n d t") #'org-roam-dailies-goto-tomorrow)
(global-set-key (kbd "C-c n d v") #'org-roam-dailies-capture-date)
(global-set-key (kbd "C-c n d y") #'org-roam-dailies-goto-yesterday)

;; org-roam-bibtex
(straight-use-package 'org-roam-bibtex)
(straight-use-package 'citar)

(setq citar-open-note-function '(orb-citar-edit-note)
      citar-bibliography '("~/org/bibtex/primary.bib" ;; "~/org/bib/references.bib"
			   )
      citar-notes-paths '("~/org/roam/literature/")
      orb-roam-ref-format 'org-cite)

(global-set-key (kbd "C-c n a") #'citar-open)

(org-roam-bibtex-mode 1)

;; org-noter
(straight-use-package 'org-noter)


;;; commands
(defun nyx-find-file-in-notes ()
  "Find a file in the `org-directory'."
  (interactive)
  (let* ((pr (project-current nil org-directory))
	 (dirs (list (project-root pr))))
    (project-find-file-in nil dirs pr)))

;; keybindings
(global-set-key (kbd "C-c n f") #'nyx-find-file-in-notes)


(provide 'nyx-org)
;;; nyx-org.el ends here
