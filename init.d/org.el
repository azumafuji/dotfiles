;;; Emacs Bedrock
;;;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables



;;; Phase 2 variables

;; Agenda variables
(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "work.org" "notes.org"))

;; Default tags
(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("JEfB" . ?b)
                      ("JET" . ?j)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("reference")
                      ("review")
                      ("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets '(("work.org" :maxlevel . 1) ("inbox.org" :maxlevel . 1)))

;;; Phase 3 variables
(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (use-package ob-restclient
    :ensure t)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (plantuml . t)
     (latex . t)
     (python . t)
     (restclient . t)
     (shell . t)
     (sql . t)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or  (string= lang "ditaa")              ; don't ask for ditaa or dot
              (string= lang "dot")
              (string= lang "plantuml")
              (string= lang "restclient")
              (string= lang "latex")
              (string= lang "sql")
              (string= lang "emacs-lisp")
              (string= lang "sh")
              )))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  
  (setq org-babel-latex-pdf-svg-process "pdf2svg %F %O")
  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("m" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("r" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
             ((agenda)
              (todo)))
            ("w" "Work" agenda ""
             ((org-agenda-files '("work.org")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ox-gfm
  :ensure t)
(use-package ox-epub
  :ensure t)
(use-package ox-odt
  :ensure t)
(use-package ox-tufte
  :ensure t)
(use-package ob-sql-mode
  :ensure t)


(setq exec-path (append exec-path '("/usr/local/texlive/2024/bin/x86_64-linux")))

;;;  Set Font Sizes
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(setq org-latex-packages-alist '(("" "listings")
                                 ("" "booktabs")
                                 ("AUTO" "polyglossia" t ("lualatex" "xelatex"))
                                 ("" "grffile")
                                 ("" "unicode-math")
                                 ("" "xcolor")
                                 ("inkscapelatex=false" "svg")
                                 ("" "fontspec")))
                                 

(setq org-latex-pdf-process '("latexmk -lualatex -shell-escape -quiet -f %f"))
(setq org-latex-compiler "lualatex")

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass[9pt,a4paper,extrafontsizes,article]{memoir}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")       
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("tufte"
                 "\\documentclass[8pt]{tufte-handout}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")       
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               )
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\section{%s}"        . "\\section*{%s}")
                 ("\\subsection{%s}"     . "\\subsection*{%s}")
                 ("\\subsubsection{%s}"  . "\\subsubsection*{%s}")))
  )

