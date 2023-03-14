;; -*- mode: emacs-lisp; fill-column: 78; comment-column: 50; -*-
;;; emacs -- Emacs init file for Dean

;;; Commentary:
;; My Emacs file for doing stuff

;;; Code:

;; Set initial frame size. 
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (width . 180) ; chars
              (height . 50))) ; lines
      (setq default-frame-alist
            '(
              (width . 180)
              (height . 50)))))


;; Set encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)



;; Default tabs and spacing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq-default python-indent-offset 4)

;;somewhat better long-line handling
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Some display settings for line numbers and the menubar
(setq line-number-mode t)
(setq column-number-mode t)

;; cd to my home directory on startup 
(cd "~")

;; I like backups because I don't use version control for everything
;; Put everything in a saves directory so backups are scattered out everywhere
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Don't create lock files
(setq create-lockfiles nil)

;; Set auto revert mode 
(global-auto-revert-mode 1)

;; For the GUI use this font and line spacing
(set-face-attribute 'default nil
                    :family "Iosevka Curly" :height 110)
(setq-default line-spacing 0.10)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "Iosevka Curly" :height 1.0)

;; Setup Visual line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Set up package repos
(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 8)
        ("melpa"        . 0)))

(package-initialize)


(setq package-list '(
		     ;; ace-window
                     ag
                     avy
                     beacon
                     ;; cider
                     ;; clojure-mode
                     ;; corfu
                     ;; eglot
                     ;; elfeed
                     exec-path-from-shell
                     expand-region
                     forge
                     ;; good-scroll
                     json-mode
                     magit
                     ;; modus-themes
                     ;; ng2-mode
                     ;; org-mime
                     ob-restclient
                     olivetti
                     ;; orderless
                     ox-epub
                     ox-gfm
                     ox-tufte
                     php-mode
                     ;; prettier
                     ;; prettier-js
                     ;; prettier-rc
                     ;; projectile
                     recentf
                     ;; restclient
                     ;; restclient-jq
                     shell-switcher
                     ;; terraform-doc
                     ;; terraform-mode
                     ;; treemacs
                     ;; treemacs-magit
                     ;; treemacs-projectile
                     use-package
                     ;; vertico
                     ;; which-key
                     yasnippet
                     yaml-mode
                     web-mode
		     ))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'use-package)

(use-package emacs
  :config
  (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in Modus themes

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-headings
           '((1 . (variable-pitch 1.5))
             (2 . (variable-pitch 1.3))
             (agenda-date . (1.3))
             (agenda-structure . (variable-pitch light 1.8))
             (t . (1.1))))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm)
  :bind ("<f5>" . modus-themes-toggle))


;; Fix Scrolling
;; Emacs 29, use pixel scroll
(pixel-scroll-mode 1)

;; Highlight lines when switching buffers or scrolling 
(beacon-mode 1)

;; Automatically sudo to edit files as root
;; Reopen files with sudo if they are read-only
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 4)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; This is a simple function to use the default completing read for recentf
(defun open-recentf (file)
  "Use `completing-read' to open a recent FILE."
  (interactive (list (completing-read "Find recent file: "
                                      recentf-list))))

(defun ds/find-recentf (file)
  "Use `completing-read' to open a recent FILE."
  (interactive (list (completing-read "Find recent file: "
                                      recentf-list)))
  (when file
    (find-file file)))


(require 'shell-switcher)

(use-package recentf
  :bind
  ("C-x C-r" . #'ds/find-recentf)
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

(setq recentf-exclude `(,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)))

;; use browser depending on url
(setq
 browse-url-handlerqs
 '(
   ("wikipedia\\.org" . browse-url-firefox)
   ("github" . browse-url-chromium)
   ("thefreedictionary\\.com" . eww-browse-url)
   ("." . eww-browse-url)
   ))


;; Expand region to quickly select text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(global-set-key (kbd "C-:") 'avy-goto-char)


;; ORG MODE
(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))


(use-package org
  :init
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (use-package ox-gfm)
  (use-package ox-epub)
  (use-package ox-odt)
  (use-package ox-koma-letter)
  (use-package ox-latex)
  (use-package ox-tufte)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (plantuml . t)
     (latex . t)
     (python . t)
     (restclient . t)
     (shell . t)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or  (string= lang "ditaa")              ; don't ask for ditaa or dot
              (string= lang "dot")
              (string= lang "elisp")
              (string= lang "plantuml")
              (string= lang "restclient")
              )))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/notes.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-agenda-files '("~/Documents/org"))
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
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)
  ("C-c c" . org-capture))


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package forge
  :after magit)

(setq auth-sources '("~/.authinfo.gpg"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("27fc6293ab3eaafd57703df8552d4e1629649a0f2266f107d8c9157956ce4d4b" default))
 '(package-selected-packages
   '(olivetti forge modus-themes web-mode yaml-mode yasnippet shell-switcher php-mode magit json-mode expand-region exec-path-from-shell beacon avy ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
