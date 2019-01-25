;; -*- mode: Lisp; fill-column: 75; comment-column: 50; -*-
;;; emacs -- Emacs init file for Dean
;;; Commentary:
;; My Emacs file for doing stuff

;;; Code:

;; Set Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Default tabs and spacing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq-default py-indent-offset 4)

;; Some display settings for line numbers and the menubar
;;(global-linum-mode 1)
(setq line-number-mode t)
(setq column-number-mode t)
;; (setq linum-format "%5d ")
;;(global-visual-line-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq inhibit-startup-message t)

;; I like backups because I don't use version control for everything
;; Put everything in a saves directory so backups are scattered out everywhere
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Set auto revert mode 
(global-auto-revert-mode 1)

(setq ns-use-srgb-colorspace t)

;; Reopen files with sudo if they are read-only
(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; Set up package repos
(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("org"          . 8)
        ("gnu"          . 5)
        ("melpa"        . 0)))
(package-initialize)

(setq package-list '(ace-jump-mode
                     color-theme-sanityinc-tomorrow
                     company
                     company-quickhelp
                     counsel
                     elpy
                     embrace
		             expand-region
                     flycheck
                     js2-refactor
                     json-mode
		             magit
                     markdown-mode
                     ob-restclient
		             org-plus-contrib
                     ox-gfm
                     rainbow-delimiters
		             restclient
                     rjsx-mode
		             shell-switcher
                     slack
                     toml-mode
                     use-package
                     web-mode
                     yaml-mode
		             yasnippet))


(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'use-package)

;; cd to my home directory on startup 
(cd "~")

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Setup local snippets
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)

(global-flycheck-mode)


;; Set a nice color theme
(load-theme 'sanityinc-tomorrow-night t)

;; Set my preferred font https://github.com/be5invis/iosevka

(set-face-attribute 'default nil
                    :family "Iosevka Term Slab" :height 100 :weight 'regular)
(setq-default line-spacing 0.20)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)


;; Setup ivy, counsel, and swiper
                
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-display-style 'fancy)

(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; Extra packages


;; Make sure we have recent files avaialble
(use-package rainbow-delimiters
  :config
  
  (require 'cl-lib)
  
  (defvar my-paren-dual-colors
    '("hot pink" "dodger blue"))
  
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (set-face-foreground
    (intern (format "rainbow-delimiters-depth-%d-face" index))
    (elt my-paren-dual-colors
         (if (cl-evenp index) 0 1))))
  :hook
  (lisp-mode . rainbow-delimiters-mode))

(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind
  ("C-x C-r" . counsel-recentf))


(use-package shell-switcher
  :init
  (setq shell-switcher-mode t)
  :bind
  ("C-'" . shell-switcher-switch-buffer)
  ("C-x 4 '" . shell-switcher-switch-buffer-other-window)
  ("C-M-'". shell-switcher-new-shell))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

(use-package restclient)

;; ORG MODE
(use-package org-install
  :config
  (setq org-startup-indented t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq org-default-notes-file "~/Documents/org/notes.org")
  (setq org-agenda-files '("~/Documents/org/projects/"))
  (org-archive-location "~/Documents/org/archives/%s_archive.org::")
  (setq org-refile-targets (quote ((nil :maxlevel . 5)
                                   (org-agenda-files :maxlevel . 5))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
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
              (string= lang "plantuml")
              (string= lang "restclient")
              )))                     
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (use-package ox-md)
  (use-package ox-odt)
  (use-package ox-koma-letter)
  (use-package ox-beamer)
  (use-package ox-latex)
  (use-package ox-tufte)
  (use-package ox-gfm)
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/notes.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")))
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

;; gnus
(require 'gnus)


(require 'ebdb-gnus)
(require 'ebdb-message)

;; GPG
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; Keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(setq smerge-command-prefix "\C-c v")

(global-set-key (kbd "C-s") 'swiper)


;; Keep windows clean
(add-to-list 'same-window-buffer-names "*SQL*")
(add-to-list 'same-window-buffer-names "*HTTP Response*")
(add-to-list 'same-window-regexps "\*Slack.*")

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x w t") 'toggle-window-split)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel-ebdb ebdb ebdb-gnorb bbdb counsel-bbdb gnorb pinentry doom-themes counsel yasnippet use-package shell-switcher restclient org-plus-contrib magit embrace color-theme-sanityinc-tomorrow)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
