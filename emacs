;; -*- mode: Lisp; fill-column: 75; comment-column: 50; -*-
;;; emacs -- Emacs init file for Dean
;;; Commentary:
;; My Emacs file for working with Python, Clojure, Org, and various other bits

;;; Code:


;; Set encoding
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
;; (global-visual-line-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq inhibit-startup-message t)

;; I like backups because I don't use version control for everything
;; Put everything in a saves directory so backups are scattered out everywhere
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq ns-use-srgb-colorspace t)

;; Set up package repos
(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(setq package-list '(ag
                     cyberpunk-theme
                     elpy
                     exec-path-from-shell
                     helm
                     helm-ag
                     json-mode
                     json-snatcher
                     magit
                     markdown-mode
                     org-plus-contrib
                     org-tree-slide
                     pandoc-mode
                     pretty-mode
                     pyvenv
                     rainbow-delimiters
                     shell-switcher
                     smartparens
                     yaml-mode
                     yasnippet
                     ))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; cd to my home directory on startup 
(cd "~")

;; When using a shell, exec path to set path properly
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; local lisp configs
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Shell switcher mode for easier access to shells
(require 'shell-switcher)
(setq shell-switcher-mode t)
(define-key shell-switcher-mode-map (kbd "C-'")
            'shell-switcher-switch-buffer)
(define-key shell-switcher-mode-map (kbd "C-x 4 '")
            'shell-switcher-switch-buffer-other-window)
(define-key shell-switcher-mode-map (kbd "C-M-'")
            'shell-switcher-new-shell)

;; Setup local snippets
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)

;; Make sure we have recent files avaialble
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'helm-recentf)

;; Set a nice color theme
(load-theme 'cyberpunk t)

;; For the GUI use this font and line spacing
(set-face-attribute 'default nil
                    :family "mononoki" :height 120 :weight 'normal)
(setq-default line-height 1.2)

;; Pretty mode redisplays some keywords as symbols
(require 'pretty-mode)
(global-pretty-mode 1)

;; Setup Visual line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; ORG MODE
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-default-notes-file "~/Documents/org/notes.org")

(setq org-agenda-files '("~/Documents/org/projects/"))

(setq org-archive-location "~/Documents/org/archives/%s_archive.org::")

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
   (python . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (or  (string= lang "ditaa")              ; don't ask for ditaa or dot
         (string= lang "dot"))))                                    
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(require 'ox-md)
(require 'ox-odt)
(require 'ox-reveal)
(require 'ox-koma-letter)
(require 'ox-beamer)
(require 'ox-latex)
(require 'ox-mm)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/notes.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))

(add-to-list 'org-latex-classes
        '("memoir"
          "\\documentclass[9pt,letterpaper,extrafontsizes,article]{memoir}"
          ("\\chapter{%s}" . "\\chapter*{%s}")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")       
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
        '("tufte"
          "\\documentclass[10pt]{tufte-handout}"
          ("\\chapter{%s}" . "\\chapter*{%s}")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")       
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        )

;; Utils
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(add-hook 'after-init-hook 'global-company-mode)


;; Python
(elpy-enable)


;; Default Files to open
(find-file "~/Documents/org/notes.org")
(find-file "~/Documents/org/journal.org")
(find-file "~/Documents/org/projects/trialreach.org")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-tree-slide yasnippet yaml-mode smartparens shell-switcher rainbow-delimiters pretty-mode pandoc-mode org-plus-contrib markdown-mode magit json-mode helm-ag exec-path-from-shell cyberpunk-theme ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
