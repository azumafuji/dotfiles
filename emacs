;;; emacs -- Emacs init file for Dean
;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;; Commentary:
;; My Emacs file for doing stuff

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
(setq line-number-mode t)
(setq column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Line Numbers
;; Current line gets undented for some reason
(setq-default display-linee-numbers-type 'absolute
              display-line-numbers-width 5)

(global-display-line-numbers-mode t)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq inhibit-startup-message t)

;; I like backups because I don't use version control for everything
;; Put everything in a saves directory so backups aren't scattered out everywhere
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Set auto revert mode
(global-auto-revert-mode 1)

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

(setq package-list '(alert
                     all-the-icons
                     circe
                     company
                     company-lsp
                     company-restclient
                     counsel
                     diminish
                     doom-modeline
                     doom-themes
                     embrace
                     expand-region
                     flycheck
                     forge
                     ivy
                     js2-mode
                     json-mode
                     lsp-mode
                     lsp-ui
                     lsp-python-ms
                     magit
                     markdown-mode
                     ob-restclient
                     org
                     org-plus-contrib
                     ox-gfm
                     ox-minutes
                     ox-tufte
                     projectile
                     rainbow-delimiters
                     restclient
                     shell-switcher
                     slack
                     swiper
                     use-package
                     yasnippet
                     yasnippet-snippets))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(cd "~")

;; use-package allows lazy loading of config
(require 'use-package)

;; For the GUI use this font and line spacing
(set-face-attribute 'default nil
                    :family "Sudo" :height 120 :weight 'regular)
(setq-default line-spacing 0.20)

(load-theme `doom-vibrant t)
(use-package all-the-icons)
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

;; Keep windows clean
(add-to-list 'display-buffer-alist
             `("^\\*Slack - Antidote.+" display-buffer-reuse-window))
              

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/src/azumafuji/" "~/src/antidote/"))
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


;; ORG MODE
(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-startup-indented t)
  (use-package ox-gfm)
  (use-package ox-odt)
  (use-package ox-koma-letter)
  (use-package ox-latex)
  (use-package ox-tufte)
  (use-package ox-gfm)
  (use-package ox-minutes)
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

(use-package yasnippet                  ; Snippets
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (yas-reload-all))


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

(use-package ivy
  :bind (("C-s" . 'swiper-isearch)
         ("M-x" . 'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("M-y" . 'counsel-yank-pop)
         ("<f1> f" . 'counsel-describe-function)
         ("<f1> v" . 'counsel-describe-variable)
         ("<f1> l" . 'counsel-find-library)
         ("<f2> i" . 'counsel-info-lookup-symbol)
         ("<f2> u" . 'counsel-unicode-char)
         ("<f2> j" . 'counsel-set-variable)
         ("C-x b" . 'ivy-switch-buffer))

  :config
  (progn
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))
    
    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'full) ;Show the full virtual file paths
    
    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ;Default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    
    ;; Do not show "./" and "../" in the `counsel-find-file' completion list
    (setq ivy-extra-directories nil)))    ;Default value: ("../" "./")


(use-package rainbow-delimiters
  :config
  
  (require 'cl-lib)
  
  (defvar my-paren-dual-colors
    '("deep sky blue" "deep pink" ))

  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (set-face-foreground
    (intern (format "rainbow-delimiters-depth-%d-face" index))
    (elt my-paren-dual-colors
         (if (cl-evenp index) 0 1))))
  :hook
  (lisp-mode . rainbow-delimiters-mode))

;; Make sure we have recent files avaialble
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind
  ("C-x C-r" . counsel-recentf))

;; Shell switcher for easy access to a shell via Emacs
(use-package shell-switcher
  :init
  (setq shell-switcher-mode t)
  :bind
  ("C-'" . shell-switcher-switch-buffer)
  ("C-x 4 '" . shell-switcher-switch-buffer-other-window)
  ("C-M-'". shell-switcher-new-shell))

;; Expand region to quickly select text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; for the edges
(use-package embrace
  :bind (("C-c e" . embrace-commander))
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook))

;; Go to the place
(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

;; For all the API testing
(use-package restclient)



(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  (advice-add 'slack-counts-update :override #'ignore)
  :config
  (slack-register-team
   :name "Dean"
   :default t
   :token "xoxs-foo"
   :subscribed-channels '(productengineering london-office engineering management engineeringonly)
   :full-and-display-names t))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; LSP configurations
(use-package lsp-mode
  :ensure t)

;; company backend for lsp-mode
(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

;; Python vs ms language server
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred


;; `javascript' mode
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

;; `json' mode
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-css lsp-go lsp-html lsp-sh json-mode slack circe alert flycheck lsp-ui ox-gfm ox-minutes ox-tufte shell-switcher rainbow-delimiters org-plus-contrib ob-restclient lsp-mode embrace diminish use-package forge doom-themes doom-modeline counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide '.emacs)
;;; .emacs ends here
