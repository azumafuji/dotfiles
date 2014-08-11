;; -*- mode: Lisp; fill-column: 75; comment-column: 50; -*-
;; .emacs --- Config for Emacs
;;; Commentary:
;; My Emacs file for working with Python, Clojure, Org, and various other bits

;;; Code:
;; Update GC Settings
(setq gc-cons-threshold 20000000)

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
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(setq package-list '(auto-complete
                     ag
                     auctex
                     cider
                     color-theme-sanityinc-tomorrow
                     cyberpunk-theme
                     elpy
                     exec-path-from-shell
                     flycheck
                     helm
                     helm-ag
                     json-mode
                     lua-mode
                     magit
                     markdown-mode
                     org-plus-contrib
                     ;;org-present
                     ;;ox-reveal
                     pretty-mode
                     rainbow-delimiters
                     ;;restclient
                     ;;shell-switcher
                     ;;smartparens
                     ;;virtualenv
                     yasnippet
                     ))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;; cd to my home directory on startup 
(cd "~")

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
                    :family "M+ 1mn" :height 140 :weight 'normal)
(setq-default line-height 1.2)


;; Pretty mode redisplays some keywords as symbols
(require 'pretty-mode)
(global-pretty-mode 1)

;; Setup Visual line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; settings for emacsserver

;;(add-hook 'server-switch-hook
;;          (exec-path-from-shell-initialize))

(add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
                    (when server-buffer-clients
                      (local-set-key (kbd "C-x k") 'server-edit))))

;; Clojure editing
(require 'cider)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(require 'eldoc) ; if not already loaded

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)

;; ORG MODE
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(setq org-default-notes-file "~/Documents/org/notes.org")

(setq org-agenda-files '("~/Documents/org/projects/"))

(setq org-archive-location '("~/Documents/org/archives/%s_archive.org::* ") )

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
;; (require 'ox-mm)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/notes.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))

;; Utils
(require 'helm-config)
(helm-mode 1)

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Javascript
(require 'json-mode)
(require 'json-snatcher)

(defun js-mode-bindings ()
  "Sets a hotkey for using the json-snatcher plugin"
  (when (string-match  "\\.json$" (buffer-name))
    (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'json-mode-hook 'js-mode-bindings)

;; Lua config
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Python Config
;;(require 'virtualenv)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

;; (eval-after-load "pymacs"
;;   '(progn
;;      (require 'pymacs)
;;      (pymacs-load "ropemacs" "rope-")))

(setq ropemacs-confirm-saving 'nil)
(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
    (lambda ()
    (add-to-list 'ac-sources 'ac-source-ropemacs)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck)
(flycheck-define-checker javascript-jslint-reporter
  "A JavaScript syntax and style checker based on JSLint Reporter."
  :command ("~/.emacs.d/jslint-reporter/jslint-reporter" "--jshint" source)
  :error-patterns
  ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
  :modes (js-mode js2-mode js3-mode))

(add-hook 'js-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))
(add-hook 'json-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))

;; When using a shell, exec path to set path properly
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
