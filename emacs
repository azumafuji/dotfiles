;; -*- mode: Lisp; fill-column: 75; comment-column: 50; -*-
;; .emacs --- Config for Emacs
;;; Commentary:
;; My Emacs file for working with Python, Clojure, Org, and various other bits

;;; Code:
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

;; For the GUI use this font and line spacing
(set-face-attribute 'default nil
                    :family "PragmataPro for Powerline" :height 130 :weight 'normal)
(setq-default line-height 1.2)

;; Set up package repos
(require 'package)
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(setq package-list '(auto-complete
                     ag
                     auctex
                     cider
                     color-theme
                     color-theme-sanityinc-tomorrow
                     clojure-mode
                     exec-path-from-shell
                     flycheck
                     helm
                     json-mode
                     json-snatcher
                     lua-mode
                     magit
                     markdown-mode
                     org-plus-contrib
                     org-present
                     ox-reveal
                     paredit
                     pretty-mode
                     pymacs
                     rainbow-delimiters
                     restclient
                     shell-switcher
                     virtualenv
                     ))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; When using a shell, exec path to set path properly
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; cd to my home directory on startup 
(cd "~")

;; local lisp configs
(add-to-list 'load-path "~/.emacs.d/lisp")  

;; Shell switcher mode for easier access to shells
(require 'shell-switcher)
(setq shell-switcher-mode t)
(define-key shell-switcher-mode-map (kbd "C-'")
            'shell-switcher-switch-buffer)
(define-key shell-switcher-mode-map (kbd "C-x 4 '")
            'shell-switcher-switch-buffer-other-window)
(define-key shell-switcher-mode-map (kbd "C-M-'")
            'shell-switcher-new-shell)

;; Make sure we have recent files avaialble
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'helm-recentf)

;; Set a nice color theme
(load-theme 'sanityinc-tomorrow-night t)

;; Pretty mode redisplays some keywords as symbols
(require 'pretty-mode)
(global-pretty-mode 1)

;; settings for emacsserver


(add-hook 'server-switch-hook
          (exec-path-from-shell-initialize))

(add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
                    (when server-buffer-clients
                      (local-set-key (kbd "C-x k") 'server-edit))))

;; Clojure editing
(require 'cider)

;; paredit config
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-mode-hook            #'enable-paredit-mode)

(require 'eldoc) ; if not already loaded
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)
   (plantuml . t)))


(setq org-default-notes-file "~/Documents/org/notes.org")

(require 'ox-md)
(require 'ox-odt)
(require 'ox-reveal)
(require 'ox-koma-letter)
(require 'ox-beamer)
(require 'ox-latex)


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
(require 'virtualenv)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(eval-after-load "pymacs"
  '(progn
     (require 'pymacs)
     (pymacs-load "ropemacs" "rope-")))


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
  :modes (js-mode js2-mode js3-mode json-mode))

(add-hook 'js-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))
(add-hook 'json-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Documents/org/notes.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; .emacs ends here
(put 'narrow-to-region 'disabled nil)
