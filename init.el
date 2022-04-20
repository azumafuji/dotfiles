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

;; Set auto revert mode 
(global-auto-revert-mode 1)

;; For the GUI use this font and line spacing
(set-face-attribute 'default nil
                    :family "Sudo" :height 140)
(setq-default line-spacing 0.20)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Sudo UI" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "Sudo" :height 1.0)

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


(setq package-list '(ace-window
                     cider
                     clojure-mode
                     corfu
                     corfu-doc   
                     expand-region
                     good-scroll
                     json-mode
                     lsp-mode
                     lsp-treemacs
                     lsp-ui
                     magit
                     mct
                     modus-themes
                     ob-restclient
                     orderless
                     ox-gfm
                     ox-tufte
                     projectile
                     recentf
                     restclient
                     treemacs
                     treemacs-magit
                     treemacs-projectile
                     use-package
                     which-key
                     yasnippet
                     yaml-mode
                     ))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'use-package)

;; Fix Scrolling
;; Emacs 29, use pixel scroll

(if (>= emacs-major-version 29)
    ((pixel-scroll-precision-mode)
     (setq pixel-scroll-precision-use-momentum 1)
     (setq pixel-scroll-precision-large-scroll-height 20.0)
     (setq pixel-scroll-precision-interpolation-factor 30))
  (use-package good-scroll
    :hook (after-init . good-scroll-mode)))

;; Alternately good-scroll.el is also really good 


(global-set-key (kbd "M-o") 'ace-window)


(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package mct
  :config
  (setq mct-apply-completion-stripes t)
  ;; We make the SPC key insert a literal space and the same for the
  ;; question mark.  Spaces are used to delimit orderless groups, while
  ;; the quedtion mark is a valid regexp character.
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil))
  
  ;; Because SPC works for Orderless and is trivial to activate, I like to
  ;; put `orderless' at the end of my `completion-styles'.  Like this:
  (setq completion-styles
        '(basic substring initials flex partial-completion orderless))
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))))
  :init
  (mct-minibuffer-mode 1)
  (mct-region-mode 1))

(use-package recentf
  :bind
  ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100
        )
  (recentf-mode 1))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

(add-hook 'corfu-mode-hook #'corfu-doc-mode)
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) 
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

 
;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


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
  (setq tab-always-indent 'complete))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)


;;best themes with easy switching between dark and light
(use-package modus-themes
  :ensure
  :init
  (setq modus-themes-italics-constructs t
        modus-themes-bold-constructs t
        modus-themes-fringes nil ; {nil,'subtle,'intense}
        modus-themes-mode-line 'nil ; {nil,'3d,'moody}
        modus-themes-syntax nil ; {nil,'faint,'yellow-comments,'green-strings,'yellow-comments-green-strings,'alt-syntax,'alt-syntax-yellow-comments}
        modus-themes-intense-hl-line nil
        modus-themes-paren-match '(intense) ; {nil,'subtle-bold,'intense,'intense-bold}
        modus-themes-links '(neutral-underline background) ; {nil,'faint,'neutral-underline,'faint-neutral-underline,'no-underline}
        modus-themes-no-mixed-fonts nil
        modus-themes-prompts nil ; {nil,'subtle,'intense}
        modus-themes-completions nil ; {nil,'moderate,'opinionated}
        modus-themes-region '(bg-only-no-extend) ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs 'bg-only ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
        modus-themes-headings ; Read the manual for this one
          '((1 . (background overline rainbow variable-pitch 1.6))
            (2 . (background overline variable-pitch 1.4))
            (3 . (background overline variable-pitch 1.3))
            (t . (overline variable-pitch 1.2)))
        modus-themes-variable-pitch-ui t
        modus-themes-scale-headings t)

  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; Expand region to quickly select text
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))



;; ORG MODE
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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0))

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package treemacs-projectile
   :after (treemacs projectile)
   :ensure t)


(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package treemacs-magit
   :after (treemacs magit)
   :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-treemacs
    :after (lsp-mode)
    :config
    (lsp-treemacs-sync-mode 1))


;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;    :config
;;    (which-key-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ox-epub good-scroll ag corfu-doc which-key treemacs-magit treemacs-projectile treemacs projectile magit lsp-mode ox-tufte ox-gfm orderless mct yasnippet use-package modus-themes expand-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

