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
                    :family "Sudo" :height 120)
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
                     ag
                     cider
                     clojure-mode
                     corfu
                     corfu-doc
                     eglot
                     elfeed
                     expand-region
                     good-scroll
                     json-mode
                     magit
                     modus-themes
                     ng2-mode
                     org-mime
                     ob-restclient
                     olivetti
                     orderless
                     ox-epub
                     ox-gfm
                     ox-tufte
                     php-mode
                     prettier
                     prettier-js
                     prettier-rc
                     projectile
                     recentf
                     restclient
                     shell-switcher
                     terraform-doc
                     terraform-mode
                     treemacs
                     treemacs-magit
                     treemacs-projectile
                     use-package
                     vertico
                     which-key
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

;; Fix Scrolling
;; Emacs 29, use pixel scroll
;; Alternately good-scroll.el is also really good

(if (>= emacs-major-version 29)
    ;; ((pixel-scroll-precision-mode)
    ;;  (setq pixel-scroll-precision-use-momentum 1)
    ;;  (setq pixel-scroll-precision-large-scroll-height 20.0)
    ;;  (setq pixel-scroll-precision-interpolation-factor 30))
  (use-package good-scroll
    :hook (after-init . good-scroll-mode)))



(global-set-key (kbd "M-o") 'ace-window)

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

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
  (global-corfu-mode))

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


(use-package eglot)



;; mu4e -------------------------------------------------------
(require 'org-mime)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir")
      mu4e-refile-folder "/[Gmail].All Mail")                     

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
  mu4e-view-prefer-html t
  mu4e-update-interval 180
  mu4e-headers-auto-update t
  mu4e-compose-signature-auto-include nil
  mu4e-compose-format-flowed t)

(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq mu4e-view-show-images t)
(setq mu4e-sent-messages-behavior 'delete)
(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
           (flyspell-mode)))

(require 'smtpmail)
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)
(setq mu4e-view-show-addresses 't)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
  (list
   (make-mu4e-context
    :name "jefb" ;;for acc1-gmail
    :enter-func (lambda () (mu4e-message "Entering context JEfB"))
    :leave-func (lambda () (mu4e-message "Leaving context JEfB"))
    :match-func (lambda (msg)
		  (when msg
		(mu4e-message-contact-field-matches
		 msg '(:from :to :cc :bcc) "dean.sellis@citypantry.com")))
    :vars '((user-mail-address . "dean.sellis@citypantry.com")
	    (user-full-name . "Dean Sellis")
	    (mu4e-sent-folder . "/jefb-gmail/[jefb].Sent Mail")
	    (mu4e-drafts-folder . "/jefb-gmail/[jefb].drafts")
	    (mu4e-trash-folder . "/jefb-gmail/[jefb].Bin")
	    (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
	    (mu4e-compose-format-flowed . t)
	    (smtpmail-queue-dir . "~/Maildir/jefb-gmail/queue/cur")
	    (message-send-mail-function . smtpmail-send-it)
	    (smtpmail-smtp-user . "dean.sellis@citypantry.com")
	    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
	    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-smtp-service . 587)
	    (smtpmail-debug-info . t)
	    (smtpmail-debug-verbose . t)
	    (mu4e-maildir-shortcuts . ( ("/jefb-gmail/INBOX"            . ?i)
					("/jefb-gmail/[jefb].Sent Mail" . ?s)
					("/jefb-gmail/[jefb].Bin"       . ?t)
					("/jefb-gmail/[jefb].All Mail"  . ?a)
					("/jefb-gmail/[jefb].Starred"   . ?r)
					("/jefb-gmail/[jefb].drafts"    . ?d)
					))))))


(setq mu4e-bookmarks
      '(( :name  "Big messages"
          :query "size:5M..500M"
          :key   ?b)
        ( :name  "Unread messages"
          :query "flag:unread AND NOT (flag:trashed OR flag:list)"
          :key ?u)
        ( :name "Today's messages"
          :query "date:today..now AND NOT (flag:trashed OR flag:list)"
          :key ?t)
        ( :name "Today's list messages"
          :query "date:today..now AND flag:list AND NOT flag:trashed"
          :key ?l)
        ( :name "Last 7 days"
          :query "date:7d..now AND NOT (flag:trashed OR flag:list)"
          :hide-unread t
          :key ?w)
        ( :name "Messages with images"
          :query "mime:image/*"
          :key ?p)
        ))



;; elfeed -------------------------------------------------------
(use-package elfeed
  :bind (("C-c w" . elfeed)
         :map elfeed-show-mode-map
         ("q" . delete-window)
         ("S-SPC" . scroll-down-command)
         ("M-SPC" . scroll-down-command))
  :custom (elfeed-feeds
           '("https://news.ycombinator.com/rss"
             "https://irreal.org/blog/?feed=rss2"
             "https://emacsninja.com/feed.atom"
             "http://pragmaticemacs.com/feed/"
             "https://emacsnotes.wordpress.com/feed/"
             "https://metaredux.com/feed.xml"
             "https://emacsredux.com/atom.xml"
             "https://endlessparentheses.com/atom.xml"
             "https://www.masteringemacs.org/feed"
             "https://planet.lisp.org/rss20.xml"))
  :config
  (setq elfeed-show-entry-switch #'pop-to-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(olivetti shell-switcher prettier prettier-js prettier-rc typescript-mode org-mime terraform-doc terraform-mode php-mode org-gcal vertico ox-epub good-scroll ag corfu-doc which-key treemacs-magit treemacs-projectile treemacs projectile magit ox-tufte ox-gfm orderless yasnippet use-package modus-themes expand-region)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

