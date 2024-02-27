;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; cd to my home directory on startup 
(cd "~")

;; I like backups because I don't use version control for everything
;; Put everything in a saves directory so backups are not scattered
(setq backup-directory-alist `(("." . "~/.saves/")))
(setq auto-save-file-name-transforms `((".*" "~/.saves/" t)))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Don't create lock files
(setq create-lockfiles nil)
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
;;(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Set Fonts
;; For the GUI use this font and line spacing
;;(set-face-attribute 'default nil
;;                    :family "Iosevka Aile" :height 90 :weight 'Semilight)

(set-face-attribute 'default nil
                    :family "Iosevka Term Curly" :height 110)
(setq-default line-spacing 0.2)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term Curly" :height 1.0 :weight 'light)

;; Set docview DPI
(setq doc-view-resolution 300)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Packages 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("ox-odt" . "https://kjambunathan.github.io/elpa/")))
      package-archive-priorities
      '(("melpa-stable" . 0)
        ("gnu"          . 8)
        ("melpa"        . 10)
        ("nongnu"       . 0))

;; (setq package-list '(ag
;;                      expand-region
;;                      exec-path-from-shell
;;                      ob-restclient
;;                      pulsar
;;                      treepy
;;                      which-key
;;                      ))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (dolist (package package-list)
;;   (when (not (package-installed-p package))
;;     (package-install package)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup
;;(add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; Much more eager
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode or fido-mode. See also
;; the file extras/base.el
;(fido-vertical-mode)
;(setq icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Somewhat better long-line handling
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Mode line information
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(setq pixel-scroll-precision-mode t)                  ; Smooth scrolling
(setq frame-inhibit-implied-resize t)                 ; Don't arbitrarily resize frame
(setq show-trailing-whitespace t)
(setq kill-whole-line t)

;; Use common keystrokes by default
;;(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 4)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Expand region to quickly select text
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))


;; Recent Files search
(defun ds/find-recentf (file)
  "Use `completing-read' to open a recent FILE."
  (interactive (list (completing-read "Find recent file: "
                                      recentf-list)))
  (when file
    (find-file file)))

(use-package recentf
  :ensure t
  :bind
  ("C-x C-r" . #'ds/find-recentf)
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

(setq recentf-exclude `(,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)))

;; Set background for sudo buffers
(defun ds/sudo-bg-color ()
       (setq buffer-face-mode-face '(:background "#FFF0F9"))
       (buffer-face-mode 1))

(defun ds/sudo-set-bg ()
  (cond ((string-match-p "sudo" (concat "." (file-remote-p default-directory)))
         (ds/sudo-bg-color))))

(add-hook 'find-file-hook 'ds/sudo-set-bg)

;; Ollivetii 
(use-package olivetti
  :ensure t
  :hook (text-mode . olivetti-mode)
  :config
  (setq-default olivetti-body-width 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        )
  (setq modus-themes-headings
        '((1 . (variable-pitch 1.3))
          (2 . (variable-pitch 1.15))
          (3 . (variable-pitch))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.45))
          (t . (1.1))))
  (load-theme 'modus-operandi t)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

(load-file (expand-file-name "extras/treemacs.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))



(use-package yasnippet
  :ensure t
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))


(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	(and (minibuffer-window-active-p (minibuffer-window))
	     (with-selected-window (minibuffer-window)
	       (eq window (minibuffer-selected-window)))))))

(use-package spacious-padding
  :ensure t
  :bind
  ("<f8>" . spacious-padding-mode)
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 2
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8))
  (setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border)))

(spacious-padding-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (load-theme 'tsdh-light t)

;; (use-package mindre-theme
;;     :ensure t
;;     :custom
;;     (mindre-use-more-bold t)
;;     (mindre-use-faded-lisp-parens t)
;;     :config
;;     (load-theme 'mindre t))

(use-package hima-theme
    :ensure t
    :config
    (load-theme 'hima t))


;; (use-package emacs
;;   :config
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-mixed-fonts t
;;         )
;;   (setq modus-themes-headings
;;         '((1 . (variable-pitch 1.3))
;;           (2 . (variable-pitch 1.15))
;;           (3 . (variable-pitch))
;;           (agenda-date . (1.3))
;;           (agenda-structure . (variable-pitch light 1.45))
;;           (t . (1.1))))
;;   (load-theme 'modus-operandi t)
;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

(load-file (expand-file-name "extras/treemacs.el" user-emacs-directory))
 
;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the elisp file for more
;; details.
;(load-file (expand-file-name "extras/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f60404efc40b646a6a742d833c7097f9225550288565f945ec990d343c1a22ff" "fbf914d9595c385f605133f4f221449c18b57370e7a292562aa62c95a86d8782" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(ef-themes ob-restclient hima-theme yasnippet yaml-mode which-key wgrep vertico treemacs-tab-bar treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil spacious-padding php-mode ox-tufte ox-pandoc ox-gfm ox-epub orderless marginalia kind-icon json-mode expand-region exec-path-from-shell embark-consult docker corfu-terminal cape)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#f2f2f2" :style nil))))
 '(header-line-highlight ((t :box (:color "#000000"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :background "#ffffff" :overline "#000000" :box (:line-width 2 :color "#ffffff" :style nil))))
 '(mode-line-active ((t :background "#ffffff" :overline "#000000" :box (:line-width 2 :color "#ffffff" :style nil))))
 '(mode-line-highlight ((t :box (:color "#000000"))))
 '(mode-line-inactive ((t :background "#ffffff" :overline "#9f9f9f" :box (:line-width 2 :color "#ffffff" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c2c2c2" :style nil))))
 '(window-divider ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider-first-pixel ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider-last-pixel ((t :background "#ffffff" :foreground "#ffffff"))))
