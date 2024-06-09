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
              (width . 132) ; chars
              (height . 43))) ; lines
      (setq default-frame-alist
            '(
              (width . 132)
              (height . 43)))))

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
                    :family "IBM Plex Mono" :height 100)
(setq-default line-spacing 0.2)

;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :height 1.0)

;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "IBM Plex Mono" :height 1.0 :weight 'light)

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
        ("ox-odt"       . "https://kjambunathan.github.io/elpa/")))
      package-archive-priorities
      '(("melpa-stable" . 0)
        ("gnu"          . 8)
        ("melpa"        . 10)
        ("nongnu"       . 0))

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
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible value

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
  :config
  (setq-default olivetti-body-width 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Visual enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         :scroll-bar-width 8)))
  ;; (setq spacious-padding-subtle-mode-line
  ;;     `( :mode-line-active "#222232"
  ;;        :mode-line-inactive "#eeeefe")))

(spacious-padding-mode 1)

(use-package heaven-and-hell
  :ensure t
  :config
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . tsdh-light)
          (dark . tsdh-dark))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "init.d/base.el" user-emacs-directory))
(load-file (expand-file-name "init.d/frames.el" user-emacs-directory))
(load-file (expand-file-name "init.d/dev.el" user-emacs-directory))
(load-file (expand-file-name "init.d/org.el" user-emacs-directory))
(load-file (expand-file-name "init.d/org-gcal.el" user-emacs-directory))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(heaven-and-hell ox-latex oauth2 org-caldav auto-sudoedit jinx casual-avy oauth2-auto org-gcal ob-sql-mode ob-sql ox-tufte ox-odt ox-epub ox-gfm ob-restclient docker php-mode json-mode yaml-mode forge magit tree-sitter-langs tree-sitter wgrep orderless kind-icon cape corfu-terminal corfu marginalia vertico embark-consult embark consult avy which-key exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "white")))
 '(header-line ((t :box (:line-width 4 :color "grey90" :style nil))))
 '(header-line-highlight ((t :box (:color "black"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "white")))
 '(mode-line ((t :background "white" :overline "#222232" :box (:line-width 2 :color "white" :style nil))))
 '(mode-line-active ((t :background "white" :overline "#222232" :box (:line-width 2 :color "white" :style nil))))
 '(mode-line-highlight ((t :box (:color "black"))))
 '(mode-line-inactive ((t :background "white" :overline "#eeeefe" :box (:line-width 2 :color "white" :style nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "white" :foreground "white")))
 '(window-divider ((t (:background "white" :foreground "white"))))
 '(window-divider-first-pixel ((t (:background "white" :foreground "white"))))
 '(window-divider-last-pixel ((t (:background "white" :foreground "white")))))

