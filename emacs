(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq-default py-indent-offset 4)

(global-linum-mode 1)
(setq line-number-mode t)
(setq column-number-mode t)
(setq linum-format "%4d ")
(global-visual-line-mode t)
(menu-bar-mode -1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(set-face-attribute 'default nil
                    :family "Envy Code R" :height 130 :weight 'normal)
(setq-default line-height 1.2)

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

(require 'color-theme)
(color-theme-initialize)
(color-theme-sanityinc-tomorrow-night)

(require 'pretty-mode)
(global-pretty-mode 1)

(require 'moinmoin-mode)
(setq auto-mode-alist (cons '("\\.moin" . moinmoin-mode) auto-mode-alist))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-cc" 'org-capture)

(require 'yasnippet) ;; not yasnippet-bundle
(setq yas-trigger-key "<backtab>")
(yas-global-mode 1)

(require 'helm-config)
(helm-mode 1)

(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

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

(when (load "flymake" t) 
     (defun flymake-pyflakes-init () 
       (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                          'flymake-create-temp-inplace)) 
      (local-file (file-relative-name 
               temp-file 
               (file-name-directory buffer-file-name)))) 
         (list "pyflakes" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks 
          '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
    (lambda ()
    (add-to-list 'ac-sources 'ac-source-ropemacs)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((virtualenv-default-directory . "~/Dev/OCINProjects/piiconsumer") (virtualenv-workon . "ocin-accounts") (virtualenv-default-directory . "~/Dev/OCINProjects/follow") (virtualenv-workon . "ocin-follow")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
