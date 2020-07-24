(use-package modus-operandi-theme
  :ensure t
  :init
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-visible-fringes t
        modus-operandi-theme-3d-modeline t
        modus-operandi-theme-subtle-diffs t
        modus-operandi-theme-intense-standard-completions t
        modus-operandi-theme-org-blocks 'greyscale
        modus-operandi-theme-variable-pitch-headings t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-section-headings t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2
        modus-operandi-theme-scale-5 1.3)
  :config
  (load-theme 'modus-operandi t))
