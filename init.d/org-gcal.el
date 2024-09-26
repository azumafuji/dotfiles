

(setq org-gcal-client-id ""
      org-gcal-client-secret ""
      org-gcal-fetch-file-alist '(("" . "~/Documents/org/cal-dean.org")
                                  ("" . "~/Documents/org/cal-pandt.org")
                                  ))

(use-package oauth2-auto
  :ensure t)

(use-package org-gcal
  :ensure t)


(setq-default plstore-cache-passphrase-for-symmetric-encryption t)
(setq epg-pinentry-mode 'loopback)

(use-package plstore
  :ensure t)


(unless (boundp 'org-agenda-files) (setq org-agenda-files ()))
(append org-agenda-files `("cal-dean.org" "cal-pandt.org"))

