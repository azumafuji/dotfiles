(use-package oauth2-auto
  :ensure t)

(use-package oauth2
  :ensure t)

(setq-default plstore-cache-passphrase-for-symmetric-encryption t)
(setq epg-pinentry-mode 'loopback)

(use-package plstore
  :ensure t)


(use-package org-caldav
  :ensure t
  :init
  (setq org-caldav-oauth2-client-id "")
  (setq org-caldav-oauth2-client-secret "")
  (setq org-caldav-url 'google)
 
  ;; calendar ID on server
  (setq org-caldav-calendar-id "")
  
  ;; Org filename where new entries from calendar stored
  (setq org-caldav-inbox "~/Documents/org/cal-work.org")
  
  ;; Additional Org files to check for calendar events
  (setq org-caldav-files nil)
  
  ;; Usually a good idea to set the timezone manually
  (setq org-icalendar-timezone "Europe/London"))
