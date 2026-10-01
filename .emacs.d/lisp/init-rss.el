;;; init-rss.el --- RSS with elfeed -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-mail.  Disabled with EMACS_NORSS=Y.

;;; Code:

;;; RSS reading

;; Couple functions to handle sync
;; mainly ripped off from https://pragmaticemacs.wordpress.com/2016/08/17/read-your-rss-feeds-in-emacs-with-elfeed/
(defun perso/elfeed-load-db-and-run ()
  "Wrapper to load the elfeed db from disk on startup"
  (interactive)
  (elfeed)
  (elfeed-db-load)
  (elfeed-search-update--force))

(defun perso/elfeed-load-db-and-update ()
  "Wrapper to load the elfeed db from disk before updating search view"
  (interactive)
  (elfeed-db-load)
  (elfeed-search-update--force))

(defun perso/elfeed-load-db-and-update-feeds ()
  "Wrapper to load the elfeed db from disk before updating feeds"
  (interactive)
  (elfeed-db-load)
  (elfeed-search-update--force)
  (elfeed-update)
  (elfeed-db-save))

(defun perso/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun perso/elfeed-mark-all-as-read ()
  "Mark all result in elfeed search buffer as read"
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread)
  (elfeed-db-save))

;; Elfeed : rss reader
(use-package elfeed
  :config
  (elfeed-set-timeout 36000)
  :bind
  (:map elfeed-search-mode-map
        ("g" . perso/elfeed-load-db-and-update)
        ("G" . perso/elfeed-load-db-and-update-feeds)
        ("w" . (lambda () (interactive) (elfeed-db-save)))
        ("R" . perso/elfeed-mark-all-as-read)
        ("q" . perso/elfeed-save-db-and-bury))
  :custom
  (elfeed-use-curl t)
  (elfeed-log-level 'info)
  (elfeed-search-filter "@1-month-ago +unread -large +daily"))

;; Elfeed-org : org-mode feed file support
(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/rss.org")))

;; Org capture template "f": add a feed to the feed file
(when (featurep 'init-org)
  (defun perso/org-capture-feeds-file ()
    (concat org-directory "/rss.org"))
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 `("f" "add RSS feed"
                   entry (file+olp ,(perso/org-capture-feeds-file) "Feeds" "Unsorted")
                   "* [[%^{link-url}][%^{link-description}]]"
                   :immediate-finish nil
                   :empty-lines 1
                   :prepend nil)
                 t)))

(provide 'init-rss)
;;; init-rss.el ends here
