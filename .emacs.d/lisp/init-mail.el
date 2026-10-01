;;; init-mail.el --- Mail with mu4e -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-org.  Disabled with EMACS_NOMU=Y.

;;; Code:

;;; Mail management
;; Include protonmail-bridge cert
(require 'gnutls)
(add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge/cert.pem"))
;; Mail identities, from perso.el (a context whose address is not set is dropped)
(perso/defsetting protonmail-user-mail-address "Protonmail address, for the mu4e protonmail context")
(perso/defsetting protonmail-user-full-name "Name used with the Protonmail address")
(perso/defsetting gmail-user-mail-address "Gmail address, for the mu4e gmail context")
(perso/defsetting gmail-user-full-name "Name used with the Gmail address")

;; mu4e
(use-package mu4e
  ;; We want the version that comes with mu, not from melpa
  :ensure nil
  :commands (mu4e make-mu4e-context)
  :custom
  (mu4e-maildir (expand-file-name "~/.mail"))
  (mu4e-attachment-dir  "~/Downloads/")
  (mu4e-change-filenames-when-moving t) ; work better for mbsync
  (mu4e-get-mail-command "mbsync protonmail")
  (mu4e-view-show-addresses t)
  (mu4e-compose-dont-reply-to-self t)
  (message-kill-buffer-on-exit t)
  (mu4e-headers-auto-update t)
  (mu4e-headers-skip-duplicates t)
  ;; We will display manually with W if needed
  (mu4e-headers-include-related nil)
  (mu4e-view-show-images t)
  (mu4e-view-prefer-html t)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-precise-alignment t)
  (mu4e-headers-date-format "%d-%m-%Y")
  (mu4e-headers-fields
   '((:human-date . 13)
     (:mdir . 15)
     (:flags . 10)
     (:mailing-list . 10)
     (:from-or-to . 25)
     (:subject)))
  (mu4e-headers-results-limit -1)
  ;; index-cleanup and index-lazy-check are needed with mbsync/gmail
  (mu4e-index-cleanup t)
  (mu4e-index-lazy-check nil)
                                  ;  (mu4e-html2text-command "html2text -utf8 -width 72")
  ;; (mu4e-html2text-command "w3m -dump -T text/html")
  (mu4e-decryption-policy 'ask)
  (mu4e-context-policy 'pick-first)
  (mu4e-hide-index-messages t)
  (mu4e-completing-read-function 'completing-read)
  :config
  (setq mu4e-maildir-shortcuts
        '((:maildir "/protonmail/inbox"     :key  ?i)
          (:maildir "/protonmail/archive"   :key  ?a)
          (:maildir "/protonmail/drafts"     :key  ?d)
          (:maildir "/protonmail/sent"      :key  ?s)
          (:maildir "/gmail/inbox"     :key  ?I)
          (:maildir "/gmail/archive"   :key  ?A)
          (:maildir "/gmail/drafts"     :key  ?D)
          (:maildir "/gmail/sent"      :key  ?S)))
  (add-to-list 'mu4e-bookmarks
               '( :name "With attachment"
                  :query "mime:application/* AND NOT mime:application/pgp* AND NOT (maildir:/protonmail/trash OR maildir:/gmail/trash OR maildir:/protonmail/spam OR maildir:/gmail/spam)"
                  :key ?a))
  (add-to-list 'mu4e-bookmarks
               '( :name  "Focused"
                  :query "flag:flagged OR (flag:unread AND NOT flag:list AND NOT (maildir:/protonmail/sent OR maildir:/gmail/sent OR maildir:/protonmail/trash OR maildir:/gmail/trash OR maildir:/protonmail/spam OR maildir:/gmail/spam))"
                  :key ?f))
  ;; org-mode integration (not required anymore, it's default since 1.10)
  ;; (require 'org-mu4e)
  ;; This is bound globally later
  (unbind-key "C--" mu4e-headers-mode-map)
  ;; mu4e-action-view-in-browser is built into mu4e
  ;; by adding it to these lists of custom actions
  ;; it can be invoked by first pressing a, then selecting
  (add-to-list 'mu4e-headers-actions
               '("in browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("in browser" . mu4e-action-view-in-browser) t)

  ;; From https://etienne.depar.is/emacs.d/mu4e.html
  (defun ed/mu4e-view-go-to-private-url (&optional multi)
    "Offer to go to url(s) in a private window of Firefox.
If MULTI (prefix-argument) is nil, go to a single one, otherwise,
offer to go to a range of urls."
    (interactive "P")
    (mu4e~view-handle-urls
     "URL to visit" multi
     (lambda (url)
       (start-process
        "private-firefox" nil
        "firefox" "--private-window" url))))

  (define-key mu4e-view-mode-map "G" #'ed/mu4e-view-go-to-private-url)

  (add-to-list 'mu4e-header-info-custom
               '(:mdir .
                       ( :name "Shortend Maildir path"
                         :shortname "Maildir"
                         :help "Shows a collapsed maildir path"
                         :function (lambda (msg)
                                     (let ((maildir (or (mu4e-message-field msg :maildir) "")))
                                       (cond ((string-match-p "Archives/" maildir)
                                              (replace-regexp-in-string "^/\\(.\\).*/\\(.\\).*/\\(.*\\)" "\\1/\\2/\\3" maildir))
                                             ((string-match-p "Archives" maildir)
                                              (replace-regexp-in-string "^/\\(.\\).*/\\(.\\).*" "\\1/\\2" maildir))
                                             (t
                                              (replace-regexp-in-string "^/\\(.\\).*/\\(.*\\)" "\\1/\\2" maildir))))))))

  ;; Marking for deletion only move to trash folder
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (target msg)
                            (mu4e-get-trash-folder msg))
              :action (lambda (docid msg target)
                        ;; Here's the main difference to the regular trash mark,
                        ;; no +T before -N so the message is not marked as
                        ;; IMAP-deleted:
                        (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

  ;; Tag message
  (add-to-list 'mu4e-marks
               '(tag
                 :char       "g"
                 :prompt     "gtag"
                 :ask-target (lambda () (read-string "What tag do you want to add?"))
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg (concat "+" target)))))
  (mu4e~headers-defun-mark-for tag)
  (define-key mu4e-headers-mode-map (kbd "g") 'mu4e-headers-mark-for-tag)

  ;; Archive Gmail-style
  (add-to-list 'mu4e-marks
               '(archive
                 :char       "A"
                 :prompt     "Archive"
                 :show-target (lambda (target) mu4e-archive-folder)
                 :action      (lambda (docid msg target)
                                ;; must come before proc-move since retag runs
                                ;; 'sed' on the file
                                (mu4e-action-retag-message msg "-\\Inbox")
                                (mu4e--server-move docid mu4e-archive-folder "+S-u-N"))))
  (mu4e~headers-defun-mark-for archive)
  (define-key mu4e-headers-mode-map (kbd "A") 'mu4e-headers-mark-for-archive)

  ;; Mark as read and move to spam
  (add-to-list 'mu4e-marks
               '(spam
                 :char       "X"
                 :prompt     "Spam"
                 :show-target (lambda (target) mu4e-spam-folder)
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg "-\\Inbox")
                                (mu4e--server-move docid mu4e-spam-folder "+S-u-N"))))
  (mu4e~headers-defun-mark-for spam)
  (define-key mu4e-headers-mode-map (kbd "X") 'mu4e-headers-mark-for-spam)

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "protonmail"
          :enter-func (lambda () (mu4e-message "Entering context protonmail"))
          :leave-func (lambda () (mu4e-message "Leaving context protonmail"))
          :match-func
          (lambda (msg)
            (when msg
              (string-match "protonmail" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . ,protonmail-user-mail-address) ; in perso.el
                  (user-full-name . ,protonmail-user-full-name) ; in perso.el
                  (mu4e-sent-folder . "/protonmail/sent")
                  (mu4e-drafts-folder . "/protonmail/drafts")
                  (mu4e-trash-folder . "/protonmail/trash")
                  (mu4e-refile-folder. "/protonmail/archive")
                  (mu4e-archive-folder . "/protonmail/archive")
                  (mu4e-spam-folder . "/protonmail/spam")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-default-smtp-server . "127.0.0.1")
                  (smtpmail-smtp-server . "127.0.0.1")
                  (smtpmail-smtp-service . 1025)))
         (make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Entering context gmail"))
          :leave-func (lambda () (mu4e-message "Leaving context gmail"))
          :match-func
          (lambda (msg)
            (when msg
              (string-match "gmail" (mu4e-message-field msg :maildir))))
          :vars `((user-mail-address . ,gmail-user-mail-address) ; in perso.el
                  (user-full-name . ,gmail-user-full-name) ; in perso.el
                  (mu4e-sent-folder . "/gmail/sent")
                  (mu4e-drafts-folder . "/gmail/drafts")
                  (mu4e-trash-folder . "/gmail/trash")
                  (mu4e-refile-folder. "/gmail/archive")
                  (mu4e-archive-folder . "/gmail/archive")
                  (mu4e-spam-folder . "/gmail/spam")
                  (mu4e-sent-messages-behavior . delete) ; IMAP takes care of it
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-stream-type . starttls)
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  ))))
  ;; Drop the contexts whose address perso.el does not set
  (setq mu4e-contexts
        (seq-filter (lambda (context)
                      (alist-get 'user-mail-address (mu4e-context-vars context)))
                    mu4e-contexts))
  ;; Encryption settings
  (setq mml-secure-openpgp-sign-with-sender t
        mml-secure-openpgp-encrypt-to-self t)

  ;; From https://macowners.club/posts/mu4e-save-attachments-faster-with-ivy/#edits
  (defun timu/mu4e-view-save-attachments ()
    "Save All Attachements in a selected directory using completion.
This is a modified version of `mu4e-view-save-attachments'."
    (interactive)
    (cl-assert (and (eq major-mode 'mu4e-view-mode)
                    (derived-mode-p 'gnus-article-mode)))
    (let* ((parts (mu4e~view-gather-mime-parts))
           (handles '())
           (files '())
           dir)
      (dolist (part parts)
        (let ((fname (cdr (assoc 'filename (assoc "attachment" (cdr part))))))
          (when fname
            (push `(,fname . ,(cdr part)) handles)
            (push fname files))))
      (if files
          (progn
            (setq dir (read-directory-name "Save to directory: "))
            (cl-loop for (f . h) in handles
                     when (member f files)
                     do (mm-save-part-to-file h (expand-file-name f dir))))
        (mu4e-message "No attached files found"))))
  (define-key mu4e-view-mode-map ">" 'timu/mu4e-view-save-attachments)
  (defun timu/mu4e-view-save-attachment ()
    "Save one attachements in a selected directory using completion.
This is a modified version of `mu4e-view-save-attachments'."
    (interactive)
    (cl-assert (and (eq major-mode 'mu4e-view-mode)
                    (derived-mode-p 'gnus-article-mode)))
    (let* ((parts (mu4e~view-gather-mime-parts))
           (handles '())
           (files '())
           dir)
      (dolist (part parts)
        (let ((fname (cdr (assoc 'filename (assoc "attachment" (cdr part))))))
          (when fname
            (push `(,fname . ,(cdr part)) handles)
            (push fname files))))
      (if files
          (progn
            (setq files (completing-read-multiple "Save part(s): " files)
                  dir (read-directory-name "Save to directory: "))
            (cl-loop for (f . h) in handles
                     when (member f files)
                     do (mm-save-part-to-file h (expand-file-name f dir))))
        (mu4e-message "No attached files found"))))
  (define-key mu4e-view-mode-map "e" 'timu/mu4e-view-save-attachment)
  )

;; Default mail agent for emacs
(setq mail-user-agent 'mu4e-user-agent)

;; Org capture template "r": a task to answer the message shown in mu4e
(when (featurep 'init-org)
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 `("r" "new task: respond to email"
                   entry (file+headline ,(perso/org-capture-tasks-file) "Tâches rapides")
                   "* TODO Répondre à [[mailto:%:fromaddress][%:fromname]]\nSCHEDULED: %^t"
                   :immediate-finish t
                   :empty-lines 1
                   :prepend nil)
                 t)
    (add-to-list 'org-capture-templates-contexts
                 '("r" ((in-mode . "mu4e-view-mode"))))))

(provide 'init-mail)
;;; init-mail.el ends here
