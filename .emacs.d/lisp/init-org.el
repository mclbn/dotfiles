;;; init-org.el --- Org, agenda, capture, CalDAV, export -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-dev.  Disabled with EMACS_NOORG=Y.

;;; Code:

;;; Org-mode
;; Timezone used when exporting to iCalendar: CET on the work profile,
;; Europe/Paris on the personal one (the timezone of the CalDAV sync)
(setq org-icalendar-timezone
      (if (string= (getenv "EMACS_WORK") "Y") "CET" "Europe/Paris"))

;; Main package and settings
(use-package org
  :ensure t
  :defer t
  :custom
  (org-modules (quote
                (org-crypt
                 org-id
                 ol-info
                 org-habit
                 org-protocol)))
  (org-agenda-start-on-weekday 1)
  (org-hide-emphasis-markers t)
  (org-adapt-indentation nil)
  (org-startup-truncated nil)
  (org-startup-folded 'overview)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-cache t)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-todo-keywords
   '((sequence "TODO(t/!)" "NEXT(n/!)" "STARTED(s/!)" "WAITING(w@/!)" "SOMEDAY(f/!)" "|" "DONE(d/!)" "CANCELED(c/!)")))
  (org-todo-keyword-faces
   '(("NEXT" . (:foreground "IndianRed1" :weight bold))
     ("STARTED" . (:foreground "OrangeRed" :weight bold))
     ("WAITING" . (:foreground "coral" :weight bold))
     ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
     ("RATED" . (:foreground "Gold" :weight bold))
     ))
  (org-src-fontify-natively t)
                                        ;  (org-todo-repeat-to-state "TODO")
  (org-log-into-drawer "LOGBOOK")
  (org-log-done 'time)
  (org-log-reschedule 'time)
  (org-log-redeadline 'note)
  (org-log-note-headings '((done        . "CLOSING NOTE %t")
                           (state       . "State %-12s from %-12S %t")
                           (note        . "Note taken on %t")
                           (reschedule  . "Schedule changed on %t: %S -> %s")
                           (delschedule . "Not scheduled, was %S on %t")
                           (redeadline  . "Deadline changed on %t: %S -> %s")
                           (deldeadline . "Removed deadline, was %S on %t")
                           (refile      . "Refiled on %t")
                           (clock-out   . "")))
  (org-hierarchical-todo-statistics t)
  (org-tags-exclude-from-inheritance (quote ("crypt" "project")))
  (org-agenda-include-diary t)
  (org-habit-show-habits-only-for-today t)
  (org-deadline-warning-days 7)
  (org-reverse-note-order nil)
  (org-blank-before-new-entry (quote ((heading . auto)
                                      (plain-list-item . auto))))
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-catch-invisible-edits 'smart)
  (org-use-property-inheritance nil) ; for performance
  (org-cycle-separator-lines 2)
  (org-id-link-to-org-use-id t)
  (org-latex-src-block-backend 'listings)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-imenu-depth 3)
  (org-attach-method 'cp)
  :bind
  ("C-z a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-z o l" . org-toggle-link-display)
  (:map org-mode-map ("C-c C-M-l" . my/org-link-sync-description))
  :config
  ;; Unbind keys bound to buffer-move
  (unbind-key (kbd "<C-S-up>") org-mode-map)
  (unbind-key (kbd "<C-S-down>") org-mode-map)
  (unbind-key (kbd "<C-S-left>") org-mode-map)
  (unbind-key (kbd "<C-S-right>") org-mode-map)

  ;; gpg setup
  (when (executable-find "gpg")
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-crypt-key "511079E5FEC0BA66B53C9A625D01D510BEBDD2FF")
    (require 'epa-file)
    (epa-file-enable))

  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map
                          (kbd "C-c o a")
                          #'org-attach-dired-to-subtree)))

  ;; Sub-package setup
  ;; other config stuff
  (if (string= (getenv "EMACS_WORK") "Y")
      (progn
        (defun perso/org-work-files ()
          ;; nil when ~/org-work is missing: org's setup goes on
          (when (file-directory-p "~/org-work")
            (seq-filter
             (lambda (x) (not (string-match "/templates/" (file-name-directory x))))
             (directory-files-recursively
              "~/org-work" "\\.org\\'" nil
              (lambda (subdir) ;  don't descend into dot prefixed dirs
                (not (string-prefix-p "." (file-name-nondirectory subdir))))))))
        (setq org-directory "~/org-work")
        (setq org-agenda-files
              (mapcar (lambda (f) (expand-file-name f org-directory))
                      '("notes.org" "tasks.org")))
        (setq org-refile-targets
              `((nil :maxlevel . 9)
                (,(perso/org-work-files) :maxlevel . 9))))
    (progn
      (setq org-directory "~/org")
      (setq org-agenda-files
            (mapcar (lambda (f) (expand-file-name f org-directory))
                    '("perso.org" "work.org" "notes.org"
                      "cloudcal-perso.org" "cloudcal-work.org")))
      (setq org-refile-targets `((nil :maxlevel . 9)
                                 (("perso.org" "work.org" "notes.org") :maxlevel . 9)))))
  ;; Auto-save org buffer on refile
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))
  (require 'org-id)
  (require 'org-capture)
  (defun org-schedule-force-note ()
    "Call org-schedule but make sure it prompts for re-scheduling note."
    (interactive)
    (let ((org-log-reschedule "note"))
      (call-interactively 'org-schedule)))
  (define-key org-mode-map (kbd "C-c C-S-s") 'org-schedule-force-note)
  (defun org-deadline-force-note ()
    "Call org-deadline but make sure it prompts for re-deadlining note."
    (interactive)
    (let ((org-log-redeadline "note"))
      (call-interactively 'org-deadline)))
  (define-key org-mode-map (kbd "C-c C-S-d") 'org-deadline-force-note)
  (defun my-skip-unless-deadline ()
    "Skip trees that have no deadline"
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (re-search-forward "DEADLINE:" subtree-end t)
          nil          ; tag found, do not skip
        subtree-end))) ; tag not found, continue after end of subtree

  (setq org-agenda-custom-commands
        '(("c" . "My Custom Agendas")
          ("cu" "Unscheduled TODO"
           ((todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'todo '("SOMEDAY" "WAITING"))))))
           nil
           nil)
          ("cd" "Deadlines"
           ((todo ""
                  ((org-agenda-overriding-header "\nDeadlines")
                   (org-agenda-skip-function 'my-skip-unless-deadline)))))))

  ;; From https://github.com/alphapapa/unpackaged.el
  (defun org-fix-blank-lines (&optional prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries (lambda ()
                       (org-with-wide-buffer
                        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                        ;; newlines before the current heading, so we do this part widened.
                        (while (not (looking-back "\n\n" nil))
                          ;; Insert blank lines before heading.
                          (insert "\n")))
                       (let ((end (org-entry-end-position)))
                         ;; Insert blank lines before entry content
                         (forward-line)
                         (while (and (org-at-planning-p)
                                     (< (point) (point-max)))
                           ;; Skip planning lines
                           (forward-line))
                         (while (re-search-forward org-drawer-regexp end t)
                           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                           ;; for some reason it doesn't work correctly when operating on hidden text.
                           ;; This works, taken from `org-agenda-get-some-entry-text'.
                           (re-search-forward "^[ \t]*:END:.*\n?" end t)
                           (goto-char (match-end 0)))
                         (unless (or (= (point) (point-max))
                                     (org-at-heading-p)
                                     (looking-at-p "\n"))
                           (insert "\n"))))
                     t (if prefix
                           nil
                         'tree)))

  ;; Org-babel stuff here
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     (shell . t)
     (calc . t)
     (org . t)))
  (add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
  (setq org-babel-prompt-command "PROMPT_COMMAND=;PS1=\"org_babel_sh_prompt> \";PS2=")

  ;; Following functions from https://kitchingroup.cheme.cmu.edu/blog/2015/03/19/Restarting-org-babel-sessions-in-org-mode-more-effectively/
  (defun org-babel-kill-session ()
    "Kill session for current code block."
    (interactive)
    (unless (org-in-src-block-p)
      (error "You must be in a src-block to run this command"))
    (save-window-excursion
      (org-babel-switch-to-session)
      (kill-buffer)))

  (defun org-babel-remove-result-buffer ()
    "Remove results from every code block in buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (org-babel-remove-result))))

  ;; Set of function to get auto-preview of Org inline images
  ;; shortly after a link is typed or pasted
  (defun my/org-image--preview-region (beg end)
    "Add missing inline image/link previews between BEG and END."
    (if (fboundp 'org-link-preview-region)            ; Org 9.8+
        (org-link-preview-region nil nil beg end)
      (org-display-inline-images nil nil beg end)))    ; Org <= 9.7

  (defun my/org-image--enabled-p ()
    "Non-nil when image/link previews are currently shown in this buffer."
    (if (boundp 'org-link-preview-overlays)            ; Org 9.8+
        org-link-preview-overlays
      (bound-and-true-p org-inline-image-overlays)))    ; Org <= 9.7

  ;; debounced refresh
  (defvar-local my/org-image--timer nil)

  (defun my/org-image--refresh (buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (my/org-image--enabled-p)                ; do nothing if previews are off
          (with-demoted-errors "Org image auto-preview: %S"
            (my/org-image--preview-region (point-min) (point-max)))))))

  (defun my/org-image--schedule ()
    (when (timerp my/org-image--timer)
      (cancel-timer my/org-image--timer))
    (setq my/org-image--timer
          (run-with-idle-timer 1 nil #'my/org-image--refresh (current-buffer))))

  ;; trigger: after closing "]]", a yank, or an org link-insert command
  ;; If you paste with Evil or CUA rather than C-y, add your paste command
  ;; (e.g. evil-paste-after, evil-paste-before, cua-paste) to the memq list.
  ;; And if you insert links via electric-pair or a snippet that drops in
  ;; the ]] for you, the typed-]] branch won't fire (no ] is self-inserted),
  ;; but org-insert-link and the yank path still cover the common cases.
  (defun my/org-image--maybe-schedule ()
    (when (or (memq this-command
                    '(yank yank-pop org-yank
                           mouse-yank-primary mouse-yank-at-click
                           org-insert-link org-insert-all-links
                           org-insert-last-stored-link))
              (and (memq this-command '(self-insert-command org-self-insert-command))
                   (> (point) 2)
                   (eq last-command-event ?\])
                   (eq (char-before (1- (point))) ?\])))   ; just typed the 2nd ]
      (my/org-image--schedule)))

  (defun my/org-image-auto-preview-setup ()
    (add-hook 'post-command-hook #'my/org-image--maybe-schedule nil t))
  (add-hook 'org-mode-hook #'my/org-image-auto-preview-setup)

;;;  Refresh Org link descriptions from their target heading
  (defvar my/org-link-sync-types '("id" "custom-id")
    "Link types whose description may be re-derived from the target heading.
Deliberately excludes `fuzzy' ([[*Heading]]) links, which break outright
on rename, and all external types, which must never be opened just to
read a description.")

  (defun my/org-link--target-title (type path)
    "Return the current heading title for a TYPE:PATH Org link, or nil.
Resolves the target without opening it, so no browser is launched, no
window configuration changes and nothing is pushed onto the mark ring."
    (let ((pos (pcase type
                 ;; Drop any ::search suffix; the heading itself is what we want.
                 ("id" (org-id-find (car (split-string path "::")) 'marker))
                 ("custom-id" (org-find-property "CUSTOM_ID" path)))))
      (when pos
        (org-with-point-at pos
          ;; A file-level :ID: has no heading; refuse rather than guess.
          (unless (org-before-first-heading-p)
            (org-trim
             (replace-regexp-in-string
              "[ \t]+" " "
              (replace-regexp-in-string
               ;; Statistics cookies are volatile noise in a description.
               "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" ""
               ;; Flatten any links inside the heading to their own text.
               (org-link-display-format (org-get-heading t t t t))))))))))

  (defun my/org-link-sync-description ()
    "Rewrite the description of the Org link at point from its target heading."
    (interactive)
    (let ((el (org-element-context)))
      (unless (eq (org-element-type el) 'link)
        (user-error "Point is not on a link"))
      (let ((type (org-element-property :type el))
            (path (org-element-property :path el))
            (raw  (org-element-property :raw-link el)))
        (unless (member type my/org-link-sync-types)
          (user-error "Refusing to sync a %S link" type))
        ;; Resolve first: on failure the buffer is left untouched.
        (let ((title (my/org-link--target-title type path))
              (beg (org-element-property :begin el))
              ;; :end includes trailing whitespace; :post-blank counts it.
              (end (- (org-element-property :end el)
                      (or (org-element-property :post-blank el) 0))))
          (unless (org-string-nw-p title)
            (user-error "No target heading for %s" raw))
          (goto-char beg)
          (delete-region beg end)
          (insert (org-link-make-string raw title))
          (message "%s" title)))))

  (defun my/org-sync-all-link-descriptions ()
    "Refresh descriptions of every id:/custom-id link in the buffer.
Positions are collected first and replaced back-to-front so earlier
edits cannot shift later ones."
    (interactive)
    (let ((positions (org-element-map (org-element-parse-buffer) 'link
                       (lambda (l)
                         (when (member (org-element-property :type l)
                                       my/org-link-sync-types)
                           (org-element-property :begin l)))))
          (done 0) (skipped 0))
      (org-save-outline-visibility t
        (save-excursion
          (dolist (pos (nreverse positions))
            (goto-char pos)
            (condition-case err
                (progn (my/org-link-sync-description) (setq done (1+ done)))
              (user-error
               (setq skipped (1+ skipped))
               (message "skipped @%d: %s" pos (error-message-string err)))))))
      (message "%d updated, %d skipped" done skipped))))

(use-package org-indent
  ;; No need to get it, comes with emacs/org
  :ensure nil
  :diminish)

(use-package org-capture
  ;; No need to get it, comes with emacs/org
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config

  (defun perso/org-capture-notes-file ()
    (concat org-directory "/notes.org"))

  (defun perso/org-capture-tasks-file ()
    (if (string= (getenv "EMACS_WORK") "Y")
        (concat org-directory "/tasks.org")
      (concat org-directory "/perso.org")))

  (defun perso/org-capture-bookmarks-file ()
    (concat org-directory "/bookmarks.org"))

  (defun perso/org-capture-junior-file ()
    (concat org-directory "/junior.org"))

  (setq org-default-notes-file (perso/org-capture-notes-file))
  (setq org-capture-templates
        `(
          ("n" "take a quick note"
           entry (file+headline ,(perso/org-capture-notes-file) "À classer")
           "* %?"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("l" "take note with context"
           entry (file+headline  ,(perso/org-capture-notes-file) "À classer")
           "* %?\n%a"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("x" "take note with clipboard"
           entry (file+headline ,(perso/org-capture-notes-file) "À classer")
           "* %?\n%x"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("s" "take note with selection"
           entry (file+headline ,(perso/org-capture-notes-file) "À classer")
           "* %?\n%i"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("t" "add simple task"
           entry (file+headline ,(perso/org-capture-tasks-file) "Tâches rapides")
           "* TODO %?"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("c" "add task with context"
           entry (file+headline ,(perso/org-capture-tasks-file) "Tâches rapides")
           "* TODO %?\n%a"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("b" "add bookmark"
           entry (file+olp ,(perso/org-capture-bookmarks-file) "Web bookmarks" "Unsorted")
           "* [[%^{link-url}][%^{link-description}]] %^g"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("j" "add simple Junior task"
           entry (file+headline ,(perso/org-capture-junior-file) "Divers")
           "* TODO %?"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil)

          ("m" "meeting notes"
           entry (file+headline  ,(perso/org-capture-notes-file) "À classer")
           "* %? %U"
           :immediate-finish nil
           :clock-in t
           :clock-resume t
           :empty-lines 1
           :prepend nil)

          ("p" "plan meeting"
           entry (file+headline  ,(perso/org-capture-notes-file) "À classer")
           "* %?\nSCHEDULED: %^T"
           :immediate-finish nil
           :empty-lines 1
           :prepend nil))))

;; Org-caldav : caldav sync
;; Only for personal stuff
(if (not (string= (getenv "EMACS_WORK") "Y"))
    (progn
      (perso/defsetting cloud-caldav-url "CalDAV server URL, for org-caldav")
      (use-package org-caldav
		:if cloud-caldav-url
		:init
		(setq org-caldav-url cloud-caldav-url)
		(setq org-caldav-calendars
			  '((:calendar-id "org-perso"
					          :sync-direction twoway
					          :files ("~/org/perso.org")
					          :inbox "~/org/cloudcal-perso.org")
			    (:calendar-id "org-work"
					          :sync-direction twoway
					          :files ("~/org/work.org")
					          :inbox "~/org/cloudcal-work.org")))
		:config
		;; Need this to avoid breaking on tel: links
		(setq org-export-with-broken-links t)
		(setq org-icalendar-alarm-time 1)
		;; This makes sure to-do items as a category can show up on the calendar
		(setq org-icalendar-include-todo t)
        (setq org-caldav-todo-percent-states
              '((0 "TODO") (1 "SOMEDAY") (5 "NEXT") (10 "STARTED") (30 "WAITING") (100 "DONE")))
		;; Deadline disabled because it creates duplicates entry when used also schedueled
		;; See: https://github.com/dengste/org-caldav/issues/121
		;; This ensures all org "deadlines" show up, and show up as due dates
		;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
		(setq org-icalendar-use-deadline '(nil))
		;; This ensures "scheduled" org items show up, and show up as start times
		(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo)))))

;; Org-superstar : beautify org-mode
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars t)
  (org-superstar-remove-leading-stars t)
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(
                                     ("TODO" . ?☐)
                                     ("TOWATCH" . ?☐)
                                     ("NEXT" . ?▻)
                                     ("STARTED" . ?►)
                                     ("WAITING" . ?…)
                                     ("SOMEDAY" . ?∞)
                                     ("DONE" . ?☑)
                                     ("WATCHED" . ?☑)
                                     ("CANCELED" . ?☒)
                                     ("RATED" . ?★)
                                     )))

;; Org-download : paste images to org, we only use it for screenshots
(use-package org-download
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :custom
  (org-download-method 'attach)
  (org-download-display-inline-images 'posframe)
  (org-download-timestamp "%Y-%m-%d_%H-%M-%S")
  :config
  (defun org-download-file-format-custom (filename)
    "It's affected by `org-download-timestamp'."
    (concat (format-time-string org-download-timestamp) "." (file-name-extension filename)))
  ;; (setq org-download-annotate-function (lambda (link) (format "#+DOWNLOADED: %s" (format-time-string "%Y-%m-%d %H:%M:%S\n"))))
  (setq org-download-annotate-function (lambda (link) ""))
  (setq org-download-file-format-function #'org-download-file-format-custom)
  (if (eq system-type 'windows-nt)
      (setq org-download-screenshot-method "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
    (setq org-download-screenshot-method "flameshot gui --raw > %s"))
  (setq org-download-posframe-show-params
        '(
          :timeout 2
          :internal-border-width 1
          :internal-border-color "#7F9F7F"
          :min-width 40
          :min-height 10
          :poshandler posframe-poshandler-window-center))
  :hook
  ((dired-mode . org-download-enable)
   (org-mode . org-download-enable)
   (org-mode . (lambda ()
                      (local-set-key (kbd "C-c y") '(lambda ()
                                                      (interactive)
                                                      (org-download-clipboard)
                                                      (org-download-rename-last-file)))))
   (org-mode . (lambda ()
                      (local-set-key (kbd "C-c x") '(lambda ()
                                                      (interactive)
                                                      (org-download-screenshot)))))))

(use-package org-appear
  :custom
  (org-appear-delay 0.2)
  :hook
  org-mode)

;; OX-pandoc : export org via pandoc
(use-package ox-pandoc)

(provide 'init-org)
;;; init-org.el ends here
