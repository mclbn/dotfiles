;;; init-core.el --- Startup basics, backups, history, help -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded first, right after package.el and use-package are set up in init.el.

;;; Code:

;; Various performance tweaks
;;; We don't care about right-to-left typing
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
;;; Wait for end of typing before fontification
(setq redisplay-skip-fontification-on-input t)
;;; Increase process output buffer, lsp will benefit from this
(setq read-process-output-max (* 4 1024 1024))
;;; We don't want to ping unknown hostnames with find-file-at-point
(setq ffap-machine-p-local 'accept)
(setq ffap-machine-p-known 'accept)
(setq ffap-machine-p-unknown 'reject)

(defun perso/packages-update ()
  "Update all packages and recompile them, logging progress to a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Package Update*")))
    (with-current-buffer buf
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert "Starting package update at " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n")))
    (pop-to-buffer buf)
    (let ((log-func (lambda (msg)
                      (with-current-buffer buf
                        (let ((buffer-read-only nil))
                          (goto-char (point-max))
                          (insert msg "\n")
                          (when-let* ((win (get-buffer-window buf t)))
                            (set-window-point win (point-max))))
                        (redisplay)))))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args)
                   (when args
                     (let ((msg (apply #'format-message args)))
                       (funcall log-func msg))))))
        (condition-case err
            (progn
              (funcall log-func "Upgrading packages...")
              (package-upgrade-all)
              (funcall log-func "Upgrading VC packages...")
              (package-vc-upgrade-all)
              (funcall log-func "Recompiling packages...")
              (package-recompile-all))
          (error
           (funcall log-func (format "ERROR: %s" (error-message-string err))))))
      (with-current-buffer buf
        (let ((buffer-read-only nil))
          (goto-char (point-max))
          (insert "\nUpdate finished at " (format-time-string "%Y-%m-%d %H:%M:%S") "\n"))))))

;;; Early packages
;; Use Garbage collector magic hack ASAP
(use-package gcmh
  :diminish
  :demand t
  :config
  (gcmh-mode 1))

;; Diminish : reduces info about modes in bottom bar
(use-package diminish
  :hook ((auto-revert-mode . my/diminish-auto-revert)
         (hs-minor-mode . my/diminish-hideshow))
  :config
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode)
  (defun my/diminish-auto-revert () (diminish 'auto-revert-mode ""))
  (defun my/diminish-hideshow ()    (diminish 'hs-minor-mode "")))

;;; Unbinding unneeded keys that will be bound by upcoming packages
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-x C-t") nil)
(global-set-key (kbd "C-x l") nil)

;;; Absolute must-have tweaks and settings
;; Disable the welcome message
(setq inhibit-startup-message t)

;; No scratch message
(setq initial-scratch-message nil)

;; Default mode is text
(setq initial-major-mode 'text-mode)

;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Yes to recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; Calendar weeks start on monday
(setq calendar-week-start-day 1)

;; Calendar timezones
(set-time-zone-rule "CET")

;; Disable bells
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Kill-ring tweaks
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

;; So Long mitigates slowness due to extremely long lines.
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Enable mouse support
(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

;;; Backups & history
;; Backup files location and versioning
(defvar --backup-directory (concat user-emacs-directory "backups"))
(defvar --auto-save-directory (concat user-emacs-directory "auto-save/"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(if (not (file-exists-p --auto-save-directory))
    (make-directory --auto-save-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq auto-save-file-name-transforms `((".*" ,--auto-save-directory t)))
(setq make-backup-files t    ; backup of a file the first time it is saved.
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
      )

;; Set history-length longer
(setq-default history-length 1000)

;; When buffer is closed, saves the cursor location
(defun save-place-reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer 0 nil
                  (lambda (buf)
                    (when (buffer-live-p buf)
                      (dolist (win (get-buffer-window-list buf nil t))
                        (with-selected-window win (recenter)))))
                  (current-buffer)))

;; Custom setup for auto-saving scratch buffers
(require 'subr-x) ; string-trim

(defgroup scratch-autosave nil
  "Automatically save the *scratch* buffer to a log folder."
  :group 'convenience
  :prefix "scratch-autosave-")

(defcustom scratch-autosave-dir "~/log/scratch/"
  "Directory where *scratch* snapshots are written.
It is created automatically, together with any missing parent
directories, the first time a snapshot is saved."
  :type 'directory
  :group 'scratch-autosave)

(defcustom scratch-autosave-idle-delay 5
  "Number of seconds Emacs must be idle before *scratch* is saved.
Changing this after activation requires calling
`scratch-autosave-setup' again (or restarting Emacs)."
  :type 'number
  :group 'scratch-autosave)

(defvar scratch-autosave--timer nil
  "The idle timer object, or nil when autosaving is disabled.")

(defvar-local scratch-autosave--file nil
  "Absolute path this scratch buffer is saved to.
Set on the first save and reused for the life of the buffer, so a
killed-and-recreated *scratch* gets a fresh file.")

(defvar-local scratch-autosave--last-content nil
  "Buffer contents at the last successful save, for change detection.")

(defun scratch-autosave--empty-p (content)
  "Return non-nil when CONTENT is not worth saving.
That means it is empty, only whitespace, or still equal to the
default `initial-scratch-message'."
  (let ((trimmed (string-trim content)))
    (or (string= "" trimmed)
        (and initial-scratch-message
             (string= trimmed (string-trim initial-scratch-message))))))

(defun scratch-autosave--make-filename ()
  "Return the absolute path for a new scratch session.
The name is the local time down to the second plus this Emacs's
PID, which keeps concurrent Emacs instances from colliding on the
same second."
  (expand-file-name
   (format "%s-%d.scratch"
           (format-time-string "%Y-%m-%d_%H-%M-%S")
           (emacs-pid))
   scratch-autosave-dir))

(defun scratch-autosave--maybe-save ()
  "Save *scratch* to its log file if it is non-empty and has changed.
Designed to be called from an idle timer and from
`kill-emacs-hook'.  It never signals: any error is reported to
*Messages* and swallowed, so it can neither break the repeating
timer nor block Emacs from exiting."
  (condition-case err
      (let ((buf (get-buffer "*scratch*")))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((content (save-restriction
                             (widen)
                             (buffer-substring-no-properties
                              (point-min) (point-max)))))
              (when (and (not (scratch-autosave--empty-p content))
                         (not (equal content scratch-autosave--last-content)))
                (unless scratch-autosave--file
                  (setq scratch-autosave--file (scratch-autosave--make-filename)))
                (make-directory (file-name-directory scratch-autosave--file) t)
                (let ((coding-system-for-write 'utf-8))
                  (write-region content nil scratch-autosave--file nil 'silent))
                (setq scratch-autosave--last-content content))))))
    (error
     (message "scratch-autosave: %s" (error-message-string err)))))

(defun scratch-autosave-setup ()
  "Enable idle autosaving of the *scratch* buffer.
Safe to call more than once: it cancels any previous timer first,
so re-evaluating this file will not stack duplicate timers."
  (interactive)
  (when (timerp scratch-autosave--timer)
    (cancel-timer scratch-autosave--timer))
  (setq scratch-autosave--timer
        (run-with-idle-timer scratch-autosave-idle-delay t
                             #'scratch-autosave--maybe-save))
  (add-hook 'kill-emacs-hook #'scratch-autosave--maybe-save))

(defun scratch-autosave-teardown ()
  "Disable idle autosaving of the *scratch* buffer.
Existing saved files are left untouched."
  (interactive)
  (when (timerp scratch-autosave--timer)
    (cancel-timer scratch-autosave--timer))
  (setq scratch-autosave--timer nil)
  (remove-hook 'kill-emacs-hook #'scratch-autosave--maybe-save))

(scratch-autosave-setup)

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :config
  (add-hook 'find-file-hook 'save-place-reposition t))

;; Recentf : recent files history
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-menu-items 20480)
  (recentf-max-saved-items 20480)
  (recentf-auto-cleanup 'never)
  ;; (recentf-exclude '((expand-file-name package-user-dir)
  ;;                    ".cache"
  ;;                    (expand-file-name (concat user-emacs-directory "bookmarks"))
  ;;                    (expand-file-name (concat user-emacs-directory "recentf"))
  ;;                    ;; org archive? (.org_archive)
  ;;                    "COMMIT_EDITMSG\\'"))

  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ".cache"
                     ,(expand-file-name "bookmarks" user-emacs-directory)
                     ,(expand-file-name "recentf" user-emacs-directory)
                     "COMMIT_EDITMSG\\'"))
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; Savehist : minibuffer history
(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

;;; Help
;; We want to switch to the help window when opening it
(setq help-window-select t)

;; Helpful: help menu replacement
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; Which-key : displays next possible keys
(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  (which-key-idle-delay 1.0)
  (which-key-side-window-max-width 0.33)
  (which-key-side-window-max-height 0.33)
  :config
  ;; X11 emacs is usually full-screen on widescreen
  (if (display-graphic-p)
      (which-key-setup-side-window-right))
  (which-key-mode))

;; Emacs-websearch
;; looking up stuff on the Internet
(use-package emacs-websearch
  :vc (:url "https://github.com/zhenhua-wang/emacs-websearch" :branch master :rev :newest)
  :bind ("C-z C-w" . emacs-websearch)
  :config (setq emacs-websearch-engine 'duckduckgo))

;;; Convenience key-binding for common actions
;; Quick access to scratch
(defun switch-to-scratch-buffer ()
  "Switch to the current session's scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(bind-key "C-z s" #'switch-to-scratch-buffer)

;; Quick access to init.el
(defun open-init-file ()
  "Open init file"
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))
(bind-key "C-z C-e" #'open-init-file)

;; Quick access to manpages
(global-set-key (kbd "C-z m") 'woman) ; Man pages

(provide 'init-core)
;;; init-core.el ends here
