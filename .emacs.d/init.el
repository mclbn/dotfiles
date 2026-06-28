;;; Init.el --- -*- lexical-binding: t -*-
;;; Emacs startup configuration file
;;; Stuff to explore later
;; Maybe something to automatically add headers
;; Custom C / C++ style definition
;; Org-mode
;; - Disable unused modules to speedup startup ?
;; - org-super-agenda
;; - org-fancy-priorities
;; General startup speed optimizations
;; Properly configure Web mode
;; Visual-regexp (https://github.com/benma/visual-regexp.el)
;; Have a look at Perspective (https://github.com/nex3/perspective-el and https://alhassy.github.io/emacs.d/#Having-a-workspace-manager-in-Emacs)
;; Removed color-rg, could try again if needed
;; Combobulate (use treesitter to manipulate code) :
;; https://www.masteringemacs.org/article/combobulate-structured-movement-editing-treesitter

;; Disabling native-compilation warnings
(setq native-comp-async-report-warnings-errors nil)

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

;;; Personal information is stored in a non-versioned file
(defvar personal-info (concat user-emacs-directory "perso.el"))
(let ((personal-settings personal-info))
  (when (file-exists-p personal-settings)
    (load-file personal-settings))
  )

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;;; Configuration for package.el
(require 'package)
;; Repositories
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Repositories priority
(setq package-archive-priorities
      '(("melpa" . 10)
	    ("nongnu" . 7)
	    ("elpa" . 5)
        ("melpa-stable"        . 3)))
;; Activating package
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))
;; Install and configure use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  ;; flip to t to run M-x use-package-report
  (setq use-package-compute-statistics nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package quelpa
  :ensure t
  :init
  (setq quelpa-update-melpa-p nil) ; no auto update (faster startup)
  )

(use-package  quelpa-use-package
  :ensure t)

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
                          (when-let ((win (get-buffer-window buf t)))
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
(setq org-icalendar-timezone "CET")

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
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; Crux : custom functions
(use-package crux
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :bind
  (("C-z C-d" . crux-delete-file-and-buffer)
   ("C-z C-n" . crux-rename-file-and-buffer)
   ("C-c C-." . crux-duplicate-current-line-or-region)
   ("C-c C-M-." . crux-duplicate-and-comment-current-line-or-region)
   ("C-k" . crux-smart-kill-line)
   ("C-c C-k" . crux-kill-whole-line)
   ("C-g" . crux-keyboard-quit-dwim)
   ))

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

;;; File manipulation
;; Dired : directory browsing
(use-package dired
  :ensure nil
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lahp --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify t)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  ;; Probe ls for capabilities
  (dired-use-ls-dired 'unspecified)
  (dired-omit-files "^\\...+$\\|\\`[.]?#\\|\\`[.][.]?\\'")
  :config
  ;; We don't use ugly listings
  (global-set-key (kbd "C-x C-d") nil)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  ;; open with external application
  (defun dired-open-external ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (define-key dired-mode-map (kbd "C-<return>") #'dired-open-external)
  (define-key dired-mode-map (kbd ";") #'dired-hide-details-mode)
  ;;   (defun open-in-external-app ()
  ;;     "Open the file where point is or the marked files in Dired in external
  ;; app. The app is chosen from your OS's preference."
  ;;     (interactive)
  ;;     (let* ((file-list
  ;;             (dired-get-marked-files)))
  ;;       (mapc
  ;;        (lambda (file-path)
  ;;          (let ((process-connection-type nil))
  ;;            (start-process "" nil "xdg-open" (shell-quote-argument file-path)))) file-list)))
  :bind
  (("C-x C-d" . (lambda () (interactive)(dired "~/")))
   ("C-x d" . dired-jump))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-async-mode)
  ;; (dired-mode . dired-hide-details-mode)
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-file)
                  (local-set-key (kbd "M-RET") #'dired-find-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "M-SPC") #'dired-view-file)
                  (local-set-key (kbd "M-<up>")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

(use-package diredfl
  :after zenburn-theme
  :custom-face
  (diredfl-dir-name ((t (:foreground "#94BFF3" :background "#3F3F3F" :weight bold))))
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :custom
  (dgi-auto-hide-details-p nil)
  :bind (:map dired-mode-map (":" . dired-git-info-mode))
  )

(use-package dired-recent
  :custom
  (dired-recent-max-directories nil)
  :config
  (dired-recent-mode 1))

(use-package nerd-icons-dired
  :init
  (defun my/dired-subtree-add-nerd-icons ()
    "Add nerd icons into subtree."
    (interactive)
    (revert-buffer))

  (defun my/dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
      (if nerd-icons-dired-mode
          (advice-add #'dired-subtree-toggle :after #'my/dired-subtree-add-nerd-icons)
        (advice-remove #'dired-subtree-toggle #'my/dired-subtree-add-nerd-icons))))
  :hook
  (dired-mode . nerd-icons-dired-mode)
  (nerd-icons-dired-mode . my/dired-subtree-toggle-nerd-icons))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

;; Image-mode
(use-package image-mode
  :ensure nil
  :config
  (bind-keys :map image-mode-map
             ("<up>" . image-previous-file)
             ("<down>" . image-next-file)
             ("<left>" . image-previous-file)
             ("<right>" . image-next-file)
             ("C-<left>" . image-backward-hscroll)
             ("C-<right>" . image-forward-hscroll)))

(use-package doc-view
  :ensure nil
  :config
  (bind-keys :map doc-view-mode-map
             ("<up>" . doc-view-previous-page)
             ("<down>" . doc-view-next-page)
             ("<left>" . doc-view-previous-page)
             ("<right>" . doc-view-next-page)))

;; Shell : Inferior shell mode
(use-package shell
  :custom
  (comint-process-echoes nil)
  :config
  (when (executable-find "zsh")
    (setq explicit-shell-file-name (executable-find "zsh"))
    (setq explicit-zsh-args '("--interactive"))))

;; Treemacs : visual tree
(use-package treemacs
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :ensure t
  :defer t
  :bind
  ("C-z C-t" . treemacs)
  (:map treemacs-mode-map ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (treemacs-project-follow-mode t))

(use-package treemacs-all-the-icons
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; TRAMP
(use-package tramp
  :custom
  (tramp-verbose 3)
  (tramp-default-method "ssh")
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024))
  :init
  (use-package ibuffer-tramp)
  :config
  (setq password-cache-expiry 600)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

;; Imenu
(use-package imenu
  :custom
  (imenu-auto-rescan t))

;;; "Main" packages that provide major features
;; Prescient : sorting and predicting algorithm
(use-package prescient
  :demand t
  :custom
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1)
  )

;; Savehist : minibuffer history
(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

;; Vertico : vertical completion UI built on the native completing-read
(use-package vertico
  :demand t
  :custom
  (vertico-count 10)
  (vertico-cycle t)
  :init
  (vertico-mode 1))

;; Orderless : space-separated, any-order matching
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  ;;  `basic' must come first for the `file' category, or remote (/ssh:)
  ;; host/user completion and dynamic tables break under orderless.
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Vertico-prescient : keep Prescient frecency sorting, let Orderless filter
(use-package vertico-prescient
  :after (vertico prescient)
  :demand t
  :custom
  (vertico-prescient-enable-filtering nil) ; keep Orderless as the filter
  (vertico-prescient-enable-sorting t)
  :config
  (vertico-prescient-mode 1))

;; Hide commands irrelevant to the current mode from M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Consult : search/navigation commands (replaces Swiper + the counsel-* commands)
(use-package consult
  :bind
  (("C-s"     . consult-line)
   ("C-z r"   . consult-recent-file)
   ("C-z b"   . consult-buffer)
   ("C-z C-b" . consult-project-buffer)
   ("C-z l"   . consult-locate)
   ("C-z SPC" . consult-mark)
   ("C-z i"   . consult-imenu)
   ("M-y" . consult-yank-from-kill-ring)
   ([remap goto-line] . consult-goto-line))
  :config
  ;; TRAMP: debounce file/buffer preview so moving the selection does
  ;; not eagerly open remote files on every keystroke.
  (consult-customize
   consult-buffer consult-project-buffer
   :preview-key '(:debounce 0.2 any)
   consult-recent-file :preview-key nil)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-dir
  :ensure t
  :bind (("C-z d" . consult-dir)
         :map vertico-map
         ("C-z d" . consult-dir)))

(use-package consult-tramp
  :quelpa (consult-tramp :repo "Ladicle/consult-tramp" :fetcher github :commit "main")
  :bind ("C-x C-t" . consult-tramp))

;; Embark : act on the thing at point or the current completion candidate
(use-package embark
  :bind
  (("C-z ." . embark-act)
   ("C-z /" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; routes prefix help through which-key
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-Consult : export to grep/dired/ibuffer, preview in collect buffers
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Marginalia : rich annotations (docs, file info, sizes) beside candidates
(use-package marginalia
  :after vertico
  :demand t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

;; Nerd icons for completion & marginalia
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Project.el : project management
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '(".projectile"
     "Makefile"
     "CMakeLists.txt"
     "compile_commands.json"
     "configure.ac"
     "Cargo.toml"
     "pom.xml"
     "Gemfile"
     "composer.json"
     "package.json"
     "pyproject.toml"
     "platformio.ini")))

(defvar perso/project-compile-history (make-hash-table :test 'equal)
  "Per-project memory of the last `compile' command, keyed by project root.")

(defun perso/project-compile ()
  "Compile from the project root, remembering the command per project.
Behaves like `projectile-compile-project': the last command used for
this project is offered as the default."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (compile-command (or (gethash root perso/project-compile-history)
                              compile-command))
         (cmd (compilation-read-command compile-command)))
    (puthash root cmd perso/project-compile-history)
    (compile cmd)))

;; Magit : Git interface
(use-package magit
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  (magit-status-show-untracked-files 'all)
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))

;; Rg : ripgrep search
(use-package rg
  :if (executable-find "rg")
  :quelpa (rg :repo "dajva/rg.el" :fetcher github :commit "master")
  :config
  (defun perso/rg ()
    (interactive)
    (call-interactively #'rg-menu)
    (add-to-list
     'display-buffer-alist
     '("\\*rg\\*" . (nil . ((body-function . select-window))))))
  (bind-key "C-z C-r" #'perso/rg))

;; Wgrep : write modified files in grep buffers
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; Buffer and window management
(setq-default cursor-in-non-selected-windows t)
(setq highlight-nonselected-windows t)

;; Manual split provide balanced layout
(setq window-combination-resize t)

;; Custom functions to center text by changing margins
(defvar global-centered-text nil "Global centered text status.")

(defvar center-text-min-space 100
  "Minimum text space between margins.")

(defvar center-text-max-space 120
  "Maximum text space between margins.")

(defvar center-text-margin-ratio 6
  "Window /ratio to try to achieve for each margin.")

(defun center-text (&optional max-size)
  "Center the text in the middle of the buffer."
  (interactive)
  (progn
    (if max-size
        (progn
          (setq-local local-buffer-max-space max-size)
          (if (< max-size center-text-min-space)
              (setq-local local-buffer-min-space max-size)
            (setq-local local-buffer-min-space center-text-min-space))))
    (if (not (local-variable-p 'local-buffer-max-space))
        (setq-local local-buffer-max-space center-text-max-space))
    (if (not (local-variable-p 'local-buffer-min-space))
        (setq-local local-buffer-min-space center-text-min-space))
    (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        nil
                        nil)
    (setq-local centered t)
    (if (>= (window-width) center-text-min-space)
        (progn
          (setq-local min-margin (/ (- (window-width) local-buffer-max-space) 2))
          (setq-local max-margin (/ (- (window-width) local-buffer-min-space) 2))
          (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                              (min (max (/ (window-width) center-text-margin-ratio) min-margin) max-margin)
                              (min (max (/ (window-width) center-text-margin-ratio) min-margin) max-margin))))))

(defun center-text-clear ()
  "Clear any margin settings."
  (interactive)
  (if global-centered-text (message "Global centered mode still active."))
  (if (local-variable-p 'centered)
      (progn
        (setq-local centered nil)
        (setq-local local-buffer-min-space center-text-min-space)
        (setq-local local-buffer-max-space center-text-max-space)
        (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                            nil
                            nil))))

(defun refresh-center-text ()
  "Refresh margins (should be hooked)."
  (interactive)
  (if (local-variable-p 'centered)
      (if centered
          (center-text)
        (center-text-clear))))

(defun toggle-center-text ()
  "Toggle centered text."
  (interactive)
  (if (local-variable-p 'centered)
      (if centered
          (center-text-clear)
        (center-text current-prefix-arg))
    (center-text current-prefix-arg)))

(defun center-text-all-buffers ()
  "Center text on all buffers."
  (interactive)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (center-text)))
        (buffer-list)))

(defun center-text-clear-all-buffers ()
  "Clear any margins on all buffers."
  (interactive)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (center-text-clear)))
        (buffer-list)))

(defun enable-center-text-globally ()
  "Enable centered text mode globally."
  (interactive)
  (remove-hook 'window-configuration-change-hook 'center-text-clear)
  (add-hook 'window-configuration-change-hook 'center-text)
  (setq global-centered-text t)
  (center-text-all-buffers))

(defun disable-center-text-globally ()
  "Disable centered text mode globally."
  (interactive)
  (remove-hook 'window-configuration-change-hook 'center-text)
  (setq global-centered-text nil)
  (center-text-clear-all-buffers))

(defun toggle-center-text-globally ()
  "Toggle global centered text mode globally."
  (interactive)
  (if global-centered-text
      (disable-center-text-globally)
    (enable-center-text-globally)))

(add-hook 'window-configuration-change-hook 'refresh-center-text)
(define-key global-map (kbd "C-z C") 'toggle-center-text)
(define-key global-map (kbd "C-z C-C") 'toggle-center-text-globally)

;; Custom function to quickly switch window and text centering setup
(defun perso/1-window-mode ()
  "Switch to 1-window mode and disable text centering everywhere."
  (interactive)
  (disable-center-text-globally)
  (delete-other-windows))
(bind-key "C-c 0" #'perso/1-window-mode)

(defun perso/1-window-centered-mode ()
  "Switch to 1-window mode and enable text centering everywhere."
  (interactive)
  (enable-center-text-globally)
  (delete-other-windows)
  (center-text))
(bind-key "C-c 1" #'perso/1-window-centered-mode)

(defun perso/2-windows-mode ()
  "Switch to 2-windows mode, disable centering everywhere and move cursor to the right one."
  (interactive)
  (disable-center-text-globally)
  (delete-other-windows)
  (split-window-right)
  (balance-windows)
  (windmove-right))
(bind-key "C-c 2" #'perso/2-windows-mode)

(defun perso/3-windows-mode ()
  "Switch to 3-windows mode, disable centering everywhere and move cursor to the right one."
  (interactive)
  (disable-center-text-globally)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows)
  (windmove-right))
(bind-key "C-c 3" #'perso/3-windows-mode)

(defun perso/4-windows-mode ()
  "Switch to 4-windows mode, disable centering everywhere and move cursor to the right one."
  (interactive)
  (disable-center-text-globally)
  (delete-other-windows)
  (split-window-below)
  (split-window-right)
  (windmove-down)
  (split-window-right)
  (balance-windows))
(bind-key "C-c 4" #'perso/4-windows-mode)

;; move the cursor when a new window is created
(defun mm/split-window-right-and-follow ()
  "A function to create a window on the right and move the cursor to it"
  (interactive)
  (select-window (split-window-right)))
(global-set-key (kbd "C-x 3") 'mm/split-window-right-and-follow)

(defun mm/split-window-below-and-follow ()
  "A function to create a window below and move the cursor to it"
  (interactive)
  (select-window (split-window-below)))
(global-set-key (kbd "C-x 2") 'mm/split-window-below-and-follow)

(bind-key "C-x <up>" #'windmove-up)
(bind-key "C-x <down>" #'windmove-down)
(bind-key "C-x <left>" #'windmove-left)
(bind-key "C-x <right>" #'windmove-right)
(bind-key "C-c q" #'delete-window)
(bind-key "C-x q" #'kill-buffer-and-window)

;; Ace-window : window selection & management
(use-package ace-window
  :bind ("C-x C-o" . ace-window))

;; Buffer-move : swap buffer positions
(use-package buffer-move
  :custom (buffer-move-stay-after-swap t)
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

;; Ibuffer : buffer management and sorting
(use-package ibuffer
  :ensure nil
  :bind
  (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-display-summary t)
  (ibuffer-use-other-window nil)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-title-face 'font-lock-doc-face)
  (ibuffer-use-header-line t)
  (ibuffer-show-empty-filter-groups nil)
  ;; Will be overidden by all-the-icons-ibuffer-formats
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  ;; Much is taken from there:
  ;; https://olddeuteronomy.github.io/post/emacs-ibuffer-config/
  (ibuffer-saved-filter-groups
   '(("Main"
      ("Apps" (or
               (mode . diary-mode)
               (mode . elfeed-search-mode)
               (mode . elfeed-show-mode)))
      ("Mail" (or
               (mode . mu4e-main-mode)
               (mode . mu4e-headers-mode)
               (mode . mu4e-view-mode)
               (mode . mu4e-compose-mode)))
      ("Directories" (mode . dired-mode))
      ("Org" (mode . org-mode))
      ("Config" (or
                 (mode . conf-mode)
                 (mode . conf-unix-mode)
                 (mode . conf-space-mode)
                 (mode . conf-toml-mode)
                 (mode . toml-ts-mode)
                 (mode . conf-windows-mode)
                 (name . "^\\.clangd$")
                 (name . "^\\.gitignore$")
                 (name . "^Doxyfile$")
                 (name . "^config\\.toml$")
                 (mode . yaml-mode)
                 (mode . i3wm-config-mode)))
      ("C / C++" (or
                  (mode . c-mode)
                  (mode . c++-mode)
                  (mode . c++-ts-mode)
                  (mode . c-ts-mode)
                  (mode . c-or-c++-ts-mode)
                  (mode . platformio-mode)))
      ("Python" (or
                 (mode . python-ts-mode)
                 (mode . python-mode)))
      ("Rust" (or
               (mode . rust-mode)))
      ("Assembly" (or
                   (mode . asm-mode)))
      ("Java" (or
               (mode . java-mode)))
      ("Web" (or
              (mode . mhtml-mode)
              (mode . html-mode)
              (mode . web-mode)
              (mode . nxml-mode)
              (mode . css-mode)
              (mode . sass-mode)
              (mode . js-mode)
              (mode . js2-mode)
              (mode . rjsx-mode)
              (mode . php-mode)))
      ("Android" (or
                  (mode . smali-mode)))
      ("Scripts" (or
                  (mode . shell-script-mode)
                  (mode . shell-mode)
                  (mode . sh-mode)
                  (mode . lua-mode)
                  (mode . bat-mode)
                  (mode . powershell-mode)
                  (mode . dockerfile-mode)))
      ("Markup" (or
                 (mode . markdown-mode)
                 (mode . adoc-mode)))
      ("LaTeX" (mode . latex-mode))
      ("CSV" (mode . csv-mode))
      ("Text" (or
               (mode . text-mode)))
      ("Hex" (or
              (mode . hexl-mode)
              (mode . nhexl-mode)))
      ("Other" (or
                (mode . fundamental-mode)
                (mode . special-mode)))
      ("Magit" (or
                (mode . magit-blame-mode)
                (mode . magit-cherry-mode)
                (mode . magit-diff-mode)
                (mode . magit-log-mode)
                (mode . magit-process-mode)
                (mode . magit-status-mode)))
      ("Build" (or
                (mode . make-mode)
                (mode . makefile-gmake-mode)
                (mode . cmake-mode)
                (name . "^Makefile$")
                (mode . change-log-mode)))
      ("Emacs" (or
                (mode . emacs-lisp-mode)
                (name . "^\\*Help\\*$")
                (name . "^\\*Custom.*")
                (name . "^\\*Org Agenda\\*$")
                (name . "^\\*info\\*$")
                (name . "^\\*scratch\\*$")
                (name . "^\\*Backtrace\\*$")
                (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")
                    (local-set-key (kbd ";") '(lambda () (interactive)
                                                (ibuffer-switch-to-saved-filter-groups "Main")))
                    (local-set-key (kbd ":") #'ibuffer-vc-set-filter-groups-by-vc-root)))
  :config
  ;; Auto switch to ibuffer-vc-set-filter-groups-by-vc-root
  ;; when using vc format
  ;; Caution : format index 1 is hardcoded
  (defun perso/ibuffer-vc-groups-on-format (&rest _)
    "Group ibuffer by VC root when on the VC-status icon format, else `Main'.
The VC format is index 1 in `all-the-icons-ibuffer-formats'."
    (when (derived-mode-p 'ibuffer-mode)
      (if (eql ibuffer-current-format 1)
          (ignore-errors (ibuffer-vc-set-filter-groups-by-vc-root))
        (ibuffer-switch-to-saved-filter-groups "Main"))))
  (advice-add 'ibuffer-switch-format :after #'perso/ibuffer-vc-groups-on-format)
  ;; From https://emacs.stackexchange.com/a/2179
  ;; Allow nice auto-refresh without post-command-hook
  (require 'ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates " .*")
  (defun my-ibuffer-stale-p (&optional noconfirm)
    ;; let's reuse the variable that's used for 'ibuffer-auto-mode
    (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))
  (defun my-ibuffer-auto-revert-setup ()
    (set (make-local-variable 'buffer-stale-function)
         'my-ibuffer-stale-p)
    (set (make-local-variable 'auto-revert-verbose) nil)
    (auto-revert-mode 1))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-auto-revert-setup))

;; Ibuffer-vc : allows grouping by project
(use-package ibuffer-vc
  :after ibuffer
  :custom
  (ibuffer-vc-skip-if-remote 'nil))

;; All-the-icons-ibuffer : icons for ibuffer
(use-package all-the-icons-ibuffer
  :after all-the-icons
  :custom
  (all-the-icons-ibuffer-formats
   '((mark modified read-only locked
           " " (icon 2 2 :left :elide)
           " " (name 18 18 :left :elide)
           " " (size-h 9 -1 :right)
           " " (mode+ 16 16 :left :elide)
           " " filename-and-process)
     (mark modified read-only locked vc-status-mini
           " " (icon 2 2 :left :elide)
           " " (name 18 18 :left :elide)
           " " (size-h 9 -1 :right)
           " " (mode+ 16 16 :left :elide)
           " " (vc-status 0 16 :left)
           " " vc-relative-file)
     (mark " " (name 16 -1) " " filename)))
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;;; Help
;; We want to switch to the help window when opening it
(setq help-window-select t)

;; Helpful: help menu replacement
(use-package helpful
  :after elisp-refs
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

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

;;; Editing experience
;; Backspace is backspace
(normal-erase-is-backspace-mode 1)

;; Keybinds to useful unicode characters
(bind-key "C->" (lambda () (interactive) (insert "→")))
(bind-key "C-<" (lambda () (interactive) (insert "←")))

;; Simple bindings to useful functions
(global-set-key (kbd "C-z x") 'read-only-mode)

;; DWIM when available
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; Also M-DEL should kill last word
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; The two following functions are from https://codeberg.org/mehrad
;; make the home key to be smart and context-aware
(defun mm/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line.

Originally adopted from: https://stackoverflow.com/a/145359/1613005"
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "C-a") 'mm/smart-beginning-of-line)
;; make the end key to be smart and context aware
(defun mm/smart-end-of-line ()
  "Move the point to end of code (before tailing whitespace and comments) or end
of line.

When having the point in the middle of some code:
1. the first time this function is invoked, it will jump to the end of the code
   (before tailing spaces and tailing comments)
2. the second time it is invoked, it will jump to the end of the line after the
   tailing comment

This is the first function that I (Mehrad) wrote in elisp, so it may still needs some work.
"
  (interactive)
  (let ((oldpos (point)))                                            ; get the current position of point
    (let* ((bolpos (progn (beginning-of-line) (point)))              ; get the position of end of line
           (eolpos (progn (end-of-line) (point))))                   ; get the position of begining of line
      (beginning-of-line)                                            ; move to the begining of line to prepare for finding comments
      (comment-normalize-vars)                                       ; this must be run as per documentation for comment-* functions
      (comment-search-forward eolpos t)                              ; move the point to the first character of the tailing comment
      (re-search-backward (concat "[^" comment-start " ]"))          ; navigate point back to the [before] last character of the code
      (forward-char)                                                 ; move point forward to fix the shortfall of the previous command
      (and (= oldpos (point))                                        ; if the point is the same as the oldpos
           (end-of-line))))                                          ; move to the end of line
  )
(define-key prog-mode-map (kbd "C-e") #'mm/smart-end-of-line)

;; We wrap at 80
(setq-default fill-column 80)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; edit compressed files
(auto-compression-mode 1)

;; The Future is now old man
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Replace selection on insert
(delete-selection-mode 1)

;; Search / query highlighting
(setq search-highlight 1)
(setq query-replace-highlight 1)

;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;; Always end a file with a newline
(setq require-final-newline t)

;; Ws-butler : smart cleanup of trailing newlines
(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

(defun pt/eol-then-newline ()
  "Go to end of line, then newline-and-indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(define-key prog-mode-map (kbd "M-<return>") #'pt/eol-then-newline)

;; Insert char by name
(bind-key "C-z e i" #'insert-char)

;; Insert date
(defun perso/insert-current-date ()
  "Insert the current date (Y-m-d) at point."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(bind-key "C-z e d" #'perso/insert-current-date)

;; Expand-region : incrementally select region
(use-package expand-region
  :bind ("C-+" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-z e m" . mc/edit-lines)
   ("C-z e a" . mc/mark-all-dwim)))

;; Move-text: move text with M-<arrows> a-la org
(use-package move-text
  :config (move-text-default-bindings))

(use-package change-inner
  :diminish
  :bind (("M-i" . #'change-inner)
         ("M-o" . #'change-outer)))

(use-package comment-dwim-2
  :config
  (defun perso/comment-c-line()
    (interactive)
    (setq comment-style 'indent)
    (call-interactively 'comment-dwim-2))

  (defun perso/comment-c-region()
    (interactive)
    (setq comment-style 'extra-line)
    (call-interactively 'comment-dwim-2))

  (defun perso/comment-line-or-region()
    (interactive)
    (if (eq major-mode 'c-mode)
        (if (region-active-p)
            (perso/comment-c-region)
          (perso/comment-c-line))
      (call-interactively 'comment-dwim-2)))
  :bind
  ("M-;" . perso/comment-line-or-region))

(use-package ediff
  :defer t
    :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-diff-options "-w"))

;; Nhexl-mode : better hex editor
(use-package nhexl-mode)

;; Iedit : editing multiple regions simultaneously
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)

;; ;; Pos-tip : required by other modules
;; ;; at least popup-kill-ring and company-quickhelp
;; ;; FIXME : check if still true, we may be able to remove it now
;; (use-package pos-tip
;;   :pin melpa ;; the good version is on melpa
;;   )

;; Yank-media binding
(global-set-key (kbd "C-c y") 'yank-media)

;; Custom function to swap clipboard with content
(defun clipboard-swap ()
  "Swaps the clipboard contents with the highlighted region."
  (interactive)
  (if (use-region-p)
      (let ((reg-beg (region-beginning))
            (reg-end (region-end)))
        (deactivate-mark)
        (goto-char reg-end)
        (clipboard-yank)
        (clipboard-kill-region reg-beg reg-end))
    (clipboard-yank)))
(global-set-key (kbd "C-z y") 'clipboard-swap) ; Yank with the Shift key to swap instead of paste.

;; Custom funtion to copy full path
(defun full-path-to-clipboard ()
  "Copy the current buffer full path to the clipboard."
  (interactive)
  (let
      ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (kill-new filename)
    (message "Copied buffer file name '%s' to the clipboard." filename)))
(bind-key "C-z p" #'full-path-to-clipboard)

(use-package vundo
  :bind
  ("C-z u" . vundo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

;; Sudo-edit : simple commands for privileged editing
(use-package sudo-edit
  :commands (sudo-edit))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Simple hack to display line
;; when matching parentheses ar off-screen
;; from https://web.archive.org/web/20201107235946/https://with-emacs.com/posts/ui-hacks/show-matching-lines-when-parentheses-go-off-screen/
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))
(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
                (not (or cursor-in-echo-area
                         executing-kbd-macro
                         noninteractive
                         (minibufferp)
                         this-command))
                (and (not (bobp))
                     (memq (char-syntax (char-before)) '(?\) ?\$)))
                (= 1 (logand 1 (- (point)
                                  (save-excursion
                                    (forward-char -1)
                                    (skip-syntax-backward "/\\")
                                    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
                  (lambda (msg &rest args)
                    (let ((msg (apply #'format-message msg args)))
                      (setq ov (display-line-overlay+
                                (window-start) msg ))))))
         (blink-matching-open))))))

;; make characters after column 80 purple
(defun perso/show-trailing-whitespace ()
  "Show trailing whitespace in the current buffer."
  (setq-local show-trailing-whitespace t))

(use-package whitespace
  :diminish
  :hook ((prog-mode . whitespace-mode)
         (prog-mode . perso/show-trailing-whitespace))
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face trailing tab-mark space-before-tab))
  :bind (("C-z w"   . whitespace-cleanup)
         ("C-z C-l" . perso/whitespace-lines-tail))
  :config
  (defun perso/whitespace-lines-tail ()
    "Toggle whitespace line-tail highlighting."
    (interactive)
    (whitespace-toggle-options 'lines-tail)))

;; also display column number
(setq column-number-mode t)

;; Wrap lines in compilation and flycheck buffers
(add-hook 'compilation-mode-hook 'visual-line-mode)

;; Page-break-lines : enable to show ^L as straight horizontal lines
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;;All-the-icons : unified icon pack
;; Requires manually installing the fonts with M-x all-the-icons-install-fonts and M-x nerd-icons-install-fonts
(use-package all-the-icons
  :pin melpa
  :if (display-graphic-p))

(use-package beacon
  :diminish
  :custom
  (beacon-color "#5F7F5F")
  :hook (after-init . beacon-mode))

(use-package dimmer
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :custom
  (dimmer-fraction 0.2)
  :config
  (dimmer-mode))

;;; Languages and spell-checking
;; Quick set of functions to handle language switches
(defvar-local perso/buffer-is-french nil
  "Non-nil when Grammalecte should grammar-check the current buffer.")

(defun perso/grammalecte-flymake (report-fn &rest args)
  "Flymake backend: run Grammalecte, but only in French buffers.
In non-French buffers it reports no diagnostics, which clears any
stale French overlays on the next check."
  (if perso/buffer-is-french
      (apply (flymake-flycheck-diagnostic-function-for 'grammalecte) report-fn args)
    (funcall report-fn nil)))

(defun perso/set-checkers-language (lang typo-lang)
  "Restrict Jinx to LANG, switch Typo to TYPO-LANG, toggle Grammalecte.
French grammar checking follows the chosen language."
  (setq-local jinx-languages lang)
  (jinx-mode -1)
  (jinx-mode 1)
  (when (bound-and-true-p typo-mode) (typo-change-language typo-lang))
  ;; Grammalecte follows the chosen language:
  (setq perso/buffer-is-french
        (and (string-match-p "fr" lang) (not (string-match-p "en" lang))))
  (when (bound-and-true-p flymake-mode)
    (flymake-start nil t)))   ; force re-run: clears French overlays unless now French

(defun perso/set-language-french ()
  "Switch all text checkers in this buffer to French."
  (interactive)
  (perso/set-checkers-language "fr_FR" "French"))
(defun perso/set-language-english ()
  "Switch all text checkers in this buffer to English."
  (interactive)
  (perso/set-checkers-language "en_US" "English"))
(bind-key "C-c f" #'perso/set-language-french)
(bind-key "C-c e" #'perso/set-language-english)

;; Flymake : on-the-fly error checking
(use-package flymake
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'right-fringe)
  :bind (:map flymake-mode-map
              ("C-x l !" . consult-flymake)
              ("M-n"     . flymake-goto-next-error)
              ("M-p"     . flymake-goto-prev-error))
  :hook (emacs-lisp-mode . flymake-mode))

;; Flymake-popon : diagnostics in a popup near point
(use-package flymake-popon
  :diminish
  :after flymake
  :custom
  (flymake-popon-method 'posframe)
  (flymake-popon-delay 0.2)
  (flymake-popon-width 70)
  (flymake-popon-posframe-border-width 1)
  :hook (flymake-mode . flymake-popon-mode)
  (flymake-popon-mode . perso/flymake-popon-quiet-eldoc)
  :config
  (defun perso/flymake-popon-quiet-eldoc ()
    "Drop Flymake's echo-area ElDoc line while the popon shows diagnostics.
Turning `flymake-popon-mode' off restores it. Eglot's own ElDoc
documentation (hover, signatures) is left intact."
    (if flymake-popon-mode
        (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
      (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function nil t))))

;; Flymake-collection : linting for non-LSP languages
;; (json, yaml, shell, dockerfile, ...)
(use-package flymake-collection
  :ensure t
  :hook (after-init . flymake-collection-hook-setup))

(dolist (h '(sh-mode-hook bash-ts-mode-hook
             yaml-mode-hook yaml-ts-mode-hook
             json-mode-hook js-json-mode-hook json-ts-mode-hook
             dockerfile-mode-hook))
  (add-hook h #'flymake-mode))

;; Flymake-flycheck : to use flycheck
;; checkers as flymake backends
(use-package flymake-flycheck :defer t)

;; Flycheck-grammalecte : french syntax checking
;; Requires running M-x grammalecte-download-grammalecte once
(use-package flycheck-grammalecte
  :defer t
  :init
  (setq flycheck-grammalecte-report-spellcheck nil
        flycheck-grammalecte-report-grammar t
        flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp nil
        flycheck-grammalecte-report-nbsp nil)
  :config
  (setq flycheck-grammalecte-filters-by-mode
        '((latex-mode "\\\\(?:title|(?:sub)*section){([^}]+)}"
                      "\\\\\\w+(?:\\[[^]]+\\])?(?:{[^}]*})?")
          (org-mode "(?ims)^[ \t]*#\\+begin_src.+?#\\+end_src"
                    "(?ims)^[ \t]*:LOGBOOK:.+?:END:"
                    "(?ims)^[ \t]*:PROPERTIES:.+?:END:"
                    "(?im):.*:" ; tags
                    "(?im)<.*>" ; timestamps
                    "(?im)\\[.*\\]" ; links, progress, etc.
                    "(?im)^[ \t]*\-[ \t]*" ; checkboxes
                    "(?im)(?im)[0-9]+" ; numbers
                    "(?im)^[ \t]*#\\+begin[_:].+$"
                    "(?im)^[ \t]*#\\+end[_:].+$"
                    "(?m)^[ \t]*(?:DEADLINE|SCHEDULED):.+$"
                    "(?m)^\\*+ .*[ \t]*(:[\\w:@]+:)[ \t]*$"
                    "(?im)^[ \t]*#\\+(?:caption|description|keywords|(?:sub)?title):"
                    "(?im)^[ \t]*#\\+(?!caption|description|keywords|(?:sub)?title)\\w+:.*$")
          (message-mode "(?m)^[ \t]*(?:[\\w_.]+>|[]>|]).*")))
  (setq grammalecte-python-package-directory
        (expand-file-name "grammalecte" user-emacs-directory))
  ;; (setq flycheck-grammalecte-predicate #'perso/grammalecte-predicate)
  (flycheck-grammalecte-setup))

;; Prose modes for grammalecte to check.
;; text-mode also covers Magit commit buffers,
;; whose default major mode is text-mode.
(defvar my/grammalecte-modes
  '(text-mode latex-mode mail-mode markdown-mode
              message-mode mu4e-compose-mode org-mode)
  "Major modes in which Grammalecte runs as a Flymake backend.")

(defun my/grammalecte-flymake ()
  "Enable Flymake with Grammalecte as its only backend, in prose buffers."
  (when (memq major-mode my/grammalecte-modes)
    (require 'flycheck-grammalecte)       ; ensure the `grammalecte' checker exists
    (require 'flymake-flycheck)
    (add-hook 'flymake-diagnostic-functions #'perso/grammalecte-flymake nil t)
    (flymake-mode 1)))

(add-hook 'after-change-major-mode-hook #'my/grammalecte-flymake)

;; Jinx : fast, multi-language spell-checking via Enchant
;; First launch compiles jinx-mod.c;
;; needs installing libenchant-2-dev + a C compiler.
(use-package jinx
  :diminish
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US fr_FR")
  :bind
  (("C-." . jinx-correct)
   ("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages))
  :config
  ;; (mu4e): if reply quotes / headers get spell-checked in compose
  ;; buffers, uncomment to exclude them.
  ;; (add-to-list 'jinx-exclude-faces
  ;;              '(message-mode message-header-name message-header-to
  ;;                message-header-cc message-header-subject message-header-other
  ;;                message-cited-text-1 message-cited-text-2
  ;;                message-cited-text-3 message-cited-text-4))
  )

;; sdcv : Stardict dictionnary
(when (executable-find "sdcv")
  (use-package sdcv
    :bind
    (("C-c d" . sdcv-search-pointer)
     ("C-c w" . sdcv-search-input))
    :config
    (setq sdcv-dictionary-data-dir "~/.stardic/dic")
    (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
          '("XMLittre"
            ))
    (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
          '(
            "XMLittre"
            "Dictionnaire de l’Académie Française, 8ème édition (1935)."
            "Oxford Advanced Learner's Dictionary 8th Ed."
            "Oxford English Dictionary 2nd Ed. P1"
            "Oxford English Dictionary 2nd Ed. P2"
            ))
    ))

;; Typo: auto-replace typographically useful unicode characters
(use-package typo
  :diminish
  :hook
  ((org-mode text-mode) . typo-mode))

;; Small function to toggle all text analysis modes
(defvar-local toggle-text-analysis-modes--state nil
  "Saved state of text analysis modes before disabling. Nil means modes are currently active.")

(defun perso/toggle-text-analysis-modes ()
  "Toggle flycheck, jinx and typo modes.
First call saves each mode's current state and disables all of them.
Second call restores each mode to its previously saved state."
  (interactive)
  (if toggle-text-analysis-modes--state
      (progn
        (when (plist-get toggle-text-analysis-modes--state :flycheck)
          (flycheck-mode 1))
        (when (plist-get toggle-text-analysis-modes--state :jinx)
          (jinx-mode 1))
        (when (plist-get toggle-text-analysis-modes--state :typo)
          (typo-mode 1))
        (setq toggle-text-analysis-modes--state nil)
        (message "Text analysis modes restored"))
    (setq toggle-text-analysis-modes--state
          (list :flycheck (bound-and-true-p flycheck-mode)
                :jinx (bound-and-true-p jinx-mode)
                :typo     (bound-and-true-p typo-mode)))
    (flycheck-mode -1)
    (jinx-mode -1)
    (typo-mode -1)
    (message "Text analysis modes disabled")))
(bind-key "C-z t" #'perso/toggle-text-analysis-modes)

;; Small function to disable all text analysis modes
(defun disable-text-analysis-modes ()
  "Explicitely disable flycheck, jinx and typo"
  (interactive)
  (flycheck-mode -1)
  (jinx-mode -1)
  (typo-mode -1))

;; Small collection of function and parameters to make the current buffer as fast as possible
(defun custom-fast-mode ()
  "Disable stuff and change parameters to be faster"
  (interactive)
  (disable-text-analysis-modes)
  (display-line-numbers-mode -1)
  (corfu-mode -1))
(bind-key "C-z f" #'custom-fast-mode)

;;; General programming
;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)

;; electric-indent is globally on by default; disable it only where unwanted,
(dolist (hook '(text-mode-hook erc-mode-hook))
  (add-hook hook (lambda () (electric-indent-local-mode -1))))

;; Auto-highlight some keywords
;; from https://www.jamescherti.com/emacs-highlight-keywords-like-todo-fixme-note/
;; List available faces with M-x list-faces-display
(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|FIXME\\|BUG\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\)\\>" 1 font-lock-doc-face prepend)))
(define-minor-mode highlight-codetags-local-mode
  "Highlight codetags like TODO, FIXME..."
  :global nil
  (if highlight-codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))
  ;; Fontify the current buffer
  (when (bound-and-true-p font-lock-mode)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))
(add-hook 'prog-mode-hook #'highlight-codetags-local-mode)
(add-hook 'org-mode-hook #'highlight-codetags-local-mode)

;; Highlight-indent-guides : show indentation level
;; (use-package highlight-indent-guides
;;   :diminish
;;   ;; Automatically enabled, but there is a bug that might require to disable it:
;;   ;; https://github.com/DarthFennec/highlight-indent-guides/issues/76
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-responsive 'top)
;;   (highlight-indent-guides-delay 0))

;; Treesit customization
;; Run M-x treesit-install-language-grammar for each language
;; Alternatively, eval this command to install all at once :
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package indent-bars
  :quelpa (indent-bars :repo "jdtsmith/indent-bars" :fetcher github :commit "main")
  :custom
  ;; Style customization
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)) ; blend=1: blend with BG only
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-display-on-blank-lines nil)
  ;; Treesitter customization
  ;; Require treesit-language source configuration and installation
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-wrap '((python argument_list parameters
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook ((prog-mode) . indent-bars-mode))

(use-package which-func
  :ensure nil
  :hook ((prog-mode org-mode) . which-function-mode))

;; Hideshow : hide (wrap) parts of the code
(use-package hideshow
  :diminish
  :defer t
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-z <tab>" . toggle-fold)
         ("C-z h a" . hs-hide-all))
  :init
  (defun toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding))))

;; Doom-modeline : rich modeline from doom-emacs
(use-package doom-modeline
  :ensure t
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-checker-simple-format nil)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-gnus nil)
  (doom-modeline-irc t)
  (doom-modeline-height 1)
  (all-the-icons-scale-factor 1.2)
  :init (doom-modeline-mode 1)
  :config
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number matches follow buffer-info remote-host buffer-position word-count selection-info)
    '(objed-state misc-info persp-name grip debug repl lsp minor-modes indent-info buffer-encoding major-mode process vcs check " "))
  )

;; Smartparens : auto parenthesis,  etc.
(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :bind
  (("C-z ("  . wrap-with-parens)
   ("C-z ["  . wrap-with-brackets)
   ("C-z {"  . wrap-with-braces)
   ("C-z '"  . wrap-with-single-quotes)
   ("C-z \"" . wrap-with-double-quotes)
   ("C-z _"  . wrap-with-underscores)
   ("C-z `"  . wrap-with-back-quotes)
   ("C-(" . (lambda () (interactive)
              (sp-beginning-of-sexp) (backward-char)))
   ("C-)" . (lambda () (interactive)
              (sp-end-of-sexp) (forward-char)))
   ("M-(" . sp-down-sexp)
   ("M-)" . (lambda () (interactive)
              (sp-end-of-sexp) (forward-char))))
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Auto newline in some pairs
  ;; (let ((c-like-modes-list '(c-mode c++-mode java-mode perl-mode)))
  ;;   (sp-local-pair c-like-modes-list "(" nil
  ;;                  :post-handlers '(:add add-paren-dwim)))
  ;; (sp-local-pair c-like-modes-list "{" nil
  ;; :post-handlers '(:add open-block-dwim)))

  ;; Some of the following is derived from
  ;; https://www.omarpolo.com/dots/emacs.html
  (defun current-line-str ()
    "Return the current line as string."
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))

  (defun inside-block-comment-or-string-p ()
    "T if point is inside a block, string or comment."
    (let ((s (syntax-ppss)))
      (or (= (nth 0 s) 0)               ; outside parens/blocks
          (nth 4 s)                     ; comment
          (nth 3 s))))                  ; string

  (defun inside-comment-or-string-p ()
    "T if point is inside a string or comment."
    (let ((s (syntax-ppss)))
      (or (nth 4 s)                     ; comment
          (nth 3 s))))                  ; string

  (defun add-paren-dwim (_id action _ctx)
    "Insert space before or semicolon after parens when appropriat."
    (when (eq action 'insert)
      (save-excursion
        ;; caret is between parens (|)
        (forward-char)
        (let ((line (current-line-str)))
          (when (not (inside-block-comment-or-string-p))
            (if (and (looking-at "\\s-*$")
                     (not (string-match-p
                           (regexp-opt '("if" "else" "switch" "for" "while"
                                         "do" "define")
                                       'words)
                           line))
                     (string-match-p "[\t ]" line))
                (insert ";")
              (progn
                (backward-char)
                (backward-char)
                ;; no space if previous char is space or opening parentheses
                (when (and (not (= (char-before) ?\())
                           (not (= (char-before) ?\ )))
                  (insert " ")))))))))

  (defun open-block-dwim (id action context)
    (when (eq action 'insert)
      (when (not (inside-comment-or-string-p))
        (let ((line (current-line-str)))
          (save-excursion
            ;; caret is between parens {|}
            (backward-char)
            (when (and (or (= (char-before) ?\))
                           (= (char-before) ?\=)))
              (insert " "))
            (forward-char))
          (if (not (string-match-p "^[[:space:]]*{}[[:space:]]*$" line))
              (progn
                (newline)
                (newline)
                (indent-according-to-mode)
                (previous-line)
                (indent-according-to-mode)))))))

  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val)))))

  (def-pairs ((paren . "(")
              (bracket . "[")
              (brace . "{")
              (single-quote . "'")
              (double-quote . "\"")
              (back-quote . "`"))))

;; Electric-operator : add spaces around operators
(use-package electric-operator
  :diminish
  :config
  (electric-operator-add-rules-for-mode 'c-mode
                                        (cons "{" " {"))
  :hook ((c-mode c++-mode python-mode rust-mode java-mode php-mode) . electric-operator-mode))

;; Quickrun : compile and run quickly
(use-package quickrun
  :custom
  (quickrun-timeout-seconds 60)
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)))

;; Rainbow-delimiters : colors for parenthesis
(use-package rainbow-delimiters
  :diminish
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Color-identifiers-mode : symbol colors
(use-package color-identifiers-mode
  :diminish
  :ensure t
  :init
  ;; Enabled by default for now to try it out
  (global-color-identifiers-mode)
  ;; :commands color-identifiers-mode
  )

;; Yasnippet : common code templates
(use-package yasnippet
  :diminish (yas-minor-mode)
  :init
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ([(tab)] . nil))
  (:map yas-minor-mode-map ("TAB" . nil))
  (:map yas-minor-mode-map ("<tab>" . nil))
  ("C-z C-s" . yas-insert-snippet)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets :after yasnippet)

;; Dumb-jump : simple "jump to definition" tool
(use-package dumb-jump
  :bind
  ("C-z C-j" . dumb-jump-go)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Corfu : in-buffer completion
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-count 10)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  :bind
  (("C-<tab>" . completion-at-point)
   :map corfu-map
   ("TAB"     . corfu-next)
   ([tab]     . corfu-next)
   ("S-TAB"   . corfu-previous)
   ([backtab] . corfu-previous)
   ("RET"     . corfu-complete))
  :init
  (global-corfu-mode 1)
  :config
  (corfu-indexed-mode 1)
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Corfu popup in the terminal (-nw),
;; where child frames are unavailable
(use-package corfu-terminal
  :after corfu
  :config (corfu-terminal-mode 1))

;; Prescient sorting for Corfu (Orderless still filters)
(use-package corfu-prescient
  :after (corfu prescient)
  :demand t
  :custom
  (corfu-prescient-enable-filtering nil) ; let Orderless filter
  (corfu-prescient-enable-sorting t)
  :config (corfu-prescient-mode 1))

;; Cape : capf sources + per-mode "backends"
(use-package cape
  :after corfu
  :bind ("C-z c" . cape-prefix-map)
  :custom
  (cape-dabbrev-check-other-buffers t)
  :init
  ;; Baseline for buffers the per-mode hooks below don't touch (conf, special…)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  ;; Reproduce your grouped company-backends:
  ;;   ((company-capf company-dabbrev :with company-yasnippet) company-files)
  ;; -> a super-capf merging the buffer's own capf + dabbrev (main sources) with
  ;;    yasnippet (auxiliary), and cape-file as a fallback.
  (defun perso/capf (mains &optional leading)
    "Buffer-local capf: optional LEADING capfs, then MAINS + dabbrev merged with
yasnippet, then file. MAINS/LEADING are lists of capf functions."
    (setq-local completion-at-point-functions
                (append leading
                        (list (apply #'cape-capf-super
                                     `(,@mains cape-dabbrev :with yasnippet-capf))
                              #'cape-file))))

  ;; Generic case: capture the capf the mode already set (Elisp, Lua, sh, markdown,
  ;; plain text…) and merge the extras onto it.
  (defun perso/capf-here ()
    ;; (perso/capf (remq t completion-at-point-functions)))
    ;; Drop ispell's capf — it errors when no word-list is installed.
    (perso/capf (seq-remove (lambda (f) (eq f #'ispell-completion-at-point))
                            (remq t completion-at-point-functions))))

  (add-hook 'prog-mode-hook #'perso/capf-here)
  (add-hook 'text-mode-hook #'perso/capf-here)

  ;; Org: pcomplete as the primary (overrides the text-mode catch-all, runs after it)
  (add-hook 'org-mode-hook
            (lambda () (perso/capf (list #'pcomplete-completions-at-point))))

  ;; ;; LSP: fires once the server is connected, so lsp-completion-at-point exists.
  ;; ;; THIS is what keeps dabbrev/yasnippet merged *with* LSP instead of a fallback.
  ;; (defun perso/capf-lsp ()
  ;;   (perso/capf
  ;;    (list #'lsp-completion-at-point)
  ;;    ;; #include completion in C modes — clangd already does this. To use
  ;;    ;; company-c-headers instead, keep `company'+`company-c-headers' and uncomment:
  ;;    ;; (when (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode 'objc-mode)
  ;;    ;;   (list (cape-company-to-capf #'company-c-headers)))
  ;;    ))
  ;;
  ;; (add-hook 'lsp-completion-mode-hook #'perso/capf-lsp))

  (defun perso/capf-eglot ()
    (perso/capf
     (list (lambda ()
             (when (eglot-current-server) (eglot-completion-at-point))))))
  (add-hook 'eglot-managed-mode-hook #'perso/capf-eglot))

;; Snippets as a capf, so they appear in Corfu
(use-package yasnippet-capf
  :after (cape yasnippet))

;;; IDE-like features
;; Eglot : IDE features
(use-package eglot
  :ensure t
  :defer t
  :hook
  ;; Enable more modes here if needed
  (((python-mode c-mode c++-mode objc-mode rust-mode php-mode
                 js-mode js2-mode typescript-mode web-mode cmake-mode
                 ;; tree-sitter variants now, so the Emacs 31 switch is mostly free:
                 python-ts-mode c-ts-mode c++-ts-mode rust-ts-mode) . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  ;; less overhead; raise only to debug
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :inlayHintProvider))
  :bind
  (:map eglot-mode-map
        ("C-x l f"   . eglot-format-buffer)
        ("C-x l r"   . eglot-rename)
        ("C-x l a"   . eglot-code-actions)
        ("C-x l g r" . xref-find-references))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((web-mode) . ("vscode-html-language-server" "--stdio")))
  ;; the C/C++/ObjC entry is defined later
  (add-to-list 'eglot-server-programs
               `((c-mode c++-mode objc-mode c-ts-mode c++-ts-mode)
                 . ,#'perso/cc-contact))
(add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (eglot-managed-p)
                (setq-local flymake-diagnostic-functions
                            (list #'eglot-flymake-backend))))))

;; Eglot-booster: uses emacs-lsp-booster binary
;; Improve JSON parsing performance when using lsp-mode
;; Require both
;; - compile lsp-mode with plist deserializaton (see https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
;; - installing emacs-lsp-booster (see https://github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :ensure t
  :quelpa (eglot-booster :repo "jdtsmith/eglot-booster" :fetcher github :commit "main")
  :after eglot
  :config (eglot-booster-mode))

;; Docs on demand
(use-package eldoc-box
  :diminish
  :ensure t
  :after eglot
  :bind
  (:map eglot-mode-map ("C-x l i" . eldoc-box-help-at-point)))

;; Header-line breadcrumb
(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-local-mode))

;; Workspace symbols
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map ("C-x l s" . consult-eglot-symbols)))

;; C/C++ server switch functions
(defvar perso/cc-server 'clangd
  "Active C/C++/ObjC language server: `clangd', `ccls', or `ccls-esp'.")

(defvar perso/clangd-args
  '("-j=2" "--header-insertion=never" "--header-insertion-decorators=0"
    "--pch-storage=memory" "--background-index" "--log=error")
  "Args passed to clangd (your former lsp-clients-clangd-args).")

(defvar perso/ccls-native-path "ccls"
  "Executable for the system/native ccls build.")

(defvar perso/ccls-esp-path (expand-file-name "~/src/ccls/Release/ccls")
  "Executable for the Espressif-LLVM ccls build.")

(defun perso/cc-contact (&optional _interactive _project)
  "Return the eglot server contact for the currently selected C/C++ server.
Eglot calls this when (re)connecting; it reads `perso/cc-server'."
  (pcase perso/cc-server
    ('ccls     (list perso/ccls-native-path "--log-file=/tmp/ccls.log"))
    ('ccls-esp (list perso/ccls-esp-path    "--log-file=/tmp/ccls-esp.log"))
    (_         (cons "clangd" perso/clangd-args))))

(defun perso/cc-switch-server (server)
  "Switch the C/C++/ObjC eglot SERVER and restart it in this buffer.
SERVER is one of the symbols `clangd', `ccls', `ccls-esp'."
  (interactive
   (list (intern (completing-read
                  "C/C++ server: " '("clangd" "ccls" "ccls-esp") nil t))))
  (setq perso/cc-server server)
  (when-let ((s (and (fboundp 'eglot-current-server) (eglot-current-server))))
    (eglot-shutdown s))                  ; reconnect won't re-read the choice; restart instead
  (when (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'c-ts-mode 'c++-ts-mode)
    (eglot-ensure))
  (message "C/C++ server -> %s" server))

;; DAPE : debugging mode
(use-package dape
  :ensure t
  :commands dape
  :custom
  (dape-buffer-window-arrangement 'right)
  :config
  ;; Optional niceties:
  ;; (dape-breakpoint-global-mode 1) ; set breakpoints with the mouse
  )

;; Compile-mode : view compilation output
(use-package compile
  :defer t
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-max-output-line-length nil))

;;; Cmake specifics
;; Cmake-mode
(use-package cmake-mode
  :mode (("\\`CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake$" . cmake-mode)))

;;; Elisp modes and settings
;; Highlight-defined for colored Elisp symbols
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;; Python-specific modes and settings
;; Python settings
(use-package python
  :custom
  ;; I usually prefer a dedicated terminal, so maybe remove it
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i --pprint")
  (python-indent-offset 4))

;; C-mode settings
;; FIXME : tree-sitter C modes indent via treesit, not c-styles
;; So I may have to check this when emacs 31 drops
(use-package cc-mode
  :defer t
  :bind (:map c-mode-map
              ("C-c C-c" . (lambda ()
                             (interactive)
                             (perso/project-compile)
                             (switch-to-buffer-other-frame "*compilation*")))))
(use-package c-ts-mode
  :defer t
  :bind (:map c-ts-mode-map
              ("C-c C-c" . (lambda ()
                             (interactive)
                             (perso/project-compile)
                             (switch-to-buffer-other-frame "*compilation*")))))

;; Python-mode settings
(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4))

;; ;; Pyenv : managing python version/venv with pyenv and pyenv-virtualenv
;; ;; Had to fork it to make it buffer-local because it has global keybindings
;; ;; that conflict with org-mode
;; (use-package pyenv-mode
;;   :ensure nil
;;   :quelpa (pyenv-mode :repo "mclbn/pyenv-mode" :fetcher github :commit "master")
;;   :diminish
;;   :after projectile
;;   :config
;;   (defun pyenv-detect-env ()
;;     "Try to identify pyenv via projectile, then .python-version."
;;     (interactive)
;;     (if (and (projectile-project-name)(member (projectile-project-name) (pyenv-mode-versions)))
;;         (projectile-project-name)
;;       (let ((pyenv-file (concat (projectile-project-root) ".python-version")))
;;         (if (and (file-exists-p pyenv-file))
;;             (let ((pyversion (first (split-string (f-read-text pyenv-file) "\n" t))))
;;               (if (member pyversion (pyenv-mode-versions))
;;                   (first (split-string (f-read-text pyenv-file) "\n" t))
;;                 nil))
;;           nil))))
;;   (defun pyenv-set-env ()
;;     "Try to identify and set pyenv."
;;     (interactive)
;;     (let ((pyenv-name (pyenv-detect-env)))
;;       (if pyenv-name
;;           (pyenv-mode-set pyenv-name)
;;         (pyenv-mode-set "default"))))
;;   (add-hook 'python-mode-hook 'pyenv-set-env)
;;   ;; We will need this at some point
;;   (use-package with-venv)
;;   :hook (python-mode . pyenv-mode)
;;   :init
;;   (let ((pyenv-path (expand-file-name "~/.pyenv/bin")))
;;     (setenv "PATH" (concat pyenv-path ":" (getenv "PATH")))
;;     (add-to-list 'exec-path pyenv-path))
;;   ;; is the following line needed ?
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (pyenv-mode-set "default"))

;;; C / C++ / Objective-C modes and settings
;; Create my personal style.
(defconst my-c-style
  '((c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
                                        ; default but we have a custom function : perso/comment-line-or-region
    (comment-style . extra-line)
    (c-offsets-alist
     (substatement-open . 0)
     (case-label . +)
     (inline-open . 0)
     (block-open . 0)
     (statement-cont . +)
     (inextern-lang . 0)
     (innamespace . 0)))
  "My C Programming Style")
(c-add-style "perso" my-c-style)

(defun my-c-mode-common-hook ()
  (c-set-style "perso"))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Emacs-ccls, compiled from source (https://github.com/MaskRay/ccls)
(use-package ccls
  :config
  ;; (setq ccls-executable (expand-file-name "~/src/ccls/Release/ccls"))
  (defun lsp-switch-to-ccls-esp ()
    "Switch current lsp workspace to ccls with Espressif llvm"
    (interactive)
    (setq ccls-executable (expand-file-name "~/src/ccls/Release/ccls"))
    (lsp-restart-workspace))

  (defun lsp-switch-to-ccls-native ()
    "Switch current lsp workspace to system-native ccls"
    (interactive)
    (setq ccls-executable "ccls")
    (lsp-restart-workspace))

  (setq ccls-args '("--log-file=/tmp/ccls.log")))
;; :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;        (lambda () (require 'ccls) (lsp))))

;;; Php modes and settings
;; PHP-mode settings
(use-package php-mode
  :ensure t)

;;; JavaScript / Typescript modes and settings
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))

;;; Web modes and settings
(use-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

;;; Rust modes and settings
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  ;; (use-package flycheck-rust
  ;;   :after flycheck
  ;;   :config
  ;;   (with-eval-after-load 'rust-mode
  ;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
  )

;;; Assembly modes and settings
;; asm-mode settings
(use-package asm-mode
  :ensure nil
  :hook (asm-mode . (lambda ()
                      (setq-local indent-tabs-mode nil)
                      (electric-indent-local-mode -1))))


;;; Android development modes and settings
;;; smali/baksmali mode (https://github.com/strazzere/Emacs-Smali)
(use-package smali-mode
  :load-path (lambda () (expand-file-name "Emacs-Smali" user-emacs-directory))
  :config
  (add-to-list 'auto-mode-alist '("\\.smali$" . smali-mode)))

;;; Lua modes and settings
(use-package lua-mode
  :defer t)

;; Scad-modes and settings
(use-package scad-mode)

;; Powershell-mode
(use-package powershell)

;;; I3WM modes and settings
(use-package i3wm-config-mode
  :ensure t)

;;; Docker modes and settings
(use-package dockerfile-mode :defer t)

;;; Yaml-mode
(use-package yaml-mode)

;; CSV modes and settings
(use-package csv-mode)

;;; Markdown modes and settings
;; Markdown-mode
(use-package markdown-mode
  :defer t
  :custom
  (markdown-fontify-code-blocks-natively t))

;; Vmd-mode : alternative markdown live preview
;; Maybe the future is grip-mode here...
(use-package vmd-mode
  :defer t)

;; HCL-mode : Hashicorp Configuration Language
(use-package hcl-mode)

;;; Mise en place
;; Mise
(use-package mise
  :hook
  (prog-mode . mise-mode))

;;; Org-mode
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
  ;;; LaTeX exports
  (use-package ox-latex
    :ensure nil
    :defer t
    :after org
    :config
    (setq org-latex-src-block-backend 'listings))
  ;; other config stuff
  (if (string= (getenv "EMACS_WORK") "Y")
      (progn
        (defun perso/org-work-files ()
          (seq-filter
           (lambda (x) (not (string-match "/templates/" (file-name-directory x))))
           (directory-files-recursively
            "~/org-work" "\\.org\\'" nil
            (lambda (subdir) ;  don't descend into dot prefixed dirs
              (not (string-prefix-p "." (file-name-nondirectory subdir)))))))
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
  (add-hook 'org-mode-hook #'my/org-image-auto-preview-setup))

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

  (defun perso/org-capture-feeds-file ()
    (concat org-directory "/rss.org"))

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

          ("f" "add RSS feed"
           entry (file+olp ,(perso/org-capture-feeds-file) "Feeds" "Unsorted")
           "* [[%^{link-url}][%^{link-description}]]"
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
           :prepend nil)

          ("r" "new task: respond to email"
           entry (file+headline ,(perso/org-capture-tasks-file) "Tâches rapides")
           "* TODO Répondre à [[mailto:%:fromaddress][%:fromname]]\nSCHEDULED: %^t"
           :immediate-finish t
           :empty-lines 1
           :prepend nil)))
  (setq org-capture-templates-contexts
        '(("r" ((in-mode . "mu4e-view-mode"))))))

;; Org-caldav : caldav sync
;; Only for personal stuff
(if (not (string= (getenv "EMACS_WORK") "Y"))
    (progn
      (use-package org-caldav
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
		(setq org-icalendar-timezone "Europe/Paris")
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
  ((dired-mode-hook . org-download-enable)
   (org-mode-hook . org-download-enable)
   (org-mode-hook . (lambda ()
                      (local-set-key (kbd "C-c y") '(lambda ()
                                                      (interactive)
                                                      (org-download-clipboard)
                                                      (org-download-rename-last-file)))))
   (org-mode-hook . (lambda ()
                      (local-set-key (kbd "C-c x") '(lambda ()
                                                      (interactive)
                                                      (org-download-screenshot)))))))

(use-package org-appear
  :custom
  (org-appear-delay 0.2)
  :hook
  org-mode)

;;; Mail management
(if (not (string= (getenv "EMACS_NOMU") "Y"))
    (progn
      ;; Include protonmail-bridge cert
      (require 'gnutls)
      (add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge/cert.pem"))
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
      ))

;; OX-pandoc : export org via pandoc
(use-package ox-pandoc)

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

;;; File/mode associations
;; Script-shell-mode on zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
;; .in and .out are text by default
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
;; Arduino is C++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;;; "AI" stuff
;; MCP
(use-package mcp
  :after gptel
  :custom
  (mcp-hub-servers
   `(("searxng" . (:url ,perso/mcp-searxng-url)))))

;; GPTel : chat with LLMs
(use-package gptel
  :config
  (require 'gptel-integrations)
  (require 'gptel-org)
  (when (executable-find "curl")
    (setq gptel-use-curl t))
  (gptel-make-openai "llama-cpp-main"
    :stream t
    :protocol "http"
    :host (concat perso/llm-host-main
                  ":" perso/llm-port-main)
    :models '(qwen36-27b-opti
              qwen36-27b-opti-large
              qwen36-35b-quality
              qwen35-9b-quality
              qwen35-9b-extra-quality
              qwen3-coder-next
              gemma-12b-quality
              glm47-flash))
  (gptel-make-openai "llama-cpp-back"
    :stream t
    :protocol "http"
    :host (concat perso/llm-host-backup
                  ":" perso/llm-port-backup)
    :models '(qwen35-4b
              qwen25-coder-7b
              gemma4-12b))
  (setq gptel-backend (gptel-get-backend "llama-cpp-main")
        gptel-model 'qwen36-27b-opti)

  ;; General settings
  (setq
   gptel-default-mode 'org-mode
   gptel-use-tools t
   gptel-confirm-tool-calls t
   gptel-include-tool-results 'auto)

  (with-eval-after-load 'gptel-transient
    (transient-define-infix perso/gptel--infix-branching-context ()
      "Toggle `gptel-org-branching-context' from the gptel menu."
      :description "Branching context (Org)"
      :class 'gptel--switches
      :variable 'gptel-org-branching-context
      :set-value #'gptel--set-with-scope
      :display-if-true  "On"
      :display-if-false "Off"
      :key "B")
    (transient-append-suffix 'gptel-menu "y"
      '(perso/gptel--infix-branching-context
        :if (lambda () (derived-mode-p 'org-mode)))))

  ;;; Tools
  (gptel-make-tool
   :name "read_file"
   :function (lambda (path)
               (let ((full-path (expand-file-name path)))
                 (cond
                  ((not (file-exists-p full-path))
                   (format "Error: file does not exist: %s" full-path))
                  ((not (file-readable-p full-path))
                   (format "Error: file is not readable: %s" full-path))
                  ((> (file-attribute-size (file-attributes full-path)) (* 1024 1024))
                   (if (yes-or-no-p (format "File %s is large (> 1 MB). Read anyway? " full-path))
                       (with-temp-buffer
                         (insert-file-contents full-path)
                         (buffer-string))
                     "Aborted by user."))
                  (t
                   (with-temp-buffer
                     (insert-file-contents full-path)
                     (buffer-string))))))
   :description "Read and return the contents of a file (read-only)."
   :args (list '(:name "path" :type "string" :description "Path to the file."))
   :category "filesystem")

  (gptel-make-tool
   :name "list_directory"
   :function (lambda (path)
               (let ((full-path (expand-file-name path)))
                 (if (file-directory-p full-path)
                     (string-join
                      (directory-files full-path nil nil t)
                      "\n")
                   (format "Error: not a directory: %s" full-path))))
   :description "List the entries in a directory."
   :args (list '(:name "path" :type "string" :description "Directory path."))
   :category "filesystem")

  ;;; Presets
  (gptel-make-preset 'eli5
                     :system "Explain like I am 5 years old."
                     :use-tools t
                     :tools '("web_url_read" "searxng_web_search"))

  (gptel-make-preset 'websearch
                     :description "Web search"
                     :pre (lambda ()
                            (gptel-mcp-connect '("searxng") 'sync nil))
                     :use-tools t
                     :tools '("web_url_read" "searxng_web_search")
                     :system "Use the provided tools to search the web for up-to-date information.")

  (gptel-make-preset 'creative
                     :description "Quick creative — high temp, no tools"
                     :temperature 1
                     :tools nil :use-tools nil
                     :system "You are an imaginative creative collaborator. Offer vivid, varied ideas.")

  (gptel-make-preset 'research
                     :description "Research — autonomous web search, bounded"
                     :temperature 0.6
                     :pre (lambda ()
                            (gptel-mcp-connect '("searxng") 'sync nil))
                     :use-tools t :tools '("searxng_web_search" "web_url_read" "searxng_instance_info" "searxng_search_suggestions")
                     :system "You are a research assistant. Plan, then perform AT MOST 10 searches, reading the most relevant results, then synthesize a sourced answer. Stop searching once you can answer. You can use search engine suggestions to find related information.")

  (gptel-make-preset 'programmer
                     :description "Careful senior programmer — precise, fs-read + web"
                     :temperature 0.7
                     :pre (lambda ()
                            (gptel-mcp-connect '("searxng") 'sync nil))
                     :use-tools t
                     :tools '("read_file" "list_directory" "searxng_web_search" "web_url_read")
                     :system "You are a careful senior programmer. Reason carefully about design tradeoffs. Use file reads and web search to ground claims. Try to provide code and only code as output without any additional text, prompt or note. If you cannot provide only code, be clear and concise.")

  (gptel-make-preset 'architect
                     :description "Architecture/brainstorm — precise, fs-read + web"
                     :temperature 0.7
                     :pre (lambda ()
                            (gptel-mcp-connect '("searxng") 'sync nil))
                     :use-tools t
                     :tools '("read_file" "list_directory" "searxng_web_search" "web_url_read")
                     :system "You are a senior software architect. Reason carefully about design tradeoffs. Use file reads and web search to ground claims.")

  (gptel-make-preset 'rag
                     :description "Document RAG — low temp, grounded"
                     :temperature 0.1
                     :pre (lambda ()
                            (gptel-mcp-connect '("searxng") 'sync nil))
                     :use-tools t
                     :tools '("read_file" "list_directory" "searxng_web_search" "web_url_read")
                     :system "Answer strictly from retrieved context (corpus or fetched pages). If the sources don't contain the answer, say so. Do not speculate. Only provide information from your context.")

  ;; A custom function to open a single gptel session
  (defun perso/gptel ()
    "Wrapper to load gptel"
    (interactive)
    (gptel "GPTel")
    (switch-to-buffer "GPTel")
    (disable-text-analysis-modes)
    (delete-other-windows))

  (use-package gptel-quick
    :quelpa (gptel-quick :repo "karthink/gptel-quick" :fetcher github :commit "master")
    :after gptel
    :bind ("C-z q" . gptel-quick)
    :config
    (setq gptel-quick-backend
          (gptel-make-openai "llama-cpp-quick"
            :stream t
            :protocol "http"
            :host (concat perso/llm-host-backup ":" perso/llm-port-backup)
            :models '(qwen35-4b)
            :request-params '(:chat_template_kwargs (:enable_thinking :json-false)
                                                    :temperature 0))
          gptel-quick-model 'qwen35-4b
          gptel-quick-timeout 30))

  (when (file-directory-p "~/.emacs.d/prompts")
    (use-package gptel-prompts
      :quelpa (gptel-prompts :repo "jwiegley/gptel-prompts" :fetcher github :commit "main")
      :after (gptel)
      :demand t
      :config
      (gptel-prompts-update)
      ;; Ensure prompts are updated if prompt files change
      (gptel-prompts-add-update-watchers)))
  :bind (("C-z g" . gptel-menu)
         ("C-z C-g" . perso/gptel)))

(use-package gptel-agent
  :after gptel
  :config
  (let ((my-agents (expand-file-name "gptel-agents/" user-emacs-directory)))
    (unless (file-directory-p my-agents)
      (make-directory my-agents t))
    (add-to-list 'gptel-agent-dirs my-agents))

  ;; OPTIONAL token-saver: run *sub-agents* on a cheaper/faster model
  ;; (setq gptel-agent-preset '(:backend "llama-cpp-back" :model qwen35-4b))

  (gptel-agent-update))

(use-package eca
  :custom
  (eca-chat-use-side-window t)
  (eca-chat-window-side 'left)
  (eca-chat-window-width 0.35)
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :hook (eca-chat-mode . disable-text-analysis-modes))

;; LLM mgmt is in another dedicated file
(when (file-exists-p (expand-file-name "llm.el" user-emacs-directory))
  (autoload 'perso/llm-menu (expand-file-name "llm.el" user-emacs-directory) nil t)
  (global-set-key (kbd "C-z @") #'perso/llm-menu))

;; ;; Claude code integration
;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-z c" . claude-code-ide-menu)
;;   :custom
;;   (claude-code-ide-window-side 'left)
;;   (claude-code-ide-window-width 80)
;;   :config
;;   (use-package vterm
;;     :ensure t)
;;   (claude-code-ide-emacs-tools-setup))

;; Emacs-websearch
;; looking up stuff on the Internet
(use-package emacs-websearch
  :quelpa (emacs-websearch :repo "zhenhua-wang/emacs-websearch" :fetcher github :commit "master")
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
  (find-file "~/.emacs.d/init.el"))
(bind-key "C-z C-e" #'open-init-file)

;; Quick init.el reload
(defun reload-init-file ()
  "Reload the Emacs configuration"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Quick access to manpages
(global-set-key (kbd "C-z m") 'woman) ; Man pages

;;; X11 / Windows configuration
;; We need a wrapper and hook because emacs --daemon won't load fonts
(defun apply-gui-stuff ()
  (interactive)
  (when (display-graphic-p)
    ;; Adjust font size and shortcuts
    (set-frame-font "DejaVu Sans Mono-13" nil t)
    (global-set-key (kbd "C-=") #'text-scale-increase)
    (global-set-key (kbd "C--") #'text-scale-decrease)
    ;; Disable dialog box (if using X or Windows)
    (setq use-dialog-box nil)
    ;; X11 Alt is Meta
    (setq x-alt-keysym 'meta)
    ;; Smooth scrolling (Emacs <= 29.1)
    ;; (when (fboundp 'pixel-scroll-precision-mode)
    ;; (pixel-scroll-precision-mode t))
    ;; Vertical Scroll
    (setq scroll-step 1)
    (setq scroll-margin 0)
    (setq scroll-conservatively 101)
    (setq scroll-up-aggressively 0.01)
    (setq scroll-down-aggressively 0.01)
    (setq auto-window-vscroll nil)
    (setq fast-but-imprecise-scrolling nil)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
    (setq mouse-wheel-progressive-speed nil)
    ;; Copy when selecting region
    (setq mouse-drag-copy-region t)
    ;; Horizontal Scroll
    (setq hscroll-step 1)
    (setq hscroll-margin 1)
    ;; Fix highlight-indent-guide visual glitch when started by daemon (not used anymore)
    ;; (highlight-indent-guides-auto-set-faces)
    ;; Fix which-key settings not applied when started by daemon
    (which-key-setup-side-window-right)
    ;; Fix company-box breaking completion when started by daemon

    ))

(if (display-graphic-p)
    (apply-gui-stuff))
(add-hook 'server-after-make-frame-hook #'apply-gui-stuff)

;; from https://emacs.stackexchange.com/a/19047
(add-hook 'replace-update-post-hook 'recenter)

;; Ultra-scroll
;; Supposedly faster scroll
(use-package ultra-scroll
                                        ;:load-path "~/code/emacs/ultra-scroll" ; if you git cloned
  :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
  :init
  (setq scroll-conservatively 101 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

;;; Color themes
;; Zenburn color theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;;; Startup time
;; Let's finish loading this file by displaying how much time we took to start
(defun display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;;; Only for debugging purpose
;; (setq debug-on-error t)
;; (setq debug-on-quit t)

(provide 'init)
;;; init.el ends here
