;;; init.el --- -*- lexical-binding: t -*-
;;; Emacs startup configuration file

;;; Stuff to explore later
;; Auto update package (throws a warning about double package-initialize...)
;; (use-package auto-package-update
;;   :if (not (daemonp))
;;   :custom
;;   (auto-package-update-interval 7) ;; in days
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-delete-old-versions t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe))
;; show parentheses out of reach (https://with-emacs.com/posts/ui-hacks/show-matching-lines-when-parentheses-go-off-screen/)
;; Check out random stuff : tramp, crux
;; Maybe something to automatically add headers
;; Bindings to learn :
;; - treemacs
;; - projectile
;; - org-mode
;; Custom C / C++ style definition
;; Org-mode
;; - Disable unused modules to speedup startup ?
;; - org-super-agenda
;; - org-fancy-priorities
;; General startup speed optimizations
;; Properly configure Web mode
;; Visual-regexp (https://github.com/benma/visual-regexp.el)
;; Have a look at Perspective (https://github.com/nex3/perspective-el and https://alhassy.github.io/emacs.d/#Having-a-workspace-manager-in-Emacs)
;; Try eglot + flymake as an alternative to lsp-mode + flycheck
;; Removed color-rg, could try again if needed

;; Disabling native-compilation warnings
(setq native-comp-async-report-warnings-errors nil)

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
      '(("melpa-stable" . 10)
	    ("nongnu" . 7)
	    ("elpa" . 5)
        ("melpa"        . 5)))
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
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package  quelpa
  :ensure t
  :init
  (setq quelpa-update-melpa-p nil) ; no auto update (faster startup)
  )

(use-package  quelpa-use-package
  :ensure t)

;;; Early packages
;; Use Garbage collector magic hack ASAP
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Diminish : reduces info about modes in bottom bar
(use-package diminish
  :config
  (diminish 'visual-line-mode))

;;; Unbinding unneeded keys that will be bound by upcoming packages
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)

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

;; When buffer is closed, saves the cursor location
(save-place-mode 1)
;; Set history-length longer
(setq-default history-length 1000)

;; Recentf : recent files history
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-menu-items 20480)
  (recentf-max-saved-items 20480)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     ;; org archive? (.org_archive)
                     "COMMIT_EDITMSG\\'"))
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list))

;;; File manipulation
;; Dired : directory browsing
(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump))
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
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  ;; Probe ls for capabilities
  (dired-use-ls-dired 'unspecified)
  (dired-omit-files "^\\...+$\\|\\`[.]?#\\|\\`[.][.]?\\'")
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  ;; open with external application
  (defun dired-open-external ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (define-key dired-mode-map (kbd "C-<return>") #'dired-open-external)
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

  (use-package diredfl
    :config
    (setq
     diredfl-dir-heading '(:foreground ,zenburn-green)
     diredfl-dir-name '(:foreground ,zenburn-green))
    :hook (dired-mode . diredfl-mode))

  (use-package dired-git-info
    :custom
    (dgi-auto-hide-details-p nil)
    :bind (:map dired-mode-map (")" . dired-git-info-mode))
    )

  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "M-RET") #'dired-find-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil)
  :init
  (advice-add 'all-the-icons-dired--icon :around #'all-the-icons-pad-families-on-tty-advice+))

;; Shell : Inferior shell mode
(use-package shell
  :custom
  (comint-process-echoes 0)
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
  ("C-z t" . treemacs)
  :config
  (treemacs-project-follow-mode t)
  :init
  (use-package treemacs-all-the-icons
    :ensure t)
  ;; Using all-the-icons-dired instead
  ;; (use-package treemacs-icons-dired
  ;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
  ;;   :ensure t
  ;;   )
  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t
    )
  (use-package treemacs-magit
    :after (treemacs magit)
    :ensure t
    ))

;; TRAMP
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  :init
  (use-package ibuffer-tramp
    :hook
    (ibuffer-mode . ibuffer-tramp-set-filter-groups-by-tramp-connection)
    )
  :config
  (setq password-cache-expiry 300))

;;; "Main" packages that provide major features
;; Prescient : sorting et predicting algorithm
(use-package prescient
  :demand t
  :custom
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1)
  )

;; Ivy, counsel and swiper : completion and search
(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-s" . swiper-isearch)
   ("C-z g" . counsel-rg)
   ("C-z r" . counsel-recentf)
   ("C-z b" . counsel-buffer-or-recentf)
   ("C-z C-b" . counsel-ibuffer)
   (:map ivy-minibuffer-map
         ("M-RET" . ivy-immediate-done)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "[%d/%d] ")
  (ivy-wrap t))

;; Ivy-prescient : prescient for ivy (must be loaded after counsel)
(use-package ivy-prescient
  :after prescient
  :hook (ivy-mode . ivy-prescient-mode))

;; Projectile : project management
(use-package projectile
  :diminish
  :bind
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;; Magit : Git interface
(use-package magit
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))

;; Rg : ripgrep search
(use-package rg
  :if (executable-find "rg")
  :quelpa (rg :repo "dajva/rg.el" :fetcher github :commit "master")
  :bind
  (("C-z C-r" . rg-menu))
  )

;;; Buffer and window management
(defun pt/split-window ()
  "Split a window."
  (interactive)
  (split-window-right)
  (balance-windows))
(bind-key "C-c 2" #'pt/split-window)

(defun pt/split-window-thirds ()
  "Split a window into thirds."
  (interactive)
  (split-window-right)
  (split-window-right)
  (balance-windows))
(bind-key "C-c 3" #'pt/split-window-thirds)

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

;; Ace-window : window selection & management
(use-package ace-window
  :bind ("C-x C-o" . ace-window))

;; Buffer-move : swap buffer positions
(use-package buffer-move
  :config
  (setq buffer-move-stay-after-swap t)
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

;; Ibuffer : buffer management and sorting
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  ;; Ibuffer-vc : allows grouping by project
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
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
  :config
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

;;; Help
;; Helpful: help menu replacement
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  )

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
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" 'delete-backward-char)

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
  :hook (prog-mode . ws-butler-mode))

(defun pt/eol-then-newline ()
  "Go to end of line, then newline-and-indent."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(define-key prog-mode-map (kbd "M-<return>") #'pt/eol-then-newline)

;; Insert char by name
(bind-key "C-z e i" #'insert-char)

;; Expand-region : incrementally select region
(use-package expand-region
  :bind ("C-+" . er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-z e m" . #'mc/edit-lines)
   ("C-z e d" . #'mc/mark-all-dwim)))

;; Move-text: move text with M-<arrows> a-la org
(use-package move-text
  :config (move-text-default-bindings))

(use-package change-inner
  :diminish
  :bind (("M-i" . #'change-inner)
         ("M-o" . #'change-outer)))

(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2))

(use-package ediff
  :defer
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-diff-options "-w")
  )

;; Nhexl-mode : better hex editor
(use-package nhexl-mode)

;; Iedit : editing multiple regions simultaneously
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)

;; Pos-tip : required by other modules
;; at least popup-kill-ring and company-quickhelp
(use-package pos-tip
  :pin melpa ;; the good version is on melpa
  )

;; Pop-up kill ring
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

;; Custom function to swap clipboard with content
(defun clipboard-swap () "Swaps the clipboard contents with the highlighted region"
       (interactive)
       (if (use-region-p)
           (progn
             (setq
              reg-beg (region-beginning)
              reg-end (region-end))
             (deactivate-mark)
             (goto-char reg-end)
             (clipboard-yank)
             (clipboard-kill-region reg-beg reg-end))
         (clipboard-yank)))
(global-set-key (kbd "C-z y") 'clipboard-swap) ; Yank with the Shift key to swap instead of paste.

;; Undo-tree
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  ;; Undos are stored on disk in a subdirectory of the user directory
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undos" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t)
  :bind
  ("C-z u" . undo-tree-visualize))

;; Sudo-edit : simple commands for privileged editing
(use-package sudo-edit
  :commands (sudo-edit))

;;; Visual configuration
;; Show line numbers on the left
(require 'linum)
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%3d \u2502 ")

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; make characters after column 80 purple
(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style
        (quote (face trailing tab-mark space-before-tab)))
  (defun perso/whitespace-lines-tail ()
    "Toggle whitespace line tails highlighting"
    (interactive)
    (whitespace-toggle-options 'lines-tail))
  (bind-key "C-z l" #'perso/whitespace-lines-tail)
  )

(add-hook 'prog-mode-hook 'whitespace-mode)
;; (add-hook 'find-file-hook 'whitespace-mode)
;; also display column number
(setq column-number-mode t)

;; whitespace-mode to view all whitespace characters
(setq show-trailing-whitespace t)

;; Shortcut to clean whitespaces
(global-set-key (kbd "C-z w") 'whitespace-cleanup)

;; Wrap lines in compilation and flycheck buffers
(add-hook 'compilation-mode-hook 'visual-line-mode)
(add-hook 'flycheck-error-list-mode-hook 'visual-line-mode)

;; Page-break-lines : enable to show ^L as straight horizontal lines
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;;All-the-icons : unified icon pack
;; Requires manually installing the fonts with M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package beacon
  :diminish
  :config (setq beacon-color "#5F7F5F")
  :hook   ((org-mode text-mode prog-mode) . beacon-mode))

(use-package dimmer
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :custom
  (dimmer-fraction 0.2)
  :config
  (dimmer-mode))

(use-package centered-window
  :bind
  ("C-z c" . centered-window-mode)
  :custom
  (cwm-centered-window-width 120))

;;; Languages and spell-checking
;; Flycheck
(use-package flycheck
  :diminish
  :hook
  (after-init . global-flycheck-mode)
  :custom
  (flycheck-checker-error-threshold nil)
  :config
  (use-package flycheck-pos-tip
    :config
    (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))
  )

;; Guess-language : automatic language detection
(use-package guess-language
  :defer t
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :init (add-hook 'flycheck-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_US" "English" "🇺🇸" "English"))
                                   (fr . ("fr_FR"  "French" "🇫🇷" "French")))
        guess-language-languages '(en fr)
        guess-language-min-paragraph-length 35)
  :diminish guess-language-mode)

;; Flyspell : on-the-fly spell-checking
(use-package flyspell
  :ensure nil
  :if (executable-find "hunspell")
  :hook (((text-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode)
	     (prog-mode . flyspell-prog-mode))
  :bind
  ("C-c e" . (lambda () (interactive)
	           (ispell-change-dictionary "en_US") (typo-change-language "English") (flyspell-buffer)))
  ("C-c f" . (lambda () (interactive)
	           (ispell-change-dictionary "fr_FR") (typo-change-language "French") (flyspell-buffer)))
  :init
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary "~/.hunspell_personal")
  :config
  (use-package flyspell-correct-ivy ; M-o to access actions
    :after ivy
    :bind
    (:map flyspell-mode-map
          ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper)
          ("C-." . flyspell-correct-wrapper))
    :custom (flyspell-correct-interface #'flyspell-correct-ivy))
  ;; Typo: auto-replace typographically useful unicode characters
  (use-package typo
    :diminish
    :hook
    ((org-mode text-mode) . typo-mode)))

;; Small hook so Flyspell skip mail headers in mu4e
(defun flyspell-skip-mail-headers (begin _end _ignored)
  "Returns non-nil if BEGIN position is in mail header."
  (save-excursion
    (goto-char (point-min))
    (let ((end-header
           (re-search-forward "^--text follows this line--[[:space:]]*$" nil t)))
      (when end-header
        (< begin end-header)))))
(add-hook 'flyspell-incorrect-hook #'flyspell-skip-mail-headers)

;; Flycheck-grammalecte : french syntax checking
;; May require running grammalecte-download-grammalecte once
(use-package flycheck-grammalecte
  :ensure t
  :after flycheck
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
  (grammalecte-download-grammalecte)
  (flycheck-grammalecte-setup))

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

;; Small function to disable all text analysis modes
(defun disable-text-analysis-modes ()
  "Explicitely disable flycheck, flyspell and typo"
  (interactive)
  (flycheck-mode -1)
  (flyspell-mode -1)
  (typo-mode -1))

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

(defun smart-electric-indent-mode ()
  "Disable 'electric-indent-mode in certain buffers and enable otherwise."
  (cond ((and (eq electric-indent-mode t)
              (member major-mode '(erc-mode text-mode)))
         (electric-indent-mode 0))
        ((eq electric-indent-mode nil) (electric-indent-mode 1))))
(add-hook 'post-command-hook #'smart-electric-indent-mode)

;; Highlight-indent-guides : show indentation level
(use-package highlight-indent-guides
  :diminish
  ;; Automatically enabled, but there is a bug that might require to disable it:
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/76
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0))

;; Show current function in mode bar
(add-hook 'prog-mode-hook #'which-function-mode)

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
    '(objed-state misc-info persp-name grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker " "))
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
  (let ((c-like-modes-list '(c-mode c++-mode java-mode perl-mode)))
    (sp-local-pair c-like-modes-list "(" nil
                   :post-handlers '(:add add-paren-dwim))
    (sp-local-pair c-like-modes-list "{" nil
                   :post-handlers '(:add open-block-dwim)))

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
          (message line)
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
       ,@(loop for (key . val) in pairs
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
  ;; I like my pointers like this: char * var;
  (electric-operator-add-rules-for-mode 'c-mode
                                        (cons "{" " {")
                                        (cons "*" " * "))
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
  :diminish
  :init
  (use-package yasnippet-snippets :after yasnippet)
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ([(tab)] . nil))
  (:map yas-minor-mode-map ("TAB" . nil))
  (:map yas-minor-mode-map ("<tab>" . nil))
  ("C-z C-s" . yas-insert-snippet)
  :config
  (yas-reload-all))

;; Dumb-jump : simple "jump to definition" tool
(use-package dumb-jump
  :bind
  ("C-c C-j" . dumb-jump-go)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'ivy)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Company : completion anywhere
(use-package company
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :diminish
  :bind
  ("<backtab>" . company-complete)
  ("C-<tab>" . company-other-backend)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 10)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence))
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  ;; invert the navigation direction if the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  :hook
  (after-init . global-company-mode)
  (company-mode . company-tng-mode)
  :config
  (setq company-backends
        '((company-capf company-dabbrev :separate :with company-yasnippet)
          company-files
          ))
  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-capf
                             company-dabbrev
                             company-ispell
                             :separate)
                            company-files))
              ))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-capf
                             company-dabbrev
                             company-ispell
                             company-files
                             :separate :with company-yasnippet)
                            company-files))
              ))
  )

;; Company-prescient
(use-package company-prescient
  :after prescient
  :hook (company-mode . company-prescient-mode))

(use-package company-c-headers
  :after (company)
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-c-headers)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-c-headers)))
  (add-hook 'objc-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-c-headers)))
  )

;;; IDE-like features

;; LSP-mode : main IDE features
(use-package lsp-mode
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :defer t
  :custom
  (lsp-keymap-prefix "C-x l")
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-idle-delay 0.5)
  (lsp-lens-enable nil)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-indentation nil)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-links nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-enable-semantic-highlight nil) ;; managed by color-identifiers-mode
  (lsp-completion-provider :none) ;; managed by company-mode
  :config
  (use-package lsp-treemacs
    :pin melpa) ;; the good version is on melpa, not melpa-stable

  (setq lsp-clients-clangd-args '("-j=2"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--pch-storage=memory"
                                  "-background-index"
                                  "-log=error"))
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook
  ((java-mode python-mode go-mode rust-mode js-mode js2-mode
              typescript-mode web-mode c-mode c++-mode objc-mode php-mode) . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :defer t
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :custom
  ;; Let's start by enabling/disabling features
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-doc-enable nil)
  ;; We prefer the less intruding binding to lsp-ui-doc-glance
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; (https://github.com/emacs-lsp/lsp-ui/issues/243)
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; M-.
        ([remap xref-find-references] . lsp-ui-peek-find-references) ; M-?
        ("C-z i" . lsp-ui-doc-glance)
        ("C-z z i" . lsp-ui-doc-focus-frame))
  :commands
  lsp-ui-mode)

;; Lsp-pyright : lsp-mode integration for Python
(use-package lsp-pyright
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :ensure t)

;; Lsp-java : lsp-mode integration for Java
(use-package lsp-java
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :after lsp-mode
  :if (executable-find "mvn")
  :init
  (use-package request :defer t) ; also requires treemacs?
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/"))
  )

;; /!\ FIXME DAP-MODE /!\
(use-package dap-mode
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :after (lsp-mode pyenv-mode)
  :commands dap-debug
  :diminish
  :config
  (require 'dap-java)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
;;   :config
;;   ;; Could not manage to make any of the following work...
;;   ;; (require 'dap-firefox)
;;   ;; (require 'dap-edge)
;;   ;; (require 'dap-chrome)
;;   ;; (require 'dap-node)
;;   (require 'dap-python)
;;   (require 'dap-lldb)
;;   (require 'dap-php)
;;   (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
;;   (setq dap-python-debugger 'debugpy)
)

;;; Elisp modes and settings
;; Highlight-defined for colored Elisp symbols
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;; Python-specific modes and settings
;; Python settings
(use-package python
  :config
  ;; I usually prefer a dedicated terminal, so maybe remove it
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i --pprint"
        python-indent-offset 4
        eldoc-documentation-function #'ignore
        )
  )

;; Python-mode settings
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  )

;; Pyenv : managing python version/venv with pyenv and pyenv-virtualenv
;; Had to fork it to make it buffer-local because it has global keybindings
;; that conflict with org-mode
(use-package pyenv-mode
  :ensure nil
  :quelpa (pyenv-mode :repo "mclbn/pyenv-mode" :fetcher github :commit "master")
  :diminish
  :after projectile
  :config
  (defun pyenv-detect-env ()
    "Try to identify pyenv via projectile, then .python-version."
    (interactive)
    (if (and (projectile-project-name)(member (projectile-project-name) (pyenv-mode-versions)))
        (projectile-project-name)
      (let ((pyenv-file (concat (projectile-project-root) ".python-version")))
        (if (and (file-exists-p pyenv-file))
            (let ((pyversion (first (split-string (f-read-text pyenv-file) "\n" t))))
              (if (member pyversion (pyenv-mode-versions))
                  (first (split-string (f-read-text pyenv-file) "\n" t))
                nil))
          nil))))
  (defun pyenv-set-env ()
    "Try to identify and set pyenv."
    (interactive)
    (let ((pyenv-name (pyenv-detect-env)))
      (if pyenv-name
          (pyenv-mode-set pyenv-name)
        (pyenv-mode-set "default"))))
  (add-hook 'python-mode-hook 'pyenv-set-env)
  ;; We will need this at some point
  (use-package with-venv)
  :hook (python-mode . pyenv-mode)
  :init
  (let ((pyenv-path (expand-file-name "~/.pyenv/bin")))
    (setenv "PATH" (concat pyenv-path ":" (getenv "PATH")))
    (add-to-list 'exec-path pyenv-path))
  ;; is the following line needed ?
  (add-to-list 'exec-path "~/.pyenv/shims")
  (pyenv-mode-set "default"))

;;; C / C++ / Objective-C modes and settings
;; Create my personal style.
(defconst my-c-style
  '((c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (indent-tabs-mode . nil)
    (c-basic-offset . 2)
    (c-comment-only-line-offset . 0))
  "My C Programming Style")
(c-add-style "perso" my-c-style)

(defun my-c-mode-common-hook ()
  (c-set-style "perso"))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Emacs-ccls, compiled from source (https://github.com/MaskRay/ccls)
(use-package ccls
  :config
  (setq ccls-executable (expand-file-name "~/src/ccls/Release/ccls"))
  (setq ccls-args '("--log-file=/tmp/ccls.log"))
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; Arduino / Teensy specific C / C++ code
(use-package platformio-mode
  :diminish
  :config
  (add-hook 'c++-mode-hook (lambda ()
                             (lsp-deferred)
                             (platformio-conditionally-enable)))
  )

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
  (use-package flycheck-rust
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

;;; Assembly modes and settings
;; asm-mode settings
(add-hook 'asm-mode-hook (lambda ()
                           (setq indent-tabs-mode nil) ; use spaces to indent
                           (electric-indent-mode nil) ; disable auto-indent on RET
                           ))

;;; Android development modes and settings
;;; smali/baksmali mode (https://github.com/strazzere/Emacs-Smali)
(use-package smali-mode
  :load-path (lambda () (expand-file-name "Emacs-Smali" user-emacs-directory))
  :config
  (add-to-list 'auto-mode-alist '(".smali$" . smali-mode)))

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

;; CSV modes and settings
(use-package csv-mode)

;;; Markdown modes and settings
;; Markdown-mode
(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  )
;; Vmd-mode : alternative markdown live preview
(use-package vmd-mode
  :defer t)

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
  (org-startup-indented nil)
  (org-adapt-indentation nil)
  (org-startup-truncated nil)
  (org-startup-folded 'overview)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
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
  (org-blank-before-new-entry (quote ((heading . audo)
                                      (plain-list-item . auto))))
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-catch-invisible-edits 'smart)
  (org-use-property-inheritance nil) ; for performance
  (org-cycle-separator-lines 2)
  (org-id-link-to-org-use-id t)
  (org-latex-listings t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  :bind
  ("C-z a" . org-agenda)
  ("C-c l" . org-store-link)
  :config
  (if (string= (getenv "EMACS_WORK") "Y")
      (progn
        (defun perso/org-work-files ()
          (seq-filter (lambda(x) (not (string-match "/templates/"(file-name-directory x))))
                      (directory-files-recursively "~/org-work" "\\.org$")))
        (setq org-directory "~/org-work")
        (setq org-agenda-files (perso/org-work-files))
        (setq org-refile-targets '((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
    (progn
      (setq org-directory "~/org")
      (setq org-agenda-files '("perso.org" "work.org" "notes.org" "cloudcal-perso.org" "cloudcal-work.org"))
      (setq org-refile-targets `((nil :maxlevel . 9)
                                 (("perso.org" "work.org" "notes.org") :maxlevel . 9)))))
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

  (add-hook 'org-mode-hook  #'which-function-mode)
  )
;; ;; company compatibility (https://github.com/company-mode/company-mode/issues/50)
;; (defun add-pcomplete-to-capf ()
;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

;; FIXME : should be inside org-mode config block (?)
(when (executable-find "gpg")
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key "511079E5FEC0BA66B53C9A625D01D510BEBDD2FF")
  (require 'epa-file)
  (epa-file-enable)
  )

;; FIXME : should be inside org-mode config block (?)
(use-package org-sidebar
  :quelpa (org-sidebar :fetcher github :repo "alphapapa/org-sidebar")
  :bind ("C-z o t" . org-sidebar-tree-toggle)
  :custom
  (org-sidebar-tree-side 'right)
  (org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source))

;; FIXME : should be inside org-mode config block (?)
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
;; FIXME : should be inside org-mode config block (?)
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
;; FIXME : should be inside org-mode config block (?)
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
                                     ))
  )

;; FIXME : should be inside org-mode config block (?)
(use-package org-super-links
  :quelpa (org-super-links :repo "toshism/org-super-links" :fetcher github :commit "develop")
  :bind (("C-c s s" . sl-link)
	     ("C-c s l" . sl-store-link)
	     ("C-c s C-l" . sl-insert-link)))

;; FIXME : should be inside org-mode config block (?)
(use-package org-edna
  :config
  (require 'org-edna)
  (org-edna-load))

;; FIXME : should be inside org-mode config block (?)
(use-package org-linker
  :quelpa (org-linker :repo "toshism/org-linker" :fetcher github :commit "master"))

;; FIXME : should be inside org-mode config block (?)
(use-package org-linker-edna
  :quelpa (org-linker-edna :repo "toshism/org-linker-edna" :fetcher github :commit "master")
  :bind (("C-c s e" . org-linker-edna)) ;; follows org-super-link binding patterns
  )

;; FIXME : should be inside org-mode config block (?)
(use-package org-books
  :custom
  (org-books-file "~/org/books.org")
  (org-books-file-depth 1)
  )

;; FIXME : should be inside org-mode config block (?)
(use-package orgmdb
  :ensure t
  :diminish
  :quelpa (orgmdb :fetcher github :repo "isamert/orgmdb.el")
  :bind
  (("C-z o m" . orgmdb-fill-movie-properties))
  :custom
  (orgmdb-omdb-apikey "ce9ed4b5")
  (orgmdb-fill-property-list '(title released genre runtime rated director writer actors plot country language imdb metacritic tomatometer imdb-id))
  )

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
        (mu4e-get-mail-command "mbsync protonmail gmail")
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
        ;; Since we are using Ivy:
        (mu4e-completing-read-function 'ivy-completing-read)
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
      (require 'mu4e)
      (setq mail-user-agent 'mu4e-user-agent)
      ))

;;; File/mode associations
;; Script-shell-mode on zsh
(add-to-list 'auto-mode-alist '(".zsh$" . shell-script-mode))
;; .in and .out are text by default
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
;; Arduino is C++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


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
    (when (fboundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t))
    ;; Vertical Scroll
    (setq scroll-step 1)
    (setq scroll-margin 1)
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
    ;; Fix highlight-indent-guide visual glitch when started by daemon
    (highlight-indent-guides-auto-set-faces)
    ;; Fix which-key settings not applied when started by daemon
    (which-key-setup-side-window-right)
    ;; Fix company-box breaking completion when started by daemon

    ))

(if (display-graphic-p)
    (apply-gui-stuff))
(add-hook 'server-after-make-frame-hook #'apply-gui-stuff)

;;; Color themes
;;; Zenburn color theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; Needed for proper hl-line-mode
     `(hl-line-face ((t (:background ,zenburn-bg+1 ))))
     `(hl-line ((t (:background ,zenburn-bg+1 ))))
     ;; Dired stuff
     ;; `(diredfl-dir-name ((t (:foreground ,zenburn-blue+1 :weight bold))))
     ;; `(diredfl-dir-name ((t (:background ,nil))))
     ;; `(diredfl-dir-heading ((t (:foreground ,zenburn-blue-1))))
     ;; `(diredfl-dir-heading ((t (:background ,nil))))
     )))

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
