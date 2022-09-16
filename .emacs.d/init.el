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
;; Company :
;; - Custom lists for spellchecking (flyspell) and completion (company)
;; - ispell completion with company
;; show parentheses out of reach (https://with-emacs.com/posts/ui-hacks/show-matching-lines-when-parentheses-go-off-screen/)
;; Check out random stuff : avy, crux
;; Maybe something to automatically add headers
;; Bindings to learn :
;; - treemacs
;; - projectile
;; - org-mode
;; Custom C / C++ style definition
;; Org-mode
;; - Disable unused modules to speedup startup ?
;; General startup speed optimizations

;;; First, let's ensure that early-init is loaded on older Emacs versions
(cond ((version< emacs-version "26.1")
       (warn "Requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))

;; Disabling native-compilation warnings
(setq native-comp-async-report-warnings-errors nil)

;;; Defining constants here


;;; Personal information is stored in a non-versioned file
(defvar personal-info (concat user-emacs-directory "perso.el"))
(let ((personal-settings personal-info))
 (when (file-exists-p personal-settings)
   (load-file personal-settings))
)

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

;;; Early packages
;; Use Garbage collector magic hack ASAP
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Diminish : reduces info about modes in bottom bar
(use-package diminish)

;;; Unbinding unneeded keys that will be bound by upcoming packages

(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-}") nil)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)

;;; Main packages

;; Prescient : sorting et predicting algorithm
(use-package prescient
  :demand t
  :custom
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1)
  )

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
   ("C-z s" . counsel-rg)
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

;; Color-rg : searching & refactoring
(use-package color-rg
  :load-path (lambda () (expand-file-name "color-rg" user-emacs-directory))
  :if (executable-find "rg")
  :bind ("C-M-s" . color-rg-search-input))

;; Iedit : editing multiple regions simultaneously
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)

;; Dired : directory browsing
(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
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
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

;; Disk-usage : browse by sorting disk usage
;; Is this really useful in the scope of a text editor / IDE ?
(use-package disk-usage
  :commands (disk-usage))

;; Pop-up kill ring
(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

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

;; Undo-tree
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  ;; Undos are stored on disk in a subdirectory of the user directory
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undos" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t))

;; Ace-window : window selection & management
(use-package ace-window
  :bind ("C-x C-o" . ace-window))

;; Sudo-edit : simple commands for privileged editing
(use-package sudo-edit
  :commands (sudo-edit))

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
           " " filename))))

;;; Absolute must-have tweaks and settings
;; Disable the welcome message
(setq inhibit-startup-message t)

;; No scratch message
(setq initial-scratch-message nil)

;; Default mode is text
(setq initial-major-mode 'text-mode)

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

;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

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

;;; Backups, history and custom variables
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
                     "COMMIT_EDITMSG\\'")))

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;;; Editing experience configuration
;; A few key bindings
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-z g") 'goto-line)
(global-set-key (kbd "C-z /") 'comment-or-uncomment-region)

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

;;; Visual configuration
;; Show line numbers on the left
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d \u2502 ")

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; make characters after column 80 purple
(setq whitespace-style
  (quote (face trailing tab-mark lines-tail space-before-tab)))
(add-hook 'find-file-hook 'whitespace-mode)
;; also display column number
(setq column-number-mode t)

;; whitespace-mode to view all whitespace characters
(setq show-trailing-whitespace t)

;; Shortcut to clean whitespaces
(global-set-key (kbd "C-z w") 'whitespace-cleanup)

;;; Languages and spell-checking
;; Guess-language : automatic language detection
(use-package guess-language
  :defer t
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (fr . ("fr_FR" "Francais")))
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
	       (ispell-change-dictionary "en_US") (flyspell-buffer)))
  ("C-c f" . (lambda () (interactive)
	       (ispell-change-dictionary "fr_FR") (flyspell-buffer)))
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
    :custom (flyspell-correct-interface #'flyspell-correct-ivy)))

;;; General programming
;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;; Smartparens : auto parenthesis,  etc.
(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :custom
  (sp-escape-quotes-after-insert nil)
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

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
  :config
  (yas-reload-all))

;; Flycheck : on-the-fly syntax checking
(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode)
  (if (display-graphic-p)
      (use-package flycheck-pos-tip
	    :hook (flycheck-mode . flycheck-pos-tip-mode)))
  :config
  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode)))

;; Dumb-jump : simple "jump to definition" tool
(use-package dumb-jump
  :bind
  ("C-c C-j" . dumb-jump-go)
  :custom (dumb-jump-selector 'ivy))

;; Indentation settings
;; We indent with spaces
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

;; Quickrun : compile and run quickly
(use-package quickrun
  :custom
  (quickrun-timeout-seconds 60)
  :bind
  (("<f5>" . quickrun)
   ("M-<f5>" . quickrun-shell)
   ("C-c e" . quickrun)
   ("C-c C-e" . quickrun-shell)))

;;; Completion
;; Company : completion engine
(use-package company
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :diminish
  :init
  :bind
  ("<backtab>" . company-complete)
  ("C-<tab>" . company-other-backend)
  :custom
  (company-idle-delay 0)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  (company-transformers '(company-sort-by-occurrence))
  (company-show-numbers t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  :config
  (company-tng-configure-default)
  (if (display-graphic-p)
      (setq company-format-margin-function #'company-vscode-dark-icons-margin)
    (setq company-format-margin-function #'company-text-icons-margin)
    )
  (setq company-backends
        '((company-capf company-dabbrev :separate :with company-yasnippet)
          company-dabbrev
          company-files
          ))
  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-dabbrev company-ispell :separate)
                            company-files))
              ))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-capf
                             company-dabbrev
                             company-ispell
                             company-files
                             :separate)
                            company-files))
              ))
  (global-company-mode 1))

;; Company-quickhelp : tooltips for company
(use-package company-quickhelp
  :diminish
  :custom
  (company-quickhelp-delay 1)
  :config
  (company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :hook (company-quickhelp-mode . company-quickhelp-terminal-mode))

(use-package company-prescient
  :after prescient
  :hook (company-mode . company-prescient-mode)
  :config
   (company-quickhelp-terminal-mode 1))

;;; File navigation UI
;; Treemacs : visual tree
(use-package treemacs
  :ensure t
  :defer t
  :bind
  ("C-z t" . treemacs)
  :config
  (treemacs-project-follow-mode t)
  :init
  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :ensure t
    )
  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t
    )
  (use-package treemacs-magit
    :after (treemacs magit)
    :ensure t
    ))

;;; Advanced programming modules
;; LSP : IDE-like features
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-x l")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (lsp-enable-indentation nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate t)
  (lsp-enable-folding nil)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((java-mode python-mode go-mode rust-mode
          js-mode js2-mode typescript-mode web-mode
          c-mode c++-mode objc-mode php-mode) . lsp-deferred)
  :config
  (setq lsp-completion-provider :none)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB for better performance
  (setq lsp-idle-delay 0.5) ;; refresh adjustment for better performance
  (setq lsp-clients-clangd-args '("-j=2"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--pch-storage=memory"
                                  "-background-index"
                                  "-log=error"))
  ;; We use phpactor (with composer installed too)
  (setq lsp-disabled-clients '(php-ls iph intelephense)))

;; Lsp-ui : visual add-ons for LSP
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-border (face-foreground 'default))
  :bind
  ("C-z i" . lsp-ui-doc-glance)
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; M-.
        ([remap xref-find-references] . lsp-ui-peek-find-references) ; M-?
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-show-with-cursor t)
  ;; We prefer the less intruding binding to lsp-ui-doc-glance
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-ignore-duplicate t)
  :config
 ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
 ;; (https://github.com/emacs-lsp/lsp-ui/issues/243)
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  )

;; need DAP ?



;;; Language-specific modes and settings
;; Python
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

;; Lsp-pyright : python integration with lsp-mode
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ; or lsp-deferred
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3"))
  :custom
  (lsp-pyright-multi-root nil)
  )

;; Pyvenv : virtualenv selection
(use-package pyvenv
  :diminish
  :config
  (pyvenv-mode 1)
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs")
  )

;; Java
;; Jsp-java : lsp-mode integration
(use-package lsp-java
  :after lsp-mode
  :if (executable-find "mvn")
  :init
  (use-package request :defer t) ; also requires treemacs?
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/"))
  )

;; C / C++ / Objective-C
;; OpenBSD KNF for C/C++ ()
(add-to-list 'load-path "~/.emacs.d/openbsd-knf-emacs")
(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "bsd")))
(setq c-default-style '((c++-mode . "bsd")))

(use-package company-c-headers
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  )

;; Php
(use-package php-mode
  :ensure t)

;; Rust
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

;; Arduino / Teensy specific C / C++ code
(use-package platformio-mode
  :diminish
  :config
  (add-hook 'c++-mode-hook (lambda ()
                             (lsp-deferred)
                             (platformio-conditionally-enable)))
  )

;; asm mode configuration
(add-hook 'asm-mode-hook (lambda ()
                           (setq indent-tabs-mode nil) ; use spaces to indent
                           (electric-indent-mode nil) ; disable auto-indent on RET
                           ))

;;; smali/baksmali mode (https://github.com/strazzere/Emacs-Smali)
(use-package smali-mode
  :load-path (lambda () (expand-file-name "Emacs-Smali" user-emacs-directory))
  :config
  (add-to-list 'auto-mode-alist '(".smali$" . smali-mode)))

;; (add-to-list 'load-path "~/.emacs.d/Emacs-Smali")
;; (autoload 'smali-mode "smali-mode" "Major mode for editing and viewing smali issues" t)
;; (add-to-list 'auto-mode-alist '(".smali$" . smali-mode))

;; I3wm-config mode
(use-package i3wm-config-mode
  :ensure t)

;; Dockerfile mode
(use-package dockerfile-mode :defer t)

;;; File/mode associations
;; Script-shell-mode on zsh
(add-to-list 'auto-mode-alist '(".zsh$" . shell-script-mode))
;; .in and .out are text by default
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
;; Arduino is C++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

;;; UI Enhancements

;; Page-break-lines : enable to show ^L as straight horizontal lines
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;;; Org-mode
;; Main package and settings
(use-package org
  :ensure t
  :defer t
  :custom
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE" "CANCELED")))
  :bind
  ("C-z a" . org-agenda)
  )
  ;; ;; company compatibility (https://github.com/company-mode/company-mode/issues/50)
  ;; (defun add-pcomplete-to-capf ()
  ;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  ;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

;; Org-superstar : beautify org-mode
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-hide-leading-stars t)
  (org-superstar-special-todo-items t)
  )

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
     )))

;;; Gruvbox color theme
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox t)
;; )

;;; X11 / Windows configuration

;; Need a wrapper and hook because emacs --daemon won't load fonts
(defun apply-gui-stuff ()
  (interactive)
  (when (display-graphic-p)
    ;; Adjust font size and shortcuts
    (set-frame-font "DejaVu Sans Mono-13" nil t)
    (global-set-key (kbd "C-=") #'text-scale-increase)
    (global-set-key (kbd "C-+") #'text-scale-increase)
    (global-set-key (kbd "C--") #'text-scale-decrease)
    ;; Disable dialog box (if using X or Windows)
    (setq use-dialog-box nil)
    ;; X11 Alt is Meta
    (setq x-alt-keysym 'meta)
    ;; Smooth scrolling
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
    ;; Horizontal Scroll
    (setq hscroll-step 1)
    (setq hscroll-margin 1)
    ;; Fix highlight-indent-guide visual glitch when started by daemon
    (highlight-indent-guides-auto-set-faces)
    ;; Fix which-key settings not applied when started by daemon
    (which-key-setup-side-window-right)
    ))

(if (display-graphic-p)
    (apply-gui-stuff))
(add-hook 'server-after-make-frame-hook #'apply-gui-stuff)

;;; Final common-use bindings
(global-set-key (kbd "C-z m") 'woman) ; Man pages

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

(provide 'init)
;;; init.el ends here
