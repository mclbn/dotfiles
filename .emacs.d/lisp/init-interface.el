;;; init-interface.el --- Theme, modeline, windows, buffers list, GUI frames -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-core.

;;; Code:

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

;; Page-break-lines : enable to show ^L as straight horizontal lines
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;;All-the-icons : unified icon pack
;; Requires manually installing the fonts with M-x all-the-icons-install-fonts and M-x nerd-icons-install-fonts
(use-package all-the-icons
  :pin melpa)

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
  ;; Symbolic colors (`foreground-color') are valid in :underline/:box plists
  ;; -- stock `whitespace-page-delimiter' uses one -- but dimmer passes them to
  ;; `color-defined-p', which wants a string: (wrong-type-argument stringp
  ;; foreground-color). The error aborts the dimming loop, so later faces stay
  ;; undimmed. They delegate to the foreground, already dimmed, so skip them.
  ;; TODO: drop once fixed upstream.
  (defun perso/dimmer--skip-symbolic-color (orig face attribute target frac)
    "Skip ATTRIBUTE of FACE when its :color is a symbol, else call ORIG."
    (let* ((value (face-attribute face attribute nil t))
           (color (and (listp value) (plist-get value :color))))
      (unless (and color (symbolp color) (not (booleanp color)))
        (funcall orig face attribute target frac))))

  (when (fboundp 'dimmer--dim-face-attribute)   ; private fn, may vanish
    (advice-add 'dimmer--dim-face-attribute
                :around #'perso/dimmer--skip-symbolic-color))
  (dimmer-mode))

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

  ;; --- Dired file/folder count segment ---
  (require 'dired)

  (defvar-local my/dired--count-cache nil
    "Cons (FILES . DIRS) of displayed entries in the current dired buffer.")

  (defun my/dired--refresh-count-cache ()
    "Recompute `my/dired--count-cache' from the actual directory on disk."
    (when (derived-mode-p 'dired-mode)
      (let* ((ddir (if (consp dired-directory)
                       (car dired-directory)
                     dired-directory))
             (entries (condition-case nil
                          (directory-files-and-attributes ddir nil nil nil 'nosort)
                        (error nil)))
             (files 0)
             (dirs 0))
        (dolist (e entries)
          (let ((name (car e))
                (attr (cadr e)))
            (unless (member name '("." ".."))
              (if (eq t attr)
                  (setq dirs (1+ dirs))
                (setq files (1+ files))))))
        (setq my/dired--count-cache (cons files dirs)))
      (force-mode-line-update)))

  (doom-modeline-def-segment dired-count
    "Number of files and directories shown in the current dired buffer."
    (when (and (derived-mode-p 'dired-mode)
               my/dired--count-cache)
      (propertize
       (format " %d   %d   "
               (car my/dired--count-cache)
               (cdr my/dired--count-cache))
       'face (doom-modeline-face 'doom-modeline-info))))

  (doom-modeline-def-modeline 'dired
    '(bar workspace-name window-number matches follow buffer-info dired-count remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name grip debug repl lsp minor-modes indent-info buffer-encoding major-mode process vcs check " "))

  (add-to-list 'doom-modeline-mode-alist '(dired-mode . dired))

  (add-hook 'dired-mode-hook         #'my/dired--refresh-count-cache)
  (add-hook 'dired-after-readin-hook #'my/dired--refresh-count-cache)
  (when (boundp 'dired-after-change-hook)
    (add-hook 'dired-after-change-hook #'my/dired--refresh-count-cache))
  )

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
(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f (apply-gui-stuff))))

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
  ;; Fix zenburn's invalid `:background nil' (Emacs 31) before enabling it.
  (load-theme 'zenburn t t)
  (dolist (setting (get 'zenburn 'theme-settings))
    (when (and (eq (car setting) 'theme-face)
               (eq (nth 1 setting) 'doom-modeline-bar-inactive))
      (setcar (nthcdr 3 setting) '((t (:background unspecified))))))
  (enable-theme 'zenburn))

(provide 'init-interface)
;;; init-interface.el ends here
