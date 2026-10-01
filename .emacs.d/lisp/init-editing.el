;;; init-editing.el --- Editing commands and settings, undo, flymake core -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-completion.

;;; Code:

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
(use-package nhexl-mode
  ;; Loaded on first use: hexl copies the C-x and C-c keymaps when it loads
  :defer t)

;; Iedit : editing multiple regions simultaneously
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)

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
(bind-key "C-z P" #'full-path-to-clipboard)

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
  (flymake-popon-method (if (or (display-graphic-p) (featurep 'tty-child-frames))
                            'posframe
                          'popon))
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
    ;; Only active modes are turned off: the packages may be absent
    (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
    (when (bound-and-true-p jinx-mode) (jinx-mode -1))
    (when (bound-and-true-p typo-mode) (typo-mode -1))
    (message "Text analysis modes disabled")))
(bind-key "C-z t" #'perso/toggle-text-analysis-modes)

;; Small function to disable all text analysis modes
(defun disable-text-analysis-modes ()
  "Explicitely disable flycheck, jinx and typo"
  (interactive)
  ;; Only active modes are turned off: the packages may be absent
  (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
  (when (bound-and-true-p jinx-mode) (jinx-mode -1))
  (when (bound-and-true-p typo-mode) (typo-mode -1)))

;; Small collection of function and parameters to make the current buffer as fast as possible
(defun custom-fast-mode ()
  "Disable stuff and change parameters to be faster"
  (interactive)
  (disable-text-analysis-modes)
  (display-line-numbers-mode -1)
  (when (bound-and-true-p corfu-mode) (corfu-mode -1)))
(bind-key "C-z f" #'custom-fast-mode)

;; from https://emacs.stackexchange.com/a/19047
(add-hook 'replace-update-post-hook 'recenter)

;; Imenu
(use-package imenu
  :custom
  (imenu-auto-rescan t))

;; Indentation: spaces, 4 columns (language offsets are in init-dev.el)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; electric-indent is globally on by default; disable it only where unwanted,
(dolist (hook '(text-mode-hook erc-mode-hook))
  (add-hook hook (lambda () (electric-indent-local-mode -1))))

(provide 'init-editing)
;;; init-editing.el ends here
