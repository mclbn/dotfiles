;;; init-completion.el --- Minibuffer and in-buffer completion, snippets -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-interface.

;;; Code:

;; Prescient : sorting and predicting algorithm
(use-package prescient
  :demand t
  :custom
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1)
  )

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
  :vc (:url "https://github.com/Ladicle/consult-tramp" :branch main :rev :newest)
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
(unless (featurep 'tty-child-frames)
  (use-package corfu-terminal
    :after corfu
    :config (corfu-terminal-mode 1)))

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
  :demand t ; load now: the per-mode completion setup is in :config
  :bind ("C-z c" . cape-prefix-map)
  :custom
  (cape-dabbrev-check-other-buffers t)
  :init
  ;; Emacs 30+ adds `ispell-completion-at-point' to every text/org buffer via
  ;; `text-mode-ispell-word-completion'. Corfu calls it on each keystroke, and
  ;; it errors when no plain word-list is configured -- one Corfu backtrace in
  ;; *Messages* per keypress. Jinx owns spell-checking here and `cape-dict'
  ;; (below) owns dictionary completion, so retire that capf entirely.
  (setq text-mode-ispell-word-completion nil)

  ;; Baseline for buffers the per-mode hooks below don't touch (conf, special…)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  ;; Plain word lists for `cape-dict' (Debian: wamerican/wfrench, Arch: words…).
  ;; Prefer the language-specific files: /usr/share/dict/words is often a
  ;; symlink repointed according to the system locale.
  (defvar perso/cape-dict-files
    (or (seq-filter #'file-readable-p
                    '("/usr/share/dict/american-english"
                      "/usr/share/dict/french"))
        (seq-filter #'file-readable-p '("/usr/share/dict/words")))
    "Existing plain word lists for `cape-dict', in completion order.")

  (if perso/cape-dict-files
      (setq cape-dict-file  perso/cape-dict-files
            ;; grep's -m<limit> truncates in *file* order, which for a sorted
            ;; list drops the obvious matches; let Orderless/Prescient rank.
            cape-dict-limit nil)
    (message "cape-dict: no plain word list found in /usr/share/dict; \
install your distribution's word-list package to enable dictionary completion"))

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

  (defun perso/capf-dict ()
    "List containing `cape-dict', or nil when no word list is installed."
    (and perso/cape-dict-files (list #'cape-dict)))

  ;; Generic case: capture the capf the mode already set (Elisp, Lua, sh, markdown,
  ;; plain text…) and merge the extras onto it.
  (defun perso/capf-here ()
    (perso/capf (remq t completion-at-point-functions)))

  ;; Prose: same as above, plus the dictionary.
  (defun perso/capf-prose ()
    (perso/capf (append (remq t completion-at-point-functions)
                        (perso/capf-dict))))

  (add-hook 'prog-mode-hook #'perso/capf-here)
  (add-hook 'text-mode-hook #'perso/capf-prose)

  ;; Org: pcomplete as the primary (overrides the text-mode catch-all, runs after it)
  (add-hook 'org-mode-hook
            (lambda ()
              (perso/capf (append (list #'pcomplete-completions-at-point)
                                  (perso/capf-dict)))))

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

(provide 'init-completion)
;;; init-completion.el ends here
