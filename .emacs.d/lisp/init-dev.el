;;; init-dev.el --- Programming: languages, LSP, debugging, compilation -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-writing.  Disabled with EMACS_NODEV=Y.

;;; Code:

;; Wrap lines in compilation and flycheck buffers
(add-hook 'compilation-mode-hook 'visual-line-mode)

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

;;; General programming
;; Indentation settings (indent-tabs-mode and tab-width are in init-editing.el)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)

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

;; Dumb-jump : simple "jump to definition" tool
(use-package dumb-jump
  :bind
  ("C-z C-j" . dumb-jump-go)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

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
  :if (executable-find "emacs-lsp-booster") ; errors without the program
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch main :rev :newest)
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
  (when-let* ((s (and (fboundp 'eglot-current-server) (eglot-current-server))))
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

;; ;; Pyenv : managing python version/venv with pyenv and pyenv-virtualenv
;; ;; Had to fork it to make it buffer-local because it has global keybindings
;; ;; that conflict with org-mode
;; (use-package pyenv-mode
;;   :ensure nil
;;   ;; NOTE: quelpa is gone; use :vc if re-enabling this.
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

;; NOTE: the `ccls' *package* (an lsp-mode client) was removed along with the
;; lsp-mode stack; C/C++ now runs on eglot. The ccls *binary* is still used --
;; see `perso/cc-contact' / `perso/cc-switch-server' above.

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

;;; Mise en place
;; Mise
(use-package mise
  :hook
  (prog-mode . mise-mode))

(provide 'init-dev)
;;; init-dev.el ends here
