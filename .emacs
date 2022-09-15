;;; .emacs --- startup configuration file

;;; Commentary:
;;; Emacs configuration file, automatically runs at startup

;;; Code:

;;; config for package.el (Emacs >= 24)
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;; Disable the welcome message
(setq inhibit-startup-message t)

;;; Disable menubar
(menu-bar-mode -1)

;;; edit compressed files
(auto-compression-mode 1)

;;; Search / query highlighting
(setq search-highlight 1)
(setq query-replace-highlight 1)

;;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

;;; Stop emacs from arbitrarily adding lines to the end of a file when the
;;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;;; Always end a file with a newline
(setq require-final-newline t)

;;; Flash instead of that annoying bell
(setq visible-bell t)

;;; Show line numbers on the left
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d \u2502 ")

;;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)

;;; Backspace is backspace
(keyboard-translate ?\C-h ?\C-?)

;;; Tell emacs where is your personal elisp lib dir
;;; this is default dir for extra packages
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Backup files location and versioning
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
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
)

;;; make characters after column 80 purple
(setq whitespace-style
  (quote (face trailing tab-mark lines-tail space-before-tab)))
(add-hook 'find-file-hook 'whitespace-mode)
;;; also display column number
(setq column-number-mode t)

;;; whitespace-mode to view all whitespace characters
(setq show-trailing-whitespace t)
;;; show unncessary whitespace that can mess up your diff (redondant with previous setting ?)
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;;; Shortcut to clean whitespaces
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;; Flyspell settings
(add-hook 'text-mode-hook 'flyspell-mode)
; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-dictionary "francais")

;;; Mutt / mail-mode
(setq auto-mode-alist (append '(("/home/marc/.mutt/temp/mutt.*" . mail-mode)) auto-mode-alist))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;;; Script-shell-mode on zsh
(add-to-list 'auto-mode-alist '(".zsh$" . shell-script-mode))

;;; php-mode (https://github.com/ejmr/php-mode)
(add-to-list 'load-path "~/.emacs.d/php-mode")
(load "php-mode")

;;; arduino-mode (https://github.com/bookest/arduino-mode)
(add-to-list 'load-path "~/.emacs.d/arduino-mode")
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

(require 'asm-mode)
(add-hook 'asm-mode-hook (lambda ()
                           (setq indent-tabs-mode nil) ; use spaces to indent
                           (electric-indent-mode -1) ; indentation in asm-mode is annoying
                           (setq tab-stop-list (number-sequence 2 60 2))))

(define-key asm-mode-map (kbd "<ret>") 'newline-and-indent)
(define-key asm-mode-map (kbd "M-.") 'helm-etags-select)

;;; lua-mode (https://github.com/immerrr/lua-mode)
(add-to-list 'load-path "~/.emacs.d/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;; smali/baksmali mode (https://github.com/strazzere/Emacs-Smali)
(add-to-list 'load-path "~/.emacs.d/Emacs-Smali")
(autoload 'smali-mode "smali-mode" "Major mode for editing and viewing smali issues" t)
(add-to-list 'auto-mode-alist '(".smali$" . smali-mode))

;;; Markdown mode (https://github.com/defunkt/markdown-mode)
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Basic settings for C-mode
(require 'cc-mode)
;;(setq c-basic-offset 4 c-default-style "linux")
;;(setq-default tab-width 4 indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

;; OpenBSD KNF for C/C++
(add-to-list 'load-path "~/.emacs.d/openbsd-knf-emacs")
(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")))

(add-to-list 'load-path "~/.emacs.d/dockerfile-mode")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(add-to-list 'load-path "~/.emacs.d/nginx-mode")
(require 'nginx-mode)

;;; Multiterm
(add-to-list 'load-path "~/.emacs.d/multi-term")
(require 'multi-term)

;;; Dash, required by (at least) smartparens (https://github.com/magnars/dash.el)
(add-to-list 'load-path "~/.emacs.d/dash.el")

;;; Smartparens (https://github.com/Fuco1/smartparens)
(add-to-list 'load-path "~/.emacs.d/smartparens")
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;;; when you press RET, the curly braces automatically
;;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;;; Yasnippet (https://github.com/capitaomorte/yasnippet)
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;; Popup-el (needed by Autocomplete and Helm, https://github.com/auto-complete/popup-el)
(add-to-list 'load-path "~/.emacs.d/popup-el")

;;; Autocomplete (https://github.com/auto-complete/auto-complete)
;;; should be loaded after yasnippet so that they can work toget
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;; EPC, required by jedi (https://github.com/kiwanami/emacs-epc)
;;; it requires defered (https://github.com/kiwanami/emacs-deferred) and ctable (https://github.com/kiwanami/emacs-ctable) itself
(add-to-list 'load-path "~/.emacs.d/emacs-epc")
(add-to-list 'load-path "~/.emacs.d/emacs-deferred")
(add-to-list 'load-path "~/.emacs.d/emacs-ctable")
(require 'epc)

;;; Python jedi completion (https://github.com/tkf/emacs-jedi), also requires (https://github.com/tkf/emacs-python-environment)
;;; you also need to run jedi:install-server after adding this module
(add-to-list 'load-path "~/.emacs.d/emacs-jedi")
(add-to-list 'load-path "~/.emacs.d/emacs-python-environment")
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(global-set-key (kbd "C-c p") 'jedi:get-in-function-call)

(add-to-list 'load-path "~/.emacs.d/tuareg")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)

(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))


;;; Clang systgem include locations must be specified
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
/usr/include/c++/4.8
/usr/include/c++/4.8/backward
/usr/include/x86_64-linux-gnu/c++/4.8
/usr/local/include
/usr/lib/clang/3.4/include
/usr/lib/gcc/x86_64-linux-gnu/4.8/include
/usr/include/x86_64-linux-gnu
/usr/include
"
               )))

;;; Add cmake listfile names to the mode list.
(autoload 'cmake-mode "~/.emacs.d/lisp/cmake-mode.el" t)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
          auto-mode-alist))

;;; Rtags for cmake-ide (require compilation and installation of https://github.com/Andersbakken/rtags)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")
;; (require 'rtags)
(global-set-key (kbd "C-c s") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-c r") 'rtags-find-references-at-point)
(global-set-key (kbd "C-c v") 'rtags-find-virtuals-at-point)

;;; Auto-complete-clang (https://github.com/brianjcj/auto-complete-clang)
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(require 'auto-complete-clang)
(setq ac-quick-help-delay 0.5)
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(global-set-key [(control tab)] 'ac-complete-clang)

;;; cmake-ide (installed from package)
;; (add-to-list 'load-path "~/tmp/cmake-ide")
(cmake-ide-setup)
;;; cmake-ide requires system include paths for C and C++
(setq cmake-ide-flags-c++ '("-I/usr/include/c++/4.8" "-I/usr/include/x86_64-linux-gnu/c++/4.8" "-I/usr/include/c++/4.8/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.8/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include"))
(setq cmake-ide-flags-c '("-I/usr/local/include" "-I/usr/lib/clang/3.4/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.8/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include"))
;;; Shortcuts for quick compiling
(defun cmake-c++-mode-hook ()
    (local-set-key (kbd "C-c C-c") 'cmake-ide-compile))
(add-hook 'c++-mode-hook 'cmake-c++-mode-hook)
(defun cmake-c-mode-hook ()
    (local-set-key (kbd "C-c C-c") 'cmake-ide-compile))
(add-hook 'c-mode-hook 'cmake-c-mode-hook)

;;; Flycheck on the fly compilation (installed from packages)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;;; member-functions helps for C++ programming
(require 'member-functions)
(setq mf--source-file-extension "cpp")

;; clean-aindent-mode remove useless whitespace when return+moving (https://github.com/pmarinov/clean-aindent-mode)
(add-to-list 'load-path "~/.emacs.d/clean-aindent-mode")
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;;; Helm (https://github.com/emacs-helm/helm, don't forget to make), require Async (https://github.com/jwiegley/emacs-async.git)
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm)
(require 'helm-config)
;; (helm-autoresize-mode t)

;;; Display function name on top when scrolling long functions
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;; For source code parsing, some functions of Helm will make use of this
(semantic-mode 1)
;;; improvement to function name display (https://github.com/tuhdo/semantic-stickyfunc-enhance)
(add-to-list 'load-path "~/.emacs.d/semantic-stickyfunc-enhance")
(require 'stickyfunc-enhance)

;;; Helm config for manpages at point
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;;; fuzzy matching for some of Helm features
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
;;; Shortcuts to Helm features
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

;;; Helm-ls-git to browse git repos (https://github.com/emacs-helm/helm-ls-git)
(add-to-list 'load-path "~/.emacs.d/helm-ls-git")
(require 'helm-ls-git)
;;; Shortcut to its main feature
(global-set-key (kbd "C-c g") 'helm-ls-git-ls)

;; Sr-speedbar for visual browsing ()installed from packages)
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "C-x SPC") 'sr-speedbar-toggle)

;;; Zenburn theme (https://github.com/bbatsov/zenburn-emacs)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
(load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-candidate-limit 10)
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(package-selected-packages '(arduino-mode sr-speedbar flycheck cmake-ide)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here

