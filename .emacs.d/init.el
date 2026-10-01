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

;;; Configuration modules live in lisp/ (see lisp/perso-lib.el)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'perso-lib)
;; Optional modules, and the environment variable that disables each one
;; when set to "Y".  Every module is enabled by default.
(setq perso/module-switches
      '((init-writing   . "EMACS_NOWRITING")
        (init-dev       . "EMACS_NODEV")
        (init-org       . "EMACS_NOORG")
        (init-mail      . "EMACS_NOMU")
        (init-rss       . "EMACS_NORSS")
        (init-ai        . "EMACS_NOAI")
        (init-ai-movies . "EMACS_NOAIMOVIES")))

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
  ;; nil: an error in one package's setup becomes a warning, and the rest
  ;; of the configuration still loads
  (setq use-package-expand-minimally nil)
  ;; flip to t to run M-x use-package-report
  (setq use-package-compute-statistics nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(perso/require-module 'init-core)
(perso/require-module 'init-interface)
(perso/require-module 'init-completion)
(perso/require-module 'init-editing)
(perso/require-module 'init-files)
(perso/require-module 'init-project)
(perso/require-module 'init-formats)
(perso/require-module 'init-writing)
(perso/require-module 'init-dev)
(perso/require-module 'init-org)
(perso/require-module 'init-mail)
(perso/require-module 'init-rss)
(perso/require-module 'init-ai)
(perso/require-module 'init-ai-movies)

;;; Startup time
;; Let's finish loading this file by displaying how much time we took to start
(defun display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections; %s."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done
   (perso/startup-report)))
(add-hook 'emacs-startup-hook #'display-startup-time)

;;; Only for debugging purpose
;; (setq debug-on-error t)
;; (setq debug-on-quit t)

(provide 'init)
;;; init.el ends here
