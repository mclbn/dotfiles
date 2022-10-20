;;; early-init.el --- -*- lexical-binding: t -*-

;;; Early startup tweaks

;; Increasing Garbage collection threshold during init
;; gcmh will take the lead from there once loaded
(setq gc-cons-threshold 100000000)

;; We handle package initialization, so we must prevent Emacs from doing it
;; early!
(setq package-enable-at-startup nil)

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disabling site-start.el loading (not used)
(setq site-run-file nil)

;; Disabling unused interfaces
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Do not show default modeline until doom-modeline is loaded
(setq-default mode-line-format nil)

;; Because undo-tree throws a warning about cl being deprecated
(setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;; early-init.el ends here
