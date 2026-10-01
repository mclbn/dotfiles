;;; init-formats.el --- Modes for data and markup formats, file associations -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-project.

;;; Code:

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

;;; File/mode associations
;; Script-shell-mode on zsh
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
;; .in and .out are text by default
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
;; Arduino is C++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(provide 'init-formats)
;;; init-formats.el ends here
