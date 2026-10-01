;;; init-project.el --- Projects, git, search -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-files.

;;; Code:

;;; "Main" packages that provide major features
;; Project.el : project management
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '(".projectile"
     "Makefile"
     "CMakeLists.txt"
     "compile_commands.json"
     "configure.ac"
     "Cargo.toml"
     "pom.xml"
     "Gemfile"
     "composer.json"
     "package.json"
     "pyproject.toml"
     "platformio.ini")))

(defvar perso/project-compile-history (make-hash-table :test 'equal)
  "Per-project memory of the last `compile' command, keyed by project root.")

(defun perso/project-compile ()
  "Compile from the project root, remembering the command per project.
Behaves like `projectile-compile-project': the last command used for
this project is offered as the default."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (compile-command (or (gethash root perso/project-compile-history)
                              compile-command))
         (cmd (compilation-read-command compile-command)))
    (puthash root cmd perso/project-compile-history)
    (compile cmd)))

;; Magit : Git interface
(use-package magit
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  (magit-status-show-untracked-files 'all)
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))

;; Rg : ripgrep search
(use-package rg
  :if (executable-find "rg")
  :config
  (defun perso/rg ()
    (interactive)
    (call-interactively #'rg-menu)
    (add-to-list
     'display-buffer-alist
     '("\\*rg\\*" . (nil . ((body-function . select-window))))))
  (bind-key "C-z C-r" #'perso/rg))

;; Wgrep : write modified files in grep buffers
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(provide 'init-project)
;;; init-project.el ends here
