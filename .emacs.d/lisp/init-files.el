;;; init-files.el --- Dired, file viewers, shell, treemacs, TRAMP -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-editing.

;;; Code:

;;; File manipulation
;; Async : asynchronous processing. Provides `dired-async-mode' (dired hook
;; below); declared here so it no longer depends on org-download pulling it in
(use-package async :defer t)

;; Dired : directory browsing
(use-package dired
  :ensure nil
  :custom
  ;; Always delete and copy recursively
  (dired-listing-switches "-lahp --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify t)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  ;; Probe ls for capabilities
  (dired-use-ls-dired 'unspecified)
  (dired-omit-files "^\\...+$\\|\\`[.]?#\\|\\`[.][.]?\\'")
  :config
  ;; We don't use ugly listings
  (global-set-key (kbd "C-x C-d") nil)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  ;; open with external application
  (defun dired-open-external ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))
  (define-key dired-mode-map (kbd "C-<return>") #'dired-open-external)
  (define-key dired-mode-map (kbd ";") #'dired-hide-details-mode)
  ;;   (defun open-in-external-app ()
  ;;     "Open the file where point is or the marked files in Dired in external
  ;; app. The app is chosen from your OS's preference."
  ;;     (interactive)
  ;;     (let* ((file-list
  ;;             (dired-get-marked-files)))
  ;;       (mapc
  ;;        (lambda (file-path)
  ;;          (let ((process-connection-type nil))
  ;;            (start-process "" nil "xdg-open" (shell-quote-argument file-path)))) file-list)))
  ;; C-x C-d is left to dired-recent (`dired-recent-open')
  :bind
  (("C-x d" . dired-jump))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-async-mode)
  ;; (dired-mode . dired-hide-details-mode)
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-file)
                  (local-set-key (kbd "M-RET") #'dired-find-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "M-SPC") #'dired-view-file)
                  (local-set-key (kbd "M-<up>")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

(use-package diredfl
  :after zenburn-theme
  :custom-face
  (diredfl-dir-name ((t (:foreground "#94BFF3" :background "#3F3F3F" :weight bold))))
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :custom
  (dgi-auto-hide-details-p nil)
  :bind (:map dired-mode-map (":" . dired-git-info-mode))
  )

(use-package dired-recent
  :custom
  (dired-recent-max-directories nil)
  :config
  (dired-recent-mode 1))

(use-package nerd-icons-dired
  :init
  (defun my/dired-subtree-add-nerd-icons ()
    "Add nerd icons into subtree."
    (interactive)
    (revert-buffer))

  (defun my/dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
      (if nerd-icons-dired-mode
          (advice-add #'dired-subtree-toggle :after #'my/dired-subtree-add-nerd-icons)
        (advice-remove #'dired-subtree-toggle #'my/dired-subtree-add-nerd-icons))))
  :hook
  (dired-mode . nerd-icons-dired-mode)
  (nerd-icons-dired-mode . my/dired-subtree-toggle-nerd-icons))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

;; Image-mode
(use-package image-mode
  :ensure nil
  :config
  (bind-keys :map image-mode-map
             ("<up>" . image-previous-file)
             ("<down>" . image-next-file)
             ("<left>" . image-previous-file)
             ("<right>" . image-next-file)
             ("C-<left>" . image-backward-hscroll)
             ("C-<right>" . image-forward-hscroll)))

(use-package doc-view
  :ensure nil
  :config
  (bind-keys :map doc-view-mode-map
             ("<up>" . doc-view-previous-page)
             ("<down>" . doc-view-next-page)
             ("<left>" . doc-view-previous-page)
             ("<right>" . doc-view-next-page)))

;; Shell : Inferior shell mode
(use-package shell
  :custom
  (comint-process-echoes nil)
  :config
  (when (executable-find "zsh")
    (setq explicit-shell-file-name (executable-find "zsh"))
    (setq explicit-zsh-args '("--interactive"))))

;; Treemacs : visual tree
(use-package treemacs
  :pin melpa ;; the good version is on melpa, not melpa-stable
  :ensure t
  :defer t
  :bind
  ("C-z C-t" . treemacs)
  (:map treemacs-mode-map ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (treemacs-project-follow-mode t))

(use-package treemacs-all-the-icons
  :after treemacs ; do not load treemacs at startup
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; TRAMP
(use-package tramp
  :custom
  (tramp-verbose 3)
  (tramp-default-method "ssh")
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024))
  :init
  (use-package ibuffer-tramp)
  :config
  (setq password-cache-expiry 600)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(provide 'init-files)
;;; init-files.el ends here
