;;; init-writing.el --- Spell and grammar checking, typography, dictionaries -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-formats.  Disabled with EMACS_NOWRITING=Y.

;;; Code:

;;; Languages and spell-checking
;; Quick set of functions to handle language switches
(defvar-local perso/buffer-is-french nil
  "Non-nil when Grammalecte should grammar-check the current buffer.")

(defun perso/grammalecte-flymake (report-fn &rest args)
  "Flymake backend: run Grammalecte, but only in French buffers.
In non-French buffers it reports no diagnostics, which clears any
stale French overlays on the next check."
  (if perso/buffer-is-french
      (apply (flymake-flycheck-diagnostic-function-for 'grammalecte) report-fn args)
    (funcall report-fn nil)))

(defun perso/set-checkers-language (lang typo-lang)
  "Restrict Jinx to LANG, switch Typo to TYPO-LANG, toggle Grammalecte.
French grammar checking follows the chosen language."
  (setq-local jinx-languages lang)
  (jinx-mode -1)
  (jinx-mode 1)
  (when (bound-and-true-p typo-mode) (typo-change-language typo-lang))
  ;; Grammalecte follows the chosen language:
  (setq perso/buffer-is-french
        (and (string-match-p "fr" lang) (not (string-match-p "en" lang))))
  (when (bound-and-true-p flymake-mode)
    (flymake-start nil t)))   ; force re-run: clears French overlays unless now French

(defun perso/set-language-french ()
  "Switch all text checkers in this buffer to French."
  (interactive)
  (perso/set-checkers-language "fr_FR" "French"))
(defun perso/set-language-english ()
  "Switch all text checkers in this buffer to English."
  (interactive)
  (perso/set-checkers-language "en_US" "English"))
(bind-key "C-c f" #'perso/set-language-french)
(bind-key "C-c e" #'perso/set-language-english)

;; Flymake-flycheck : to use flycheck
;; checkers as flymake backends
(use-package flymake-flycheck :defer t)

;; Flycheck-grammalecte : french syntax checking
;; Requires running M-x grammalecte-download-grammalecte once
(use-package flycheck-grammalecte
  :defer t
  :init
  (setq flycheck-grammalecte-report-spellcheck nil
        flycheck-grammalecte-report-grammar t
        flycheck-grammalecte-report-apos nil
        flycheck-grammalecte-report-esp nil
        flycheck-grammalecte-report-nbsp nil)
  :config
  (setq flycheck-grammalecte-filters-by-mode
        '((latex-mode "\\\\(?:title|(?:sub)*section){([^}]+)}"
                      "\\\\\\w+(?:\\[[^]]+\\])?(?:{[^}]*})?")
          (org-mode "(?ims)^[ \t]*#\\+begin_src.+?#\\+end_src"
                    "(?ims)^[ \t]*:LOGBOOK:.+?:END:"
                    "(?ims)^[ \t]*:PROPERTIES:.+?:END:"
                    "(?im):.*:" ; tags
                    "(?im)<.*>" ; timestamps
                    "(?im)\\[.*\\]" ; links, progress, etc.
                    "(?im)^[ \t]*\-[ \t]*" ; checkboxes
                    "(?im)(?im)[0-9]+" ; numbers
                    "(?im)^[ \t]*#\\+begin[_:].+$"
                    "(?im)^[ \t]*#\\+end[_:].+$"
                    "(?m)^[ \t]*(?:DEADLINE|SCHEDULED):.+$"
                    "(?m)^\\*+ .*[ \t]*(:[\\w:@]+:)[ \t]*$"
                    "(?im)^[ \t]*#\\+(?:caption|description|keywords|(?:sub)?title):"
                    "(?im)^[ \t]*#\\+(?!caption|description|keywords|(?:sub)?title)\\w+:.*$")
          (message-mode "(?m)^[ \t]*(?:[\\w_.]+>|[]>|]).*")))
  (setq grammalecte-python-package-directory
        (expand-file-name "grammalecte" user-emacs-directory))
  ;; (setq flycheck-grammalecte-predicate #'perso/grammalecte-predicate)
  (flycheck-grammalecte-setup))

;; Prose modes for grammalecte to check.
;; text-mode also covers Magit commit buffers,
;; whose default major mode is text-mode.
(defvar my/grammalecte-modes
  '(text-mode latex-mode mail-mode markdown-mode
              message-mode mu4e-compose-mode org-mode)
  "Major modes in which Grammalecte runs as a Flymake backend.")

(defun my/grammalecte-flymake ()
  "Enable Flymake with Grammalecte as its only backend, in prose buffers."
  (when (memq major-mode my/grammalecte-modes)
    (require 'flycheck-grammalecte)       ; ensure the `grammalecte' checker exists
    (require 'flymake-flycheck)
    (add-hook 'flymake-diagnostic-functions #'perso/grammalecte-flymake nil t)
    (flymake-mode 1)))

(add-hook 'after-change-major-mode-hook #'my/grammalecte-flymake)

;; Jinx : fast, multi-language spell-checking via Enchant
;; First launch compiles jinx-mod.c;
;; needs installing libenchant-2-dev + a C compiler.
(use-package jinx
  :diminish
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US fr_FR")
  :bind
  (("C-." . jinx-correct)
   ("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages))
  :config
  ;; (mu4e): if reply quotes / headers get spell-checked in compose
  ;; buffers, uncomment to exclude them.
  ;; (add-to-list 'jinx-exclude-faces
  ;;              '(message-mode message-header-name message-header-to
  ;;                message-header-cc message-header-subject message-header-other
  ;;                message-cited-text-1 message-cited-text-2
  ;;                message-cited-text-3 message-cited-text-4))
  )

;; sdcv : Stardict dictionnary
(when (executable-find "sdcv")
  (use-package sdcv
    :bind
    (("C-c d" . sdcv-search-pointer)
     ("C-c w" . sdcv-search-input))
    :config
    (setq sdcv-dictionary-data-dir "~/.stardic/dic")
    (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
          '("XMLittre"
            ))
    (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
          '(
            "XMLittre"
            "Dictionnaire de l’Académie Française, 8ème édition (1935)."
            "Oxford Advanced Learner's Dictionary 8th Ed."
            "Oxford English Dictionary 2nd Ed. P1"
            "Oxford English Dictionary 2nd Ed. P2"
            ))
    ))

;; Typo: auto-replace typographically useful unicode characters
(use-package typo
  :diminish
  :hook
  ((org-mode text-mode) . typo-mode))

(provide 'init-writing)
;;; init-writing.el ends here
