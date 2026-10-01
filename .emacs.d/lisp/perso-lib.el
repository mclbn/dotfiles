;;; perso-lib.el --- Module loader and startup report -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded at the very beginning of init.el.
;; - `perso/require-module' loads one configuration module (lisp/init-*.el).
;;   A module whose switch is set to "Y" is skipped, and a failing module is
;;   reported instead of stopping the whole startup.
;; - `perso/defsetting' declares a value that perso.el is expected to set.
;; - `perso/startup-report' summarises both, for the startup message.

;;; Code:

;; Emacs 31's --debug-init no longer turns on `debug-on-error' while the
;; init file loads, so errors caught by `perso/require-module' or by
;; use-package would never reach the debugger.  Restore the older
;; behaviour: with --debug-init, `debug-on-error' is on during init only.
(when init-file-debug
  (setq debug-on-error t)
  (add-hook 'after-init-hook (lambda () (setq debug-on-error nil))))

(defvar perso/module-switches nil
  "Alist of (MODULE . ENV-VAR): MODULE is not loaded when ENV-VAR is \"Y\".")

(defvar perso/modules-loaded nil "Modules loaded, most recent first.")
(defvar perso/modules-disabled nil "Modules skipped by their switch, most recent first.")
(defvar perso/modules-failed nil "Alist of (MODULE . ERROR-MESSAGE), most recent first.")
(defvar perso/settings-missing nil "perso.el values that were not set, most recent first.")

(defun perso/require-module (module)
  "Load MODULE, unless its switch in `perso/module-switches' is \"Y\".
An error while loading MODULE is recorded and shown in *Warnings*, and
startup goes on with the next module.  When debugging (for instance with
--debug-init), errors are not caught, so the debugger opens as usual."
  (let ((switch (alist-get module perso/module-switches)))
    (if (and switch (equal (getenv switch) "Y"))
        (push module perso/modules-disabled)
      (condition-case-unless-debug err
          (progn
            (require module)
            (push module perso/modules-loaded))
        (error
         (push (cons module (error-message-string err)) perso/modules-failed)
         (display-warning 'init
                          (format "Module %s failed to load: %s"
                                  module (error-message-string err))
                          :error))))))

(defmacro perso/defsetting (symbol docstring)
  "Declare SYMBOL, a value that perso.el is expected to set.
SYMBOL defaults to nil and keeps the value perso.el gave it, if any.
When it is still nil, it is recorded for the startup report and a
message says so.  Code using SYMBOL must skip its feature when nil."
  (declare (doc-string 2))
  `(progn
     (defvar ,symbol nil ,docstring)
     (unless ,symbol
       (push ',symbol perso/settings-missing)
       (message "perso.el: %s is not set (%s)" ',symbol ,docstring))))

(defun perso/startup-report ()
  "Return a one-line summary of module loading and perso.el values."
  (let* ((n (length perso/modules-loaded))
         (parts (list (format "%d module%s loaded" n (if (= n 1) "" "s")))))
    (when perso/modules-disabled
      (push (format "disabled: %s"
                    (mapconcat #'symbol-name (reverse perso/modules-disabled) ", "))
            parts))
    (when perso/modules-failed
      (push (format "FAILED: %s"
                    (mapconcat (lambda (f) (format "%s (%s)" (car f) (cdr f)))
                               (reverse perso/modules-failed) ", "))
            parts))
    (unless (file-exists-p (locate-user-emacs-file "perso.el"))
      (push "perso.el missing" parts))
    (when perso/settings-missing
      (let ((m (length perso/settings-missing)))
        (push (format "%d perso.el value%s missing (see *Messages*)"
                      m (if (= m 1) "" "s"))
              parts)))
    (mapconcat #'identity (nreverse parts) "; ")))

(provide 'perso-lib)
;;; perso-lib.el ends here
