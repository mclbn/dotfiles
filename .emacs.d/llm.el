;;; llm.el --- LLM stack management -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'gptel)
(require 'transient)
(require 'gptel-transient)
(require 'cl-lib)
(require 'subr-x)

(defun perso/portainer-token (machine port)
  "Retrieve the secret for MACHINE and PORT from `auth-sources'.
PORT can be a string or an integer.
Returns the secret as a string, or signals an error if not found."
  (let* ((port-str (if (numberp port) (number-to-string port) port))
         (entry (car (auth-source-search :host machine
                                         :port port-str
                                         :require '(:secret)
                                         :max 1)))
         (secret (and entry (plist-get entry :secret))))
    (cond
     ((null entry)
      (error "No authinfo entry found for machine=%s port=%s" machine port))
     ((null secret)
      (error "Entry found but has no secret for machine=%s port=%s" machine port))
     ((functionp secret) (funcall secret))   ; unwrap the closure
     (t secret))))

(defun perso/portainer--curl (host method path &optional data)
  "Minimal Portainer API call.
    HOST is a hostname string (e.g. \"mycomputer.lan.org\").
    METHOD is an HTTP method string (e.g. \"GET\", \"POST\").
    PATH is the API endpoint.  DATA, when non-nil, is a JSON string
    sent as the request body.
    Return parsed JSON (alist) or nil on any error."
  (let* ((url (concat "https://" host ":9443" path))
         (args (append
                (list "-sk" "-X" method
                      "-H" (concat "X-API-Key: " (perso/portainer-token host 9443)))
                (when data (list "-H" "Content-Type: application/json" "-d" data))
                (list url)))
         ;; (_ (message "curl %s" (mapconcat #'shell-quote-argument args " ")))
         (out (with-output-to-string
                (with-current-buffer standard-output
                  (apply #'call-process "curl" nil t nil args)))))
    (ignore-errors (json-parse-string out :object-type 'alist :array-type 'list))))

(defun perso/llm-active-stacks (host)
  "Return the list of active stack names on HOST."
  (let ((stacks (perso/portainer--curl host "GET" "/api/stacks")))
    (cl-loop for s in stacks
             when (eq (alist-get 'Status s) 1)
             collect (alist-get 'Name s))))

(defun perso/llm-stack-action (host id name action)
  "Start or stop a Portainer stack by ID. ACTION is `start' or `stop'.
NAME is used only for display purposes.
Beware: endpoint id is hardcoded to 3 (default local one)."
  (if (perso/portainer--curl
       host
       "POST"
       (format "/api/stacks/%s/%s?endpointId=3" id action))
      (message "Portainer: %s -> %s" name action)
    (user-error "Portainer: failed to %s stack %s" action name)))

(defun perso/llm-stack-activate (host backend-name model)
  "Start MODEL's stack; stop all other model stacks from BACKEND-NAME.
BACKEND-NAME is a gptel backend name (e.g. \"llama-cpp-main\").
MODEL is a gptel model symbol (e.g. \\='qwen36-27b-opti)."
  (let* ((backend (alist-get backend-name gptel--known-backends nil nil #'equal))
         (all-models (gptel-backend-models backend))
         (target (symbol-name model))
         (stacks (perso/portainer--curl host "GET" "/api/stacks"))
         (id-alist (cl-loop for s in stacks
                            collect (cons (alist-get 'Name s)
                                         (alist-get 'Id s)))))
    (dolist (m all-models)
      (unless (eq m model)
        (let ((id (alist-get (symbol-name m) id-alist nil nil #'equal)))
          (when id
            (ignore-errors
              (perso/llm-stack-action host id (symbol-name m) "stop"))))))
    (let ((id (alist-get target id-alist nil nil #'equal)))
      (unless id (user-error "No Portainer stack named %s" target))
      (perso/llm-stack-action host id target "start"))))

(defun perso/llm-menu--select-backend ()
  "Select a gptel backend via completing-read and update `gptel-backend' and `gptel-model'."
  (interactive)
  (let* ((names (mapcar #'car gptel--known-backends))
         (choice (completing-read "Backend: " names nil t)))
    (setq gptel-backend (gptel-get-backend choice)
          gptel-model   (car (gptel-backend-models gptel-backend)))
    (message "Backend: %s  Model: %s" choice gptel-model)))

(defun perso/llm-menu--select-model ()
  "Select a gptel model for the current backend via completing-read."
  (interactive)
  (let* ((models (gptel-backend-models gptel-backend))
         (choice (completing-read "Model: "
                                  (mapcar #'symbol-name models) nil t)))
    (setq gptel-model (intern choice))
    (message "Model: %s" gptel-model)))

(defun perso/llm-menu--activate-stack ()
  "Start the stack for the current model, stop others."
  (interactive)
  (perso/llm-stack-activate
   (car (split-string (gptel-backend-host gptel-backend) ":"))
   (gptel-backend-name gptel-backend)
   gptel-model)
  (message "Stack activated: %s@%s"
           (symbol-name gptel-model)
           (gptel-backend-host gptel-backend)))

(defun perso/llm-menu--backend-desc ()
  "Dynamic description showing the current backend name."
  (concat "Backend  " (propertize (gptel-backend-name gptel-backend) 'face 'transient-value)))

(defun perso/llm-menu--model-desc ()
  "Dynamic description showing the current model."
  (concat "Model  " (propertize (gptel--model-name gptel-model) 'face 'transient-value)))

(defun perso/llm-menu--stack-desc ()
  "Dynamic description showing stack name and host."
  (concat "Start stack "
          (propertize (concat (gptel--model-name gptel-model) "@" (gptel-backend-host gptel-backend))
                      'face 'warning)))

(defun perso/llm-menu--running-desc ()
  "Dynamic description showing the currently running stack."
  (let* ((host (gptel-backend-host gptel-backend))
         (active (car (perso/llm-active-stacks
                       (car (split-string host ":"))))))
    (concat "Running stack on "
            (propertize host 'face 'transient-value)
            " → "
            (propertize (or active "none") 'face 'transient-value))))

;;;; Prompt builder ----------------------------------------------------------
;; Assemble a modular system prompt from the org template tree (master file
;; with tagged slot headings, plus one directory per category), stage it
;; together with tools, sampling parameters and scope, and apply the result
;; as a gptel preset spec.  Entry point: `perso/gptel-prompt-builder'.

(declare-function perso/gptel--resolve-org-includes nil (text dir))
(declare-function perso/gptel--maybe-prepend-datetime nil (text with-datetime))
(declare-function gptel-mcp-connect "gptel-integrations")
(declare-function org-map-entries "org" (func &optional match scope &rest skip))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-current-level "org" ())
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-set-tags "org" (tags))
(defvar mcp-hub-servers)
(defvar savehist-additional-variables)
(defvar org-use-tag-inheritance)
(defvar org-inhibit-startup)

(defcustom perso/gptel-prompt-root
  (expand-file-name "prompts/templates/" user-emacs-directory)
  "Root of the modular prompt tree: master file and category directories."
  :type 'directory :group 'gptel)

(defcustom perso/gptel-prompt-master "master.org"
  "Master template file name, relative to `perso/gptel-prompt-root'."
  :type 'string :group 'gptel)

(defcustom perso/gptel-prompt-compiled-directory
  (expand-file-name "prompts/" user-emacs-directory)
  "Directory the preview buffer saves to by default (\\[write-file])."
  :type 'directory :group 'gptel)

(defcustom perso/gptel-prompt-categories
  '((roles    :dir "roles"    :tag "prompt_roles"    :key "R" :multi nil
              :default ("general.org"))
    (skills   :dir "skills"   :tag "prompt_skills"   :key "S" :multi t)
    (projects :dir "projects" :tag "prompt_projects" :key "P" :multi t)
    (outputs  :dir "outputs"  :tag "prompt_outputs"  :key "O" :multi t
              :default ("general.org")))
  "Prompt slot categories, as (NAME . PLIST) entries.
PLIST keys: :dir (directory relative to `perso/gptel-prompt-root'),
:tag (org tag of the slot heading in the master file), :key (single
letter; the dashboard drill-in key is its downcase), :multi (non-nil to
allow several selections), :default (list of file names selected by
default)."
  :type '(alist :key-type symbol :value-type plist) :group 'gptel)

(defcustom perso/gptel-prompt-default-tools '(("time" . "current_datetime"))
  "Tools staged by default, as (CATEGORY . NAME) pairs."
  :type '(alist :key-type string :value-type string) :group 'gptel)

(defcustom perso/gptel-prompt-thinking-on
  '(:chat_template_kwargs (:enable_thinking t))
  "Request parameters sent when the thinking toggle is on."
  :type 'sexp :group 'gptel)

(defcustom perso/gptel-prompt-thinking-off
  '(:chat_template_kwargs (:enable_thinking :json-false))
  "Request parameters sent when the thinking toggle is off."
  :type 'sexp :group 'gptel)

(defcustom perso/gptel-prompt-agentic-file "_agentic.org"
  "File name (in the skills directory) of the agentic capability fragment.
Hidden from the menu by the underscore convention; still included by name."
  :type 'string :group 'gptel)

(defcustom perso/gptel-prompt-subagent-file "_subagent.org"
  "File name (in the skills directory) of the sub-agent operating fragment.
Hidden from the menu by the underscore convention; still included by name."
  :type 'string :group 'gptel)

(defcustom perso/gptel-prompt-subagent-directory
  (expand-file-name "gptel-agents/" user-emacs-directory)
  "Directory where sub-agent definitions are written.
Must be a member of `gptel-agent-dirs' for gptel-agent to load them."
  :type 'directory :group 'gptel)

(defcustom perso/gptel-prompt-hidden-file-regexp "\\`[._]"
  "Fragments whose file name matches this regexp are hidden from the menu.
The default hides dot- and underscore-prefixed files.  Hidden fragments
remain usable as #+INCLUDE building blocks and by explicit name."
  :type 'regexp :group 'gptel)

(defvar perso/gptel-prompt--last-stage nil
  "Stage of the last apply/preview/save, restored when the builder opens.")

(defvar perso/gptel-prompt-favorites nil
  "List of (CATEGORY . FILE) favorite fragments, surfaced on the dashboard.
Managed from the dashboard and persisted via savehist.")

(defvar perso/gptel-prompt--current-stage nil
  "Working stage shared by the builder dashboard and all its sub-menus.
Initialised only by `perso/gptel-prompt-builder'; sub-menus never reset it.")

(defvar perso/gptel-prompt--picker-category nil
  "Category symbol the generic category picker is currently editing.")

(defvar perso/gptel-prompt--recipe-registry (make-hash-table :test 'eq)
  "Map preset NAME (symbol) to its assembly recipe, for loading back / export.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'perso/gptel-prompt--last-stage)
  (add-to-list 'savehist-additional-variables 'perso/gptel-prompt-favorites))

;;;;; Stage

(defun perso/gptel-prompt--category (cat)
  "Return the plist of category CAT."
  (or (cdr (assq cat perso/gptel-prompt-categories))
      (error "Unknown prompt category %s" cat)))

(defun perso/gptel-prompt--category-dir (cat)
  "Return the absolute directory of category CAT."
  (expand-file-name (plist-get (perso/gptel-prompt--category cat) :dir)
                    perso/gptel-prompt-root))

(defun perso/gptel-prompt--file-lessp (a b)
  "Order relative fragment paths A and B: root-level files first, then by
sub-directory name, then by file name."
  (let ((da (or (file-name-directory a) ""))
        (db (or (file-name-directory b) "")))
    (cond ((string= da db) (string-lessp a b))
          ((string-empty-p da) t)
          ((string-empty-p db) nil)
          (t (string-lessp da db)))))

(defun perso/gptel-prompt--org-files-under (root dir)
  "Collect *.org files under DIR recursively, as paths relative to ROOT.
Files and directories whose base name matches
`perso/gptel-prompt-hidden-file-regexp' are skipped."
  (let (out)
    (dolist (abs (directory-files dir t nil t))
      (let ((base (file-name-nondirectory abs)))
        (unless (or (member base '("." ".."))
                    (string-match-p perso/gptel-prompt-hidden-file-regexp base))
          (cond ((file-directory-p abs)
                 (setq out (nconc out (perso/gptel-prompt--org-files-under root abs))))
                ((string-suffix-p ".org" base)
                 (push (file-relative-name abs root) out))))))
    out))

(defun perso/gptel-prompt--category-files (cat)
  "Return CAT's visible org fragments as sub-directory-qualified relative paths.
The category directory is searched recursively, so a fragment in a sub-folder is
returned as \"<subfolder>/<name>.org\" (and displayed as \"<subfolder>/<name>\").
Hidden files and folders — base name matching
`perso/gptel-prompt-hidden-file-regexp' — are skipped.  Ordering is root-level
fragments first, then by sub-folder name, then by file name."
  (let ((dir (perso/gptel-prompt--category-dir cat)))
    (when (file-directory-p dir)
      (sort (perso/gptel-prompt--org-files-under dir dir)
            #'perso/gptel-prompt--file-lessp))))

(defun perso/gptel-prompt--cat-key (cat)
  "Dashboard drill-in key for category CAT (downcase of its :key)."
  (downcase (plist-get (perso/gptel-prompt--category cat) :key)))

(defun perso/gptel-prompt--category-picker-symbol (cat)
  "Return the transient sub-prefix command symbol for category CAT.
One picker prefix is generated per category (see the Transients section) so
that entering it from the dashboard is a real transient sub-prefix — the
dashboard is pushed on the stack and =C-g= pops straight back to it."
  (intern (format "perso/gptel-prompt-category-picker-%s" cat)))

(defun perso/gptel-prompt--default-stage ()
  "Return a fresh stage plist holding the configured defaults."
  (list :selections (mapcar (lambda (c)
                              (cons (car c)
                                    (copy-sequence (plist-get (cdr c) :default))))
                            perso/gptel-prompt-categories)
        :tools (copy-tree perso/gptel-prompt-default-tools)
        :use-tools t :confirm 'auto :datetime t :temperature nil :thinking 'unset
        :mode 'frozen :backend nil :model nil :scope 'global
        :agentic nil :subagents nil :workdir nil :main-agent-tools 'extend))

(defun perso/gptel-prompt--sanitize-stage (stage)
  "Drop stale file selections and sub-agent names from STAGE; return STAGE."
  (let ((sels (plist-get stage :selections)))
    (dolist (c perso/gptel-prompt-categories)
      (let ((cell (assq (car c) sels)))
        (when cell
          (let ((files (perso/gptel-prompt--category-files (car c))))
            (setcdr cell (seq-filter (lambda (f) (member f files)) (cdr cell)))))))
    (plist-put stage :selections sels))
  (when (featurep 'gptel-agent)
    (let ((avail (perso/gptel-prompt--available-subagents)))
      (plist-put stage :subagents
                 (seq-filter (lambda (n) (member n avail))
                             (plist-get stage :subagents)))))
  stage)

(defun perso/gptel-prompt--initial-stage ()
  "Default stage overlaid with the persisted last stage, stale entries dropped."
  (let ((stage (perso/gptel-prompt--default-stage)))
    (cl-loop for (k v) on (copy-tree perso/gptel-prompt--last-stage) by #'cddr
             do (plist-put stage k v))
    (let ((sels (plist-get stage :selections)))
      (dolist (c perso/gptel-prompt-categories)
        (unless (assq (car c) sels)
          (setq sels (append sels (list (cons (car c)
                                              (copy-sequence
                                               (plist-get (cdr c) :default))))))))
      (plist-put stage :selections sels))
    (when (plist-get stage :agentic)
      (when (require 'gptel-agent nil t)
        (unless (bound-and-true-p gptel-agent--agents)
          (ignore-errors (gptel-agent-update)))))
    (perso/gptel-prompt--sanitize-stage stage)))

(defun perso/gptel-prompt--stage ()
  "Return the builder's current working stage."
  perso/gptel-prompt--current-stage)

(defun perso/gptel-prompt--toggle-file (stage cat file)
  "Toggle FILE of category CAT in STAGE, honoring single/multi selection."
  (let* ((cell (assq cat (plist-get stage :selections)))
         (multi (plist-get (perso/gptel-prompt--category cat) :multi))
         (current (cdr cell)))
    (setcdr cell (cond ((member file current) (delete file current))
                       (multi (append current (list file)))
                       (t (list file))))))

(defun perso/gptel-prompt--set-category (stage cat files)
  "Set category CAT's selection in STAGE to FILES."
  (let ((cell (assq cat (plist-get stage :selections))))
    (if cell
        (setcdr cell files)
      (plist-put stage :selections
                 (append (plist-get stage :selections) (list (cons cat files)))))))

(defun perso/gptel-prompt--toggle-tool (stage cat name)
  "Toggle the tool (CAT . NAME) in STAGE, preserving selection order."
  (let ((tools (plist-get stage :tools))
        (pair (cons cat name)))
    (plist-put stage :tools (if (member pair tools)
                                (remove pair tools)
                              (append tools (list pair))))))

;;;;; Assembly

(defun perso/gptel-prompt--slot-position (tag)
  "Return the position of the heading tagged TAG, nil if absent.
Signal a `user-error' if several headings carry the tag."
  (let ((org-use-tag-inheritance nil)
        (found nil))
    (org-map-entries (lambda () (push (point) found)) (concat "+" tag))
    (when (cdr found)
      (user-error "Master template: several headings tagged :%s:" tag))
    (car found)))

(defun perso/gptel-prompt--assemble-body (selections)
  "Assemble and resolve the master template for SELECTIONS.
SELECTIONS is an alist of (CATEGORY . FILES)."
  (unless (fboundp 'perso/gptel--resolve-org-includes)
    (user-error "perso/gptel--resolve-org-includes is not defined (gptel config not loaded?)"))
  (let* ((root (file-name-as-directory (expand-file-name perso/gptel-prompt-root)))
         (master (expand-file-name perso/gptel-prompt-master root)))
    (unless (file-readable-p master)
      (user-error "Master template not readable: %s" master))
    (with-temp-buffer
      (insert-file-contents master)
      (setq default-directory root)
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode)))
      (dolist (spec perso/gptel-prompt-categories)
        (let* ((plist (cdr spec))
               (tag (plist-get plist :tag))
               (files (cdr (assq (car spec) selections)))
               (pos (perso/gptel-prompt--slot-position tag)))
          (cond
           ((and files (not pos))
            (user-error "Master template: no heading tagged :%s:" tag))
           ((not pos))
           ((not files)
            (goto-char pos)
            (delete-region pos (progn (org-end-of-subtree t t) (point))))
           (t
            (goto-char pos)
            (let ((minlevel (1+ (org-current-level))))
              (org-set-tags (remove tag (org-get-tags nil t)))
              (goto-char pos)
              (org-end-of-subtree t t)
              (let ((end (point)))
                (skip-chars-backward " \t\n")
                (delete-region (point) end))
              (insert "\n\n"
                      (mapconcat
                       (lambda (f)
                         (format "#+INCLUDE: \"%s%s\" :minlevel %d"
                                 (file-name-as-directory (plist-get plist :dir))
                                 f minlevel))
                       files "\n")
                      "\n\n"))))))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n")
      (perso/gptel--resolve-org-includes
       (buffer-substring-no-properties (point-min) (point-max)) root))))

(defun perso/gptel-prompt--effective-selections (stage)
  "Return STAGE's selections, injecting the agentic skill when agent-aware."
  (let ((sels (copy-tree (plist-get stage :selections))))
    (when (plist-get stage :agentic)
      (let* ((file perso/gptel-prompt-agentic-file)
             (path (expand-file-name file (perso/gptel-prompt--category-dir 'skills))))
        (unless (file-readable-p path)
          (user-error "Agentic fragment not found: %s" path))
        (let ((cell (assq 'skills sels)))
          (if cell
              (unless (member file (cdr cell))
                (setcdr cell (append (cdr cell) (list file))))
            (setq sels (append sels (list (cons 'skills (list file)))))))))
    sels))

(defun perso/gptel-prompt--expand-agents (text stage)
  "Replace {{AGENTS}} in TEXT with STAGE's selected sub-agent roster."
  (if (not (plist-get stage :agentic))
      text
    (let* ((names (plist-get stage :subagents))
           (roster
            (mapconcat
             (lambda (name)
               (let ((desc (and (bound-and-true-p gptel-agent--agents)
                                (plist-get (cdr (assoc name gptel-agent--agents))
                                           :description))))
                 (format "`%s`: %s" name (or desc ""))))
             names "\n")))
      (replace-regexp-in-string (regexp-quote "{{AGENTS}}") (or roster "")
                                text t t))))

(defun perso/gptel-prompt--body (recipe)
  "Assemble RECIPE's prompt body, injecting the agentic layer and roster.
RECIPE is a stage-like plist read for :selections, :agentic and :subagents."
  (perso/gptel-prompt--expand-agents
   (perso/gptel-prompt--assemble-body
    (perso/gptel-prompt--effective-selections recipe))
   recipe))

(defun perso/gptel-prompt-build-system (recipe)
  "Assemble the system prompt string from RECIPE (a stage-like plist).
This is the single assembly entry point shared by the interactive builder,
saved presets and exported Elisp.  Reads :selections, :agentic, :subagents
and :datetime; references `perso/gptel-prompt-root', never a baked path."
  (perso/gptel--maybe-prepend-datetime
   (perso/gptel-prompt--body recipe)
   (plist-get recipe :datetime)))

(defun perso/gptel-prompt--system-value (stage)
  "Return STAGE's system prompt: a string, or a function in live modes."
  (let ((dt (plist-get stage :datetime)))
    (pcase (plist-get stage :mode)
      ('live
       (lambda () (perso/gptel-prompt-build-system stage)))
      ('datetime-live
       (let ((body (perso/gptel-prompt--body stage)))
         (if dt
             (lambda () (perso/gptel--maybe-prepend-datetime body t))
           body)))
      (_ (perso/gptel-prompt-build-system stage)))))

;;;;; Recipe registry (load-back and export)

(defun perso/gptel-prompt--recipe (stage)
  "Return STAGE's assembly-only recipe plist."
  (list :selections (copy-tree (plist-get stage :selections))
        :datetime (plist-get stage :datetime)
        :mode (plist-get stage :mode)
        :agentic (plist-get stage :agentic)
        :subagents (copy-sequence (plist-get stage :subagents))))

(defun perso/gptel-prompt--register-recipe (name recipe)
  "Record RECIPE under preset NAME for later load-back."
  (puthash name recipe perso/gptel-prompt--recipe-registry))

(defun perso/gptel-prompt--recipe-for (name)
  "Return the recipe recorded for preset NAME, or nil."
  (gethash name perso/gptel-prompt--recipe-registry))

(defun perso/gptel-prompt--plist-delete (plist prop)
  "Return a copy of PLIST without PROP."
  (let (out)
    (cl-loop for (k v) on plist by #'cddr
             unless (eq k prop) do (setq out (nconc out (list k v))))
    out))

(defun perso/gptel-prompt--tool-name->pair (name)
  "Return the (CATEGORY . NAME) pair for tool NAME, searching known tools."
  (cl-loop for (cat . entries) in (bound-and-true-p gptel--known-tools)
           when (assoc name entries) return (cons cat name)))

(defun perso/gptel-prompt--tool-names->pairs (names)
  "Map tool NAMES to (CATEGORY . NAME) pairs, dropping unknown ones."
  (delq nil (mapcar #'perso/gptel-prompt--tool-name->pair names)))

(defun perso/gptel-prompt--unwrap-eval (v)
  "Unwrap a (:eval (quote X)) or (:eval X) request-params value to X."
  (if (and (consp v) (eq (car v) :eval))
      (let ((form (cadr v)))
        (if (and (consp form) (eq (car form) 'quote)) (cadr form) form))
    v))

(defun perso/gptel-prompt--params->thinking (params)
  "Infer the :thinking axis (on/off/unset) from a preset's PARAMS value."
  (let ((p (perso/gptel-prompt--unwrap-eval params)))
    (cond ((null p) 'unset)
          ((equal p perso/gptel-prompt-thinking-on) 'on)
          ((equal p perso/gptel-prompt-thinking-off) 'off)
          (t 'unset))))

(defun perso/gptel-prompt--stage-from-preset (name)
  "Rebuild a builder stage from preset NAME (recipe + stored keys), or nil."
  (let ((recipe (perso/gptel-prompt--recipe-for name))
        (preset (ignore-errors (gptel-get-preset name))))
    (when (and recipe preset)
      (let ((stage (perso/gptel-prompt--default-stage)))
        (plist-put stage :selections (copy-tree (plist-get recipe :selections)))
        (plist-put stage :datetime (plist-get recipe :datetime))
        (plist-put stage :mode (or (plist-get recipe :mode) 'live))
        (plist-put stage :agentic (plist-get recipe :agentic))
        (plist-put stage :subagents (copy-sequence (plist-get recipe :subagents)))
        (let ((tools (plist-get preset :tools)))
          (when (and (consp tools) (eq (car tools) :append))
            (setq tools (cadr tools)))
          (plist-put stage :tools (perso/gptel-prompt--tool-names->pairs tools)))
        (plist-put stage :use-tools (plist-get preset :use-tools))
        (plist-put stage :confirm (plist-get preset :confirm-tool-calls))
        (plist-put stage :temperature (plist-get preset :temperature))
        (plist-put stage :thinking
                   (perso/gptel-prompt--params->thinking
                    (plist-get preset :request-params)))
        (when (plist-member preset :backend)
          (plist-put stage :backend (plist-get preset :backend))
          (plist-put stage :model (plist-get preset :model)))
        (when (member 'gptel-agent (plist-get preset :parents))
          (plist-put stage :agentic t))
        (perso/gptel-prompt--sanitize-stage stage)))))

;;;;; Preset spec and application

(defun perso/gptel-prompt--mcp-servers (tools)
  "Return the MCP server names required by TOOLS ((CATEGORY . NAME) pairs)."
  (delete-dups
   (cl-loop for (cat . _name) in tools
            when (string-prefix-p "mcp-" cat)
            collect (substring cat 4))))

(defun perso/gptel-prompt--ensure-tools (tools)
  "Connect the MCP servers TOOLS need and check that each tool resolves."
  (when-let* ((servers (perso/gptel-prompt--mcp-servers tools)))
    (gptel-mcp-connect servers 'sync nil))
  (when-let* ((missing (cl-loop for (cat . name) in tools
                                unless (ignore-errors
                                         (gptel-get-tool (list cat name)))
                                collect (format "%s/%s" cat name))))
    (user-error "Unknown tools: %s" (string-join missing ", "))))

(defun perso/gptel-prompt--spec (stage system &optional with-pre)
  "Return the preset spec for STAGE, with SYSTEM as system prompt value.
Omits :temperature and :request-params when unset (leaving gptel's current
value untouched).  When agent-aware, inherits the gptel-agent preset via
:parents; :main-agent-tools controls whether staged tools extend (:append)
or restrict (replace) the inherited toolset.  With WITH-PRE, add a :pre hook
connecting the MCP servers the staged tools require."
  (let* ((agentic (plist-get stage :agentic))
         (tools (copy-tree (plist-get stage :tools)))
         (tool-names (mapcar #'cdr tools))
         (servers (perso/gptel-prompt--mcp-servers tools))
         (restrict (and agentic (eq (plist-get stage :main-agent-tools) 'restrict)))
         (params (pcase (plist-get stage :thinking)
                   ('on (copy-tree perso/gptel-prompt-thinking-on))
                   ('off (copy-tree perso/gptel-prompt-thinking-off))))
         (use-tools (if agentic
                        (or (plist-get stage :use-tools) t)
                      (plist-get stage :use-tools)))
         (temp (plist-get stage :temperature))
         (spec nil))
    (when (and with-pre servers)
      (setq spec (list :pre (lambda () (gptel-mcp-connect servers 'sync nil)))))
    (when agentic
      (setq spec (nconc spec (list :parents '(gptel-agent)))))
    (when (plist-get stage :backend)
      (setq spec (nconc spec (list :backend (plist-get stage :backend)
                                   :model (plist-get stage :model)))))
    (setq spec (nconc spec
                      (list :system system
                            :tools (if (and agentic (not restrict))
                                       (list :append tool-names)
                                     tool-names)
                            :use-tools use-tools
                            :confirm-tool-calls (plist-get stage :confirm))))
    (when temp (setq spec (nconc spec (list :temperature temp))))
    (when params (setq spec (nconc spec (list :request-params params))))
    spec))

(defun perso/gptel-prompt--scope-flag (stage)
  "Translate STAGE's scope to a `gptel--set-with-scope' flag."
  (pcase (plist-get stage :scope) ('buffer t) ('oneshot 1)))

(defun perso/gptel-prompt--preset-description (stage)
  "Return a one-line description summarising STAGE's selections."
  (let ((sels (plist-get stage :selections)))
    (format "Prompt builder: %s"
            (string-join
             (delq nil
                   (mapcar (lambda (c)
                             (when-let* ((files (cdr (assq (car c) sels))))
                               (format "%s=%s" (car c)
                                       (mapconcat #'file-name-sans-extension
                                                  files "+"))))
                           perso/gptel-prompt-categories))
             " "))))

(defun perso/gptel-prompt--apply (stage system)
  "Apply STAGE with SYSTEM as system prompt, honoring STAGE's scope."
  (perso/gptel-prompt--ensure-tools (plist-get stage :tools))
  (let ((flag (perso/gptel-prompt--scope-flag stage)))
    (gptel--apply-preset
     (perso/gptel-prompt--spec stage system)
     (lambda (sym val) (gptel--set-with-scope sym val flag))))
  (let ((wd (and (plist-get stage :agentic) (plist-get stage :workdir))))
    (when (and wd (bound-and-true-p gptel-mode))
      (setq-local default-directory (file-name-as-directory wd))))
  (setq perso/gptel-prompt--last-stage (copy-tree stage))
  (message "gptel prompt applied — %s%s, scope %s, %d staged tool(s)%s%s"
           (plist-get stage :mode)
           (if (plist-get stage :agentic) " (agent-aware)" "")
           (plist-get stage :scope)
           (length (plist-get stage :tools))
           (if (stringp system)
               (format ", %d chars" (length system))
             ", dynamic system prompt")
           (let ((wd (and (plist-get stage :agentic) (plist-get stage :workdir))))
             (if (and wd (bound-and-true-p gptel-mode))
                 (format ", cwd %s" (abbreviate-file-name wd))
               ""))))

;;;;; Preview buffer

(defvar-local perso/gptel-prompt-preview--stage nil)

(defvar perso/gptel-prompt-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'perso/gptel-prompt-preview-accept)
    (define-key map (kbd "C-c C-k") #'perso/gptel-prompt-preview-abort)
    map))

(define-minor-mode perso/gptel-prompt-preview-mode
  "Accept or discard an assembled prompt preview."
  :lighter " PromptPreview")

(defun perso/gptel-prompt--preview-system (stage text)
  "Return the system prompt value for accepting TEXT under STAGE."
  (pcase (plist-get stage :mode)
    ('live (perso/gptel-prompt--system-value stage))
    ('datetime-live (if (plist-get stage :datetime)
                        (lambda () (perso/gptel--maybe-prepend-datetime text t))
                      text))
    (_ text)))

(defun perso/gptel-prompt-preview (stage)
  "Show STAGE's assembled prompt in a preview buffer."
  (let* ((mode (plist-get stage :mode))
         (body (perso/gptel-prompt--body stage))
         (text (if (eq mode 'datetime-live)
                   body
                 (perso/gptel--maybe-prepend-datetime
                  body (plist-get stage :datetime))))
         (len (length text))
         (buffer (get-buffer-create "*gptel prompt preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (org-mode)
      (perso/gptel-prompt-preview-mode 1)
      (setq perso/gptel-prompt-preview--stage (copy-tree stage)
            buffer-read-only (eq mode 'live)
            header-line-format
            (concat
             (format " Prompt preview [%s]%s — C-c C-c apply (%s scope), C-c C-k discard"
                     mode
                     (if (plist-get stage :agentic) " (agent-aware)" "")
                     (plist-get stage :scope))
             (pcase mode
               ('live " — read-only, regenerated per request")
               ('datetime-live " — date/time line added per request")
               (_ ""))
             (format " | ~%d tok / %d ch" (/ len 4) len)
             (when (plist-get stage :agentic)
               (format " | parents: gptel-agent | sub-agents: %s%s"
                       (if (plist-get stage :subagents)
                           (string-join (plist-get stage :subagents) ",")
                         "none")
                       (if (plist-get stage :workdir)
                           (format " | cwd: %s"
                                   (abbreviate-file-name (plist-get stage :workdir)))
                         "")))))
      (when (file-directory-p perso/gptel-prompt-compiled-directory)
        (setq default-directory
              (file-name-as-directory perso/gptel-prompt-compiled-directory))))
    (pop-to-buffer buffer)))

(defun perso/gptel-prompt-preview-accept ()
  "Apply the previewed prompt with the stage it was assembled from."
  (interactive)
  (let* ((stage perso/gptel-prompt-preview--stage)
         (text (buffer-substring-no-properties (point-min) (point-max))))
    (unless stage (user-error "Not in a prompt preview buffer"))
    (perso/gptel-prompt--apply stage (perso/gptel-prompt--preview-system stage text))
    (quit-window t)))

(defun perso/gptel-prompt-preview-abort ()
  "Discard the prompt preview."
  (interactive)
  (quit-window t))

;;;;; Menu commands

(defun perso/gptel-prompt-apply-now ()
  "Assemble the staged prompt and apply it."
  (interactive)
  (let ((stage (copy-tree (perso/gptel-prompt--stage))))
    (perso/gptel-prompt--apply stage (perso/gptel-prompt--system-value stage))))

(defun perso/gptel-prompt-open-preview ()
  "Assemble the staged prompt into a preview buffer."
  (interactive)
  (perso/gptel-prompt-preview (copy-tree (perso/gptel-prompt--stage))))

(defun perso/gptel-prompt-save-preset ()
  "Save the staged configuration as a named gptel preset (session-lived)."
  (interactive)
  (let* ((stage (copy-tree (perso/gptel-prompt--stage)))
         (input (string-trim
                 (completing-read "Save as preset: "
                                  (mapcar (lambda (p) (symbol-name (car p)))
                                          gptel--known-presets))))
         (name (intern input)))
    (when (string-empty-p input)
      (user-error "Preset name cannot be empty"))
    (apply #'gptel-make-preset name
           :description (perso/gptel-prompt--preset-description stage)
           (perso/gptel-prompt--spec
            stage (perso/gptel-prompt--system-value stage) 'with-pre))
    (perso/gptel-prompt--register-recipe name (perso/gptel-prompt--recipe stage))
    (setq perso/gptel-prompt--last-stage (copy-tree stage))
    (message "gptel preset saved: @%s" input)))

(defun perso/gptel-prompt-reset ()
  "Reset the staged configuration to the configured defaults."
  (interactive)
  (setq perso/gptel-prompt--current-stage (perso/gptel-prompt--default-stage)))

(defun perso/gptel-prompt--toggle-datetime ()
  "Toggle the date/time preamble."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :datetime (not (plist-get stage :datetime)))))

(defun perso/gptel-prompt--available-subagents ()
  "Return delegable sub-agent names (loaded agents minus the two presets)."
  (when (and (featurep 'gptel-agent) (bound-and-true-p gptel-agent--agents))
    (cl-remove-if (lambda (n) (member n '("gptel-agent" "gptel-plan")))
                  (mapcar #'car gptel-agent--agents))))

(defun perso/gptel-prompt--toggle-subagent (stage name)
  "Toggle sub-agent NAME in STAGE's roster selection."
  (let ((cur (plist-get stage :subagents)))
    (plist-put stage :subagents
               (if (member name cur) (remove name cur)
                 (append cur (list name))))))

(defun perso/gptel-prompt--update-agents ()
  "Reload gptel-agent definitions and drop stale sub-agent selections."
  (interactive)
  (if (not (fboundp 'gptel-agent-update))
      (message "gptel-agent is not loaded")
    (gptel-agent-update)
    (let* ((stage (perso/gptel-prompt--stage))
           (avail (perso/gptel-prompt--available-subagents)))
      (plist-put stage :subagents
                 (seq-filter (lambda (n) (member n avail))
                             (plist-get stage :subagents))))
    (message "gptel-agent: definitions refreshed")))

(defun perso/gptel-prompt--toggle-agentic ()
  "Toggle agent-aware mode; loading gptel-agent when enabling it."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (if (plist-get stage :agentic)
        (plist-put stage :agentic nil)
      (unless (require 'gptel-agent nil t)
        (user-error "gptel-agent is not available"))
      (unless (bound-and-true-p gptel-agent--agents)
        (ignore-errors (gptel-agent-update)))
      (unless (gptel-get-preset 'gptel-agent)
        (user-error "gptel-agent preset not found (run gptel-agent-update)"))
      (plist-put stage :agentic t))))

(defun perso/gptel-prompt--toggle-main-agent-tools ()
  "Toggle whether staged tools extend or restrict the main agent's toolset."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :main-agent-tools
               (if (eq (plist-get stage :main-agent-tools) 'restrict)
                   'extend 'restrict))))

(defun perso/gptel-prompt--select-workdir ()
  "Set the working directory for the agent-aware session."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (unless (plist-get stage :agentic)
      (user-error "Working directory applies to agent-aware sessions only"))
    (let* ((cur (or (plist-get stage :workdir) default-directory))
           (dir (read-directory-name "Agent working directory: " cur nil t)))
      (plist-put stage :workdir (expand-file-name dir)))))

(defconst perso/gptel-prompt--use-tools-choices
  '(("on (model decides)"     . t)
    ("force (require a call)" . force)
    ("off (no tools)"         . nil))
  "Label/value choices for `:use-tools'.")

(defconst perso/gptel-prompt--confirm-choices
  '(("tool decides" . auto)
    ("no"           . nil)
    ("always"       . t))
  "Label/value choices for `:confirm'.")

(defconst perso/gptel-prompt--thinking-choices
  '(("unset (backend default)" . unset)
    ("on (enable thinking)"    . on)
    ("off (disable thinking)"  . off))
  "Label/value choices for `:thinking'.")

(defconst perso/gptel-prompt--mode-choices
  '(("frozen (assemble once)"                    . frozen)
    ("datetime-live (refresh date each request)" . datetime-live)
    ("live (reassemble each request)"            . live))
  "Label/value choices for `:mode'.")

(defconst perso/gptel-prompt--scope-choices
  '(("global (default everywhere)" . global)
    ("buffer (this buffer only)"   . buffer)
    ("oneshot (next request only)" . oneshot))
  "Label/value choices for `:scope'.")

(defun perso/gptel-prompt--choice-label (choices value)
  "Label in CHOICES (alist of (LABEL . VALUE)) whose value is VALUE."
  (or (car (rassq value choices)) (format "%s" value)))

(defun perso/gptel-prompt--read-choice (prompt choices current)
  "Pick one of CHOICES via `completing-read', preselecting CURRENT."
  (let* ((default (perso/gptel-prompt--choice-label choices current))
         (label (completing-read (format "%s (default %s): " prompt default)
                                 (mapcar #'car choices) nil t nil nil default)))
    (cdr (assoc label choices))))

(defun perso/gptel-prompt--select-use-tools ()
  "Choose tool usage from the minibuffer."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :use-tools
               (perso/gptel-prompt--read-choice
                "Use tools" perso/gptel-prompt--use-tools-choices
                (plist-get stage :use-tools)))))

(defun perso/gptel-prompt--select-confirm ()
  "Choose tool-call confirmation from the minibuffer."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :confirm
               (perso/gptel-prompt--read-choice
                "Confirm tool calls" perso/gptel-prompt--confirm-choices
                (plist-get stage :confirm)))))

(defun perso/gptel-prompt--select-thinking ()
  "Choose thinking mode from the minibuffer."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :thinking
               (perso/gptel-prompt--read-choice
                "Thinking" perso/gptel-prompt--thinking-choices
                (plist-get stage :thinking)))))

(defun perso/gptel-prompt--select-mode ()
  "Choose assembly mode from the minibuffer."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :mode
               (perso/gptel-prompt--read-choice
                "Mode" perso/gptel-prompt--mode-choices
                (plist-get stage :mode)))))

(defun perso/gptel-prompt--select-scope ()
  "Choose application scope from the minibuffer."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (plist-put stage :scope
               (perso/gptel-prompt--read-choice
                "Scope" perso/gptel-prompt--scope-choices
                (plist-get stage :scope)))))

(defun perso/gptel-prompt--read-temperature ()
  "Read a temperature for the stage; empty input unsets it."
  (interactive)
  (let* ((stage (perso/gptel-prompt--stage))
         (cur (plist-get stage :temperature))
         (input (string-trim
                 (read-string "Temperature (empty to unset): "
                              (and cur (number-to-string cur))))))
    (cond ((string-empty-p input) (plist-put stage :temperature nil))
          ((string-match-p "\\`[0-9]*\\.?[0-9]+\\'" input)
           (plist-put stage :temperature (string-to-number input)))
          (t (message "Not a number: %s" input)))))

(defun perso/gptel-prompt--select-model ()
  "Stage a backend+model, or clear them to leave gptel untouched.
Presents the same combined, backend-prefixed candidate list as the =-m= command
of `gptel-menu' (one entry per model across all backends, e.g. \"ChatGPT:gpt-4o\"),
so the choices are identical; the pick is staged rather than applied live."
  (interactive)
  (let* ((stage (perso/gptel-prompt--stage))
         (none "(leave as-is)")
         (cands (cl-loop for (name . backend) in gptel--known-backends
                         nconc (cl-loop for model in (gptel-backend-models backend)
                                        collect (cons (concat name ":"
                                                              (gptel--model-name model))
                                                      (cons name model)))))
         (choice (completing-read "Model: "
                                  (cons none (mapcar #'car cands)) nil t)))
    (if (equal choice none)
        (progn (plist-put stage :backend nil)
               (plist-put stage :model nil))
      (let ((bm (cdr (assoc choice cands))))
        (plist-put stage :backend (car bm))
        (plist-put stage :model (cdr bm))))))

(defun perso/gptel-prompt--connect-mcp ()
  "Connect MCP servers so their tools appear in the selector."
  (interactive)
  (if (not (and (locate-library "mcp-hub") (require 'mcp-hub nil t)))
      (message "mcp.el is not available")
    (let* ((names (mapcar #'car mcp-hub-servers))
           (chosen (completing-read-multiple
                    "Connect MCP servers (empty for all): " names nil t)))
      (gptel-mcp-connect (or chosen names) 'sync nil)
      (message "MCP servers connected: %s" (string-join (or chosen names) ", ")))))

;;;;; Preset apply / load

(defun perso/gptel-prompt-apply-preset ()
  "Apply an existing gptel preset, honoring the builder's scope."
  (interactive)
  (let ((names (mapcar (lambda (p) (symbol-name (car p))) gptel--known-presets)))
    (unless names (user-error "No presets defined"))
    (let* ((name (intern (completing-read "Apply preset: " names nil t)))
           (flag (perso/gptel-prompt--scope-flag (perso/gptel-prompt--stage)))
           (spec (perso/gptel-prompt--plist-delete
                  (gptel-get-preset name) :description)))
      (gptel--apply-preset spec (lambda (sym val)
                                  (gptel--set-with-scope sym val flag)))
      (message "Applied preset @%s (scope %s)" name
               (plist-get (perso/gptel-prompt--stage) :scope)))))

(defun perso/gptel-prompt-load-preset ()
  "Load a builder-created (recipe-carrying) preset back into the stage."
  (interactive)
  (let ((names (cl-remove-if-not
                #'perso/gptel-prompt--recipe-for
                (mapcar #'car gptel--known-presets))))
    (unless names
      (user-error "No loadable (recipe-carrying) presets — export or save one first"))
    (let* ((choice (intern (completing-read
                            "Load preset into builder: "
                            (mapcar #'symbol-name names) nil t)))
           (stage (perso/gptel-prompt--stage-from-preset choice)))
      (unless stage (user-error "Could not rebuild a stage for @%s" choice))
      (setq perso/gptel-prompt--current-stage stage)
      (message "Loaded preset @%s into the builder" choice)
      (perso/gptel-prompt-dashboard))))

;;;;; Elisp export

(defun perso/gptel-prompt--lisp-atom (v)
  "Render V as Elisp source: t/nil bare, other symbols quoted, else `prin1'."
  (cond ((eq v t) "t")
        ((null v) "nil")
        ((symbolp v) (concat "'" (symbol-name v)))
        (t (prin1-to-string v))))

(defun perso/gptel-prompt--export-code (name stage style)
  "Return Elisp source (string) defining preset NAME from STAGE.
STYLE is `dynamic' (rebuilds from fragments via a wrapper) or `frozen'
(a self-contained baked system string)."
  (let* ((agentic (plist-get stage :agentic))
         (tools (mapcar #'cdr (plist-get stage :tools)))
         (servers (perso/gptel-prompt--mcp-servers (plist-get stage :tools)))
         (restrict (and agentic (eq (plist-get stage :main-agent-tools) 'restrict)))
         (params (pcase (plist-get stage :thinking)
                   ('on perso/gptel-prompt-thinking-on)
                   ('off perso/gptel-prompt-thinking-off)))
         (use-tools (if agentic (or (plist-get stage :use-tools) t)
                      (plist-get stage :use-tools)))
         (temp (plist-get stage :temperature))
         (backend (plist-get stage :backend))
         (desc (perso/gptel-prompt--preset-description stage))
         (lines nil))
    (cl-flet ((add (fmt &rest args) (push (apply #'format fmt args) lines)))
      (if (eq style 'dynamic)
          (progn
            (add "(perso/gptel-prompt-define-preset '%s" name)
            (add "  :description %s" (prin1-to-string desc))
            (add "  :recipe '%s" (prin1-to-string
                                  (perso/gptel-prompt--recipe stage))))
        (progn
          (add "(gptel-make-preset '%s" name)
          (add "  :description %s" (prin1-to-string desc))
          (add "  :system %s" (prin1-to-string
                               (perso/gptel-prompt-build-system stage)))))
      (when agentic (add "  :parents '(gptel-agent)"))
      (add "  :tools '%s" (prin1-to-string
                           (if (and agentic (not restrict))
                               (list :append tools)
                             tools)))
      (add "  :use-tools %s" (perso/gptel-prompt--lisp-atom use-tools))
      (add "  :confirm-tool-calls %s"
           (perso/gptel-prompt--lisp-atom (plist-get stage :confirm)))
      (when temp (add "  :temperature %s" (number-to-string temp)))
      (when params (add "  :request-params '%s" (prin1-to-string params)))
      (when backend
        (add "  :backend %s" (prin1-to-string backend))
        (add "  :model '%s" (plist-get stage :model)))
      (when servers
        (add "  :pre (lambda () (gptel-mcp-connect '%s 'sync nil))"
             (prin1-to-string servers))))
    (concat (string-join (nreverse lines) "\n") ")\n")))

(defun perso/gptel-prompt-define-preset (name &rest spec)
  "Define preset NAME from SPEC, building a dynamic :system from its :recipe.
SPEC is a plist; its :recipe (an assembly recipe) becomes a :system thunk that
re-assembles from the live fragments on every request, and is registered so the
preset can be loaded back into the builder.  Remaining keys pass to
`gptel-make-preset' unchanged.  This is the form emitted by dynamic export."
  (let ((recipe (plist-get spec :recipe))
        (rest (perso/gptel-prompt--plist-delete spec :recipe)))
    (when recipe (perso/gptel-prompt--register-recipe name recipe))
    (apply #'gptel-make-preset name
           :system (lambda () (perso/gptel-prompt-build-system recipe))
           rest)))

(defvar-local perso/gptel-prompt-export--dest nil)
(defvar-local perso/gptel-prompt-export--file nil)

(defvar perso/gptel-prompt-export-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'perso/gptel-prompt-export-accept)
    (define-key map (kbd "C-c C-k") #'perso/gptel-prompt-preview-abort)
    map))

(define-minor-mode perso/gptel-prompt-export-mode
  "Copy or write an exported preset definition."
  :lighter " PresetExport")

(defun perso/gptel-prompt--export-preview (name code dest file)
  "Preview CODE for preset NAME; C-c C-c copies (DEST kill-ring) or writes FILE."
  (let ((buf (get-buffer-create "*gptel preset export*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert code))
      (emacs-lisp-mode)
      (perso/gptel-prompt-export-mode 1)
      (setq perso/gptel-prompt-export--dest dest
            perso/gptel-prompt-export--file file
            header-line-format
            (format " Preset export @%s → %s — C-c C-c %s, C-c C-k cancel"
                    name
                    (if (eq dest 'file) (abbreviate-file-name file) "kill-ring")
                    (if (eq dest 'file) "write" "copy")))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun perso/gptel-prompt-export-accept ()
  "Copy the exported preset to the kill-ring, or append it to the target file."
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (dest perso/gptel-prompt-export--dest)
        (file perso/gptel-prompt-export--file))
    (pcase dest
      ('kill-ring (kill-new code) (message "Preset copied to kill-ring"))
      ('file (append-to-file (concat "\n" code "\n") nil file)
             (message "Preset appended to %s" (abbreviate-file-name file))))
    (quit-window t)))

(defun perso/gptel-prompt-export-preset ()
  "Export the staged configuration as an Elisp preset definition."
  (interactive)
  (let* ((stage (copy-tree (perso/gptel-prompt--stage)))
         (name (intern (string-trim
                        (read-string "Preset name: "
                                     (perso/gptel-prompt--preset-description stage)))))
         (style (perso/gptel-prompt--read-choice
                 "Export style"
                 '(("dynamic (rebuilds from fragments)" . dynamic)
                   ("frozen (baked prompt string)" . frozen))
                 'dynamic))
         (dest (perso/gptel-prompt--read-choice
                "Destination"
                '(("kill-ring" . kill-ring) ("write to file" . file))
                'kill-ring))
         (file (when (eq dest 'file)
                 (read-file-name "Write preset to file: "
                                 (expand-file-name "gptel-presets.el"
                                                   user-emacs-directory))))
         (code (perso/gptel-prompt--export-code name stage style)))
    (perso/gptel-prompt--export-preview name code dest file)))

;;;;; On-demand check

(defun perso/gptel-prompt-check-quit ()
  "Bury the check buffer and return to the dashboard."
  (interactive)
  (quit-window)
  (perso/gptel-prompt-dashboard))

(define-derived-mode perso/gptel-prompt-check-mode special-mode "gptel-check"
  "Major mode for the *gptel prompt check* report buffer.")

(define-key perso/gptel-prompt-check-mode-map (kbd "q")
  #'perso/gptel-prompt-check-quit)

(defun perso/gptel-prompt-check ()
  "Preflight the staged configuration: fragments, tools, MCP, roster."
  (interactive)
  (let* ((stage (perso/gptel-prompt--stage))
         (out nil))
    (cl-flet ((line (fmt &rest args) (push (apply #'format fmt args) out)))
      (line "Fragments")
      (condition-case err
          (progn (perso/gptel-prompt--body stage)
                 (line "  ✓ assembly OK (includes resolved)"))
        (error (line "  ✗ %s" (error-message-string err))))
      (line "Tools")
      (if (null (plist-get stage :tools))
          (line "  (none staged)")
        (dolist (pair (plist-get stage :tools))
          (if (ignore-errors (gptel-get-tool (list (car pair) (cdr pair))))
              (line "  ✓ %s/%s" (car pair) (cdr pair))
            (line "  ✗ unresolved: %s/%s" (car pair) (cdr pair)))))
      (let ((servers (perso/gptel-prompt--mcp-servers (plist-get stage :tools))))
        (when servers
          (line "MCP servers")
          (dolist (s servers)
            (if (and (boundp 'mcp-hub-servers) (assoc s mcp-hub-servers))
                (line "  • %s (known to mcp-hub)" s)
              (line "  ⚠ %s (unknown to mcp-hub)" s)))))
      (when (or (plist-get stage :agentic) (plist-get stage :subagents))
        (line "Agent")
        (cond ((and (plist-get stage :agentic) (null (plist-get stage :subagents)))
               (line "  ⚠ agent-aware ON but roster empty"))
              ((and (not (plist-get stage :agentic)) (plist-get stage :subagents))
               (line "  ⚠ sub-agents selected but agent-aware OFF"))
              (t (line "  ✓ agent-aware / roster coherent")))))
    (let ((buf (get-buffer-create "*gptel prompt check*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "*gptel prompt check*\n\n"
                  "(press q to return to the menu)\n\n"
                  (string-join (nreverse out) "\n") "\n"))
        (perso/gptel-prompt-check-mode)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

;;;;; Favorites

(defun perso/gptel-prompt--ordered-collection (candidates)
  "Return a completion table over CANDIDATES that preserves their given order.
Prevents the minibuffer UI from re-sorting alphabetically, so the caller's
ordering (here: category, then fragment name) is what the user sees."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun perso/gptel-prompt-manage-favorites ()
  "Toggle the dashboard favorites, then return to the dashboard.
Each prompt lists *all* fragments as \"category/name\" (ordered by category then
name) with a =[x]/[ ]= marker for the current working set: pick one to flip it,
or choose the Done entry to save and exit.  This deliberately avoids
`completing-read-multiple', whose inline comma-separated entry filtered the
candidate list down to the already-selected items."
  (interactive)
  (let* ((pairs (cl-loop for c in perso/gptel-prompt-categories
                         append (mapcar (lambda (f) (cons (car c) f))
                                        (perso/gptel-prompt--category-files (car c)))))
         (working (seq-filter (lambda (fav) (member fav pairs))
                              (copy-sequence perso/gptel-prompt-favorites)))
         (done "✓ Done (save favorites)"))
    (catch 'done
      (while t
        (let* ((label->pair
                (mapcar (lambda (p)
                          (cons (format "%s %s/%s"
                                        (if (member p working) "[x]" "[ ]")
                                        (car p) (file-name-sans-extension (cdr p)))
                                p))
                        pairs))
               (choice (completing-read
                        (format "Favorites — %d selected — toggle one or Done: "
                                (length working))
                        (perso/gptel-prompt--ordered-collection
                         (cons done (mapcar #'car label->pair)))
                        nil t)))
          (if (or (null choice) (string= choice done))
              (throw 'done nil)
            (let ((pair (cdr (assoc choice label->pair))))
              (when pair
                (setq working (if (member pair working)
                                  (remove pair working)
                                (append working (list pair))))))))))
    (setq perso/gptel-prompt-favorites working)
    (perso/gptel-prompt-dashboard)))

;;;;; Sub-agents: assembly, save, recipe helpers, tool editing

(defun perso/gptel-prompt--subagent-body (stage)
  "Assemble a sub-agent body: STAGE's selection plus the sub-agent fragment.
Excludes the agentic layer and the roster; no date/time preamble."
  (let* ((sels (copy-tree (plist-get stage :selections)))
         (file perso/gptel-prompt-subagent-file)
         (path (expand-file-name file (perso/gptel-prompt--category-dir 'skills))))
    (unless (file-readable-p path)
      (user-error "Sub-agent fragment not found: %s" path))
    (let ((cell (assq 'skills sels)))
      (if cell
          (unless (member file (cdr cell))
            (setcdr cell (append (cdr cell) (list file))))
        (setq sels (append sels (list (cons 'skills (list file)))))))
    (perso/gptel-prompt--assemble-body sels)))

(defun perso/gptel-prompt--serialize-recipe (stage)
  "Serialize STAGE's sub-agent re-assembly recipe into one `read'-able line.
Stores selections and the template root expressed relative to the sub-agent
directory, so the recipe stays valid after the project is cloned elsewhere."
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil)
        (root-rel (file-relative-name
                   (expand-file-name perso/gptel-prompt-root)
                   (expand-file-name perso/gptel-prompt-subagent-directory))))
    (prin1-to-string (list :selections (plist-get stage :selections)
                           :root-rel root-rel))))

(defun perso/gptel-prompt--deserialize-recipe (recipe-str)
  "Parse RECIPE-STR from a :prompt-recipe: property into a plist, or nil."
  (condition-case err
      (let ((form (car (read-from-string recipe-str))))
        (and (listp form) (plist-member form :selections) form))
    (error
     (message "gptel-subagent: unreadable :prompt-recipe: — %s"
              (error-message-string err))
     nil)))

(defun perso/gptel-prompt--root-for-file (recipe agent-file)
  "Template root for AGENT-FILE, from RECIPE's :root-rel (default \"../templates/\")."
  (expand-file-name (or (plist-get recipe :root-rel) "../templates/")
                    (file-name-directory agent-file)))

(defun perso/gptel-prompt--split-drawer (text)
  "Split sub-agent file TEXT into (DRAWER . BODY), or return nil.
DRAWER is the leading :PROPERTIES:...:END: block up to and including the first
:END: line; BODY is everything after it."
  (when (string-prefix-p ":PROPERTIES:" (string-trim-left text))
    (let ((case-fold-search t))
      (when (string-match "^[ \t]*:END:[ \t]*\n" text)
        (let ((end (match-end 0)))
          (cons (substring text 0 end)
                (substring text end)))))))

(defun perso/gptel-prompt--sync-fallback (agent-file new-body)
  "Refresh AGENT-FILE's frozen body with NEW-BODY, preserving its drawer."
  (when (and (stringp new-body)
             (file-readable-p agent-file)
             (file-writable-p agent-file))
    (let* ((current (with-temp-buffer
                      (insert-file-contents agent-file)
                      (buffer-string)))
           (parts (perso/gptel-prompt--split-drawer current)))
      (when parts
        (let ((old-body (string-trim (cdr parts)))
              (new-trim (string-trim new-body)))
          (unless (string-equal old-body new-trim)
            (let* ((updated (concat (string-trim-right (car parts))
                                    "\n\n" new-trim "\n"))
                   (dir (file-name-directory agent-file))
                   (tmp (make-temp-file
                         (expand-file-name ".#gptel-fallback-" dir))))
              (unwind-protect
                  (progn
                    (with-temp-file tmp (insert updated))
                    (rename-file tmp agent-file t))
                (when (file-exists-p tmp)
                  (ignore-errors (delete-file tmp)))))))))))

(defun perso/gptel-prompt--system-thunk (recipe agent-file fallback)
  "Return a function re-assembling a sub-agent system prompt from RECIPE.
AGENT-FILE anchors the portable template root; FALLBACK (the frozen body) is
returned if live assembly fails."
  (lambda (&rest _)
    (condition-case err
        (if (fboundp 'perso/gptel-prompt--subagent-body)
            (let* ((perso/gptel-prompt-root
                    (perso/gptel-prompt--root-for-file recipe agent-file))
                   (body (perso/gptel-prompt--subagent-body recipe)))
              (when (and (stringp body) (not (string-empty-p (string-trim body))))
                (ignore-errors
                  (perso/gptel-prompt--sync-fallback agent-file body)))
              body)
          (user-error "perso/gptel-prompt--subagent-body is unavailable"))
      (error
       (message "gptel-subagent: dynamic prompt for %s failed (%s); using frozen copy"
                (file-name-nondirectory agent-file)
                (error-message-string err))
       (or fallback "")))))

(defun perso/gptel-prompt--dynamize (orig agent-file &rest args)
  "Around-advice for `gptel-agent-read-file' enabling dynamic sub-agent prompts."
  (let ((result (apply orig agent-file args)))
    (when (and (consp result) (not (cadr args)))
      (let* ((plist (cdr result))
             (recipe-str (and plist (plist-get plist :prompt-recipe))))
        (when recipe-str
          (let ((fallback (plist-get plist :system))
                (recipe (perso/gptel-prompt--deserialize-recipe recipe-str)))
            (cl-remf plist :prompt-recipe)
            (when recipe
              (setq plist (plist-put plist :system
                                     (perso/gptel-prompt--system-thunk
                                      recipe agent-file fallback))))
            (setcdr result plist)))))
    result))

(with-eval-after-load 'gptel-agent
  (advice-add 'gptel-agent-read-file :around #'perso/gptel-prompt--dynamize))

(defun perso/gptel-prompt--subagent-file-content (name description stage body nmsg)
  "Return the Org text for a sub-agent definition."
  (let* ((tools (cl-remove "Agent" (mapcar #'cdr (plist-get stage :tools))
                           :test #'string=))
         (servers (perso/gptel-prompt--mcp-servers (plist-get stage :tools)))
         (backend (plist-get stage :backend))
         (recipe (perso/gptel-prompt--serialize-recipe stage))
         (props
          (delq nil
                (list (format ":name: %s" name)
                      (format ":description: %s" description)
                      (and tools (format ":tools: %s" (string-join tools " ")))
                      (and backend (format ":backend: %s" backend))
                      (and backend (format ":model: %s" (plist-get stage :model)))
                      (and (plist-get stage :temperature)
                           (format ":temperature: %s" (plist-get stage :temperature)))
                      (and nmsg (format ":num-messages-to-send: %d" nmsg))
                      (and servers
                           (format ":pre: (lambda () (gptel-mcp-connect '(%s) 'sync nil))"
                                   (mapconcat (lambda (s) (format "\"%s\"" s))
                                              servers " ")))
                      (format ":prompt-recipe: %s" recipe)))))
    (concat ":PROPERTIES:\n"
            (mapconcat #'identity props "\n")
            "\n:END:\n\n"
            (string-trim-right body)
            "\n")))

(defun perso/gptel-prompt--sanitize-agent-name (name)
  "Return NAME made safe for use as a file base and agent name."
  (replace-regexp-in-string "[^A-Za-z0-9_-]+" "-" (string-trim name)))

(defun perso/gptel-prompt--default-subagent-name (stage)
  "Suggest a sub-agent name from STAGE's selected role, else \"specialist\"."
  (let ((roles (cdr (assq 'roles (plist-get stage :selections)))))
    (if (= (length roles) 1)
        (file-name-sans-extension (car roles))
      "specialist")))

(defvar-local perso/gptel-prompt-subagent-preview--file nil)

(defvar perso/gptel-prompt-subagent-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'perso/gptel-prompt-subagent-preview-write)
    (define-key map (kbd "C-c C-k") #'perso/gptel-prompt-preview-abort)
    map))

(define-minor-mode perso/gptel-prompt-subagent-preview-mode
  "Write or discard a sub-agent definition preview."
  :lighter " SubagentPreview")

(defun perso/gptel-prompt--subagent-preview (file content)
  "Preview CONTENT before writing it to FILE."
  (let ((buf (get-buffer-create "*gptel sub-agent preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (org-mode)
      (perso/gptel-prompt-subagent-preview-mode 1)
      (setq perso/gptel-prompt-subagent-preview--file file
            header-line-format
            (format " Sub-agent preview → %s — C-c C-c write, C-c C-k cancel"
                    (abbreviate-file-name file)))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun perso/gptel-prompt-subagent-preview-write ()
  "Write the previewed sub-agent definition and refresh gptel-agent."
  (interactive)
  (let ((file perso/gptel-prompt-subagent-preview--file)
        (content (buffer-substring-no-properties (point-min) (point-max))))
    (unless file (user-error "Not in a sub-agent preview buffer"))
    (when (and (file-exists-p file)
               (not (yes-or-no-p (format "%s exists — overwrite? "
                                         (abbreviate-file-name file)))))
      (user-error "Aborted"))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert content))
    (when (fboundp 'gptel-agent-update) (gptel-agent-update))
    (message "Sub-agent saved to %s — agents refreshed"
             (abbreviate-file-name file))
    (quit-window t)))

(defun perso/gptel-prompt-save-subagent ()
  "Save the current selection as a gptel-agent sub-agent (with preview)."
  (interactive)
  (unless (require 'gptel-agent nil t)
    (user-error "gptel-agent is not available"))
  (let* ((stage (copy-tree (perso/gptel-prompt--stage)))
         (name (perso/gptel-prompt--sanitize-agent-name
                (read-string "Sub-agent name: "
                             (perso/gptel-prompt--default-subagent-name stage)))))
    (when (string-empty-p name) (user-error "Name cannot be empty"))
    (when (member name '("gptel-agent" "gptel-plan"))
      (user-error "Refusing to shadow the %s preset" name))
    (let* ((description (string-trim
                         (read-string "Description (mandatory, one line): ")))
           (_ (when (string-empty-p description)
                (user-error "Description is mandatory")))
           (nmsg-in (string-trim
                     (read-string "Max messages to send (empty = default): ")))
           (nmsg (and (string-match-p "\\`[0-9]+\\'" nmsg-in)
                      (string-to-number nmsg-in)))
           (dir (file-name-as-directory
                 (expand-file-name perso/gptel-prompt-subagent-directory)))
           (file (expand-file-name (concat name ".org") dir))
           (body (perso/gptel-prompt--subagent-body stage))
           (content (perso/gptel-prompt--subagent-file-content
                     name description stage body nmsg)))
      (perso/gptel-prompt--subagent-preview file content))))

(defun perso/gptel-prompt--agent-tools-desc (name)
  "Describe sub-agent NAME's tools for the picker (best-effort)."
  (let ((tools (and (bound-and-true-p gptel-agent--agents)
                    (plist-get (cdr (assoc name gptel-agent--agents)) :tools))))
    (cond ((null tools) "(inherits main)")
          ((listp tools) (string-join (mapcar (lambda (x) (format "%s" x)) tools) ", "))
          ((stringp tools) tools)
          (t (format "%s" tools)))))

(defun perso/gptel-prompt--drawer-tools (drawer)
  "Return the list of tool tokens in DRAWER's :tools: line, or nil."
  (when (string-match "^[ \t]*:tools:[ \t]*\\(.*\\)$" drawer)
    (split-string (match-string 1 drawer) "[ \t]+" t)))

(defun perso/gptel-prompt--drawer-set-tools (drawer tools)
  "Return DRAWER with its :tools: line set to TOOLS (a list), inserting if absent."
  (if (string-match-p "^[ \t]*:tools:[ \t]*.*$" drawer)
      (if tools
          (replace-regexp-in-string "^[ \t]*:tools:[ \t]*.*$"
                                    (concat ":tools: " (string-join tools " "))
                                    drawer)
        (replace-regexp-in-string "^[ \t]*:tools:[ \t]*.*\n?" "" drawer))
    (if tools
        (replace-regexp-in-string "^\\([ \t]*:END:\\)"
                                  (concat ":tools: " (string-join tools " ") "\n\\1")
                                  drawer)
      drawer)))

(defun perso/gptel-prompt-edit-agent-tools ()
  "Edit the :tools of a saved sub-agent file in place, then refresh gptel-agent.
Tools are toggled one at a time from a list that always shows *every* candidate
(known tool names and categories, plus whatever the agent already lists) with a
=[x]/[ ]= marker — avoiding `completing-read-multiple', whose comma-separated
entry hid the unselected candidates behind the current selection."
  (interactive)
  (let* ((dir (file-name-as-directory
               (expand-file-name perso/gptel-prompt-subagent-directory)))
         (files (and (file-directory-p dir)
                     (directory-files dir nil "\\`[^.].*\\.org\\'"))))
    (unless files (user-error "No sub-agent files in %s" dir))
    (let* ((file (expand-file-name
                  (completing-read "Edit tools of agent: " files nil t) dir))
           (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
           (parts (perso/gptel-prompt--split-drawer text)))
      (unless parts (user-error "No property drawer in %s" file))
      (let* ((drawer (car parts))
             (body (cdr parts))
             (cur (perso/gptel-prompt--drawer-tools drawer))
             (cands (delete-dups
                     (append cur
                             (mapcar #'cdr (perso/gptel-prompt--all-tool-pairs))
                             (mapcar #'car (bound-and-true-p gptel--known-tools)))))
             (working (copy-sequence cur))
             (done "✓ Done (save tools)")
             (chosen
              (catch 'done
                (while t
                  (let* ((label->name
                          (mapcar (lambda (n)
                                    (cons (format "%s %s"
                                                  (if (member n working) "[x]" "[ ]")
                                                  n)
                                          n))
                                  cands))
                         (choice (completing-read
                                  (format "Agent tools — %d selected — toggle or Done: "
                                          (length working))
                                  (perso/gptel-prompt--ordered-collection
                                   (cons done (mapcar #'car label->name)))
                                  nil t)))
                    (if (or (null choice) (string= choice done))
                        (throw 'done working)
                      (let ((name (cdr (assoc choice label->name))))
                        (when name
                          (setq working (if (member name working)
                                            (remove name working)
                                          (append working (list name)))))))))))
             (new-drawer (perso/gptel-prompt--drawer-set-tools drawer chosen)))
        (with-temp-file file (insert new-drawer body))
        (when (fboundp 'gptel-agent-update) (gptel-agent-update))
        (message "Updated tools for %s" (file-name-nondirectory file))))))

;;;;; Menu layout helpers

(defun perso/gptel-prompt--fold1 (items)
  "First of ITEMS, then \"(+N more)\" when more remain; \"none\" when empty."
  (cond ((null items) "none")
        ((null (cdr items)) (car items))
        (t (format "%s (+%d more)" (car items) (1- (length items))))))

(defun perso/gptel-prompt--toggle-keys (n reserved)
  "Return N mutually prefix-free quick-select keys.
Single lowercase letters (a–z) are used for the first 26 items; beyond that,
two-character keys of the form \"Aa\", \"Ab\", … (an uppercase first letter and a
lowercase second letter).  The uppercase first letter avoids the action letters
in RESERVED, so no key is a prefix of another and none clashes with an uppercase
action key such as =C=, =E= or =U=."
  (let ((singles (append "abcdefghijklmnopqrstuvwxyz" nil))
        (keys nil))
    (if (<= n (length singles))
        (dotimes (i n) (push (char-to-string (nth i singles)) keys))
      (dolist (c singles) (push (char-to-string c) keys))
      (let ((firsts (seq-remove (lambda (c) (memq c reserved))
                                (append "ABCDEFGHIJKLMNOPQRSTUVWXYZ" nil)))
            (seconds (append "abcdefghijklmnopqrstuvwxyz" nil))
            (i (length singles)))
        (catch 'done
          (dolist (f firsts)
            (dolist (s seconds)
              (when (>= i n) (throw 'done nil))
              (push (concat (char-to-string f) (char-to-string s)) keys)
              (setq i (1+ i)))))))
    (nreverse keys)))

(defun perso/gptel-prompt--ncols (specs &optional maxcols)
  "Columns that fit SPECS across the current frame, 1..MAXCOLS (default 4)."
  (if (null specs) 1
    (let* ((labels (mapcar (lambda (s) (format "%s %s" (car s) (cadr s))) specs))
           (longest (apply #'max 6 (mapcar #'string-width labels)))
           (avail (max 40 (- (frame-width) 4)))
           (per (+ longest 6))
           (fit (max 1 (/ avail per))))
      (min fit (or maxcols 4) (length specs)))))

(defun perso/gptel-prompt--slice-column (specs col ncols)
  "Return the SPECS in column COL (0-based) of NCOLS, filled column-major."
  (let* ((n (length specs))
         (rows (max 1 (ceiling n ncols)))
         (start (* col rows)))
    (when (< start n)
      (seq-subseq specs start (min n (+ start rows))))))

(defun perso/gptel-prompt--col-shown-p (specs col)
  "Non-nil if column COL should be shown for SPECS."
  (and specs (< col (perso/gptel-prompt--ncols specs))))

(defun perso/gptel-prompt--col-children (prefix specs col)
  "Parse the toggle suffixes of column COL of SPECS for PREFIX."
  (transient-parse-suffixes
   prefix
   (or (perso/gptel-prompt--slice-column specs col (perso/gptel-prompt--ncols specs))
       '())))

(defun perso/gptel-prompt--summary-desc (cat)
  "Dashboard summary line for category CAT."
  (let* ((label (capitalize (symbol-name cat)))
         (files (cdr (assq cat (plist-get (perso/gptel-prompt--stage) :selections))))
         (names (mapcar #'file-name-sans-extension files)))
    (format "%-9s %s" label (perso/gptel-prompt--fold1 names))))

(defun perso/gptel-prompt--tools-summary-desc ()
  "Dashboard summary line for staged tools."
  (let* ((stage (perso/gptel-prompt--stage))
         (names (mapcar (lambda (p) (format "%s/%s" (car p) (cdr p)))
                        (plist-get stage :tools)))
         (use (pcase (plist-get stage :use-tools)
                ('force "force") ('nil "off") (_ "on"))))
    (format "%-9s %s · use:%s · %s" "Tools"
            (perso/gptel-prompt--fold1 names) use
            (perso/gptel-prompt--choice-label
             perso/gptel-prompt--confirm-choices (plist-get stage :confirm)))))

(defun perso/gptel-prompt--subagents-summary-desc ()
  "Dashboard summary line for the sub-agent roster."
  (let ((stage (perso/gptel-prompt--stage)))
    (format "%-9s %s" "Sub-agents"
            (if (plist-get stage :agentic)
                (perso/gptel-prompt--fold1 (plist-get stage :subagents))
              "(agent-aware off)"))))

(defun perso/gptel-prompt--status-desc ()
  "Cheap always-on status line for the dashboard (no I/O)."
  (let ((stage (perso/gptel-prompt--stage))
        (warns nil))
    (dolist (pair (plist-get stage :tools))
      (unless (ignore-errors (gptel-get-tool (list (car pair) (cdr pair))))
        (push (format "%s/%s not connected" (car pair) (cdr pair)) warns)))
    (when (and (plist-get stage :agentic) (null (plist-get stage :subagents)))
      (push "agent-aware on, roster empty" warns))
    (when (and (not (plist-get stage :agentic)) (plist-get stage :subagents))
      (push "sub-agents set but agent-aware off" warns))
    (if warns (concat "⚠ " (string-join (nreverse warns) " · ")) "✓ ok")))

(defun perso/gptel-prompt--modules-children ()
  "Dashboard drill-in suffixes: one per category, plus Tools and Sub-agents."
  (append
   (mapcar
    (lambda (c)
      (let ((cat (car c)))
        (list (perso/gptel-prompt--cat-key cat)
              (perso/gptel-prompt--summary-desc cat)
              (perso/gptel-prompt--category-picker-symbol cat))))
    perso/gptel-prompt-categories)
   (list (list "t" (perso/gptel-prompt--tools-summary-desc)
               #'perso/gptel-prompt-tools-picker)
         (list "g" (perso/gptel-prompt--subagents-summary-desc)
               #'perso/gptel-prompt-subagents-picker))))

(defun perso/gptel-prompt--favorites-children (_)
  "Digit-key toggle suffixes for the dashboard favorites (max 9)."
  (let ((stage (perso/gptel-prompt--stage))
        (i 0)
        (out nil))
    (dolist (fav perso/gptel-prompt-favorites)
      (setq i (1+ i))
      (when (<= i 9)
        (let* ((cat (car fav))
               (file (cdr fav))
               (sel (member file (cdr (assq cat (plist-get stage :selections))))))
          (push (list (number-to-string i)
                      (format "%s %s/%s" (if sel "[x]" "[ ]")
                              cat (file-name-sans-extension file))
                      `(lambda () (interactive)
                         (perso/gptel-prompt--toggle-file
                          (perso/gptel-prompt--stage) ',cat ,file))
                      :transient t)
                out))))
    (nreverse out)))

(defun perso/gptel-prompt--parameters-children (stage)
  "Suffixes of the Parameters column, given STAGE."
  (list (list "-e" (format "Temperature: %s"
                           (or (plist-get stage :temperature) "unset"))
              #'perso/gptel-prompt--read-temperature :transient t)
        (list "-k" (format "Thinking: %s"
                           (perso/gptel-prompt--choice-label
                            perso/gptel-prompt--thinking-choices
                            (plist-get stage :thinking)))
              #'perso/gptel-prompt--select-thinking :transient t)
        (list "-M" (format "Mode: %s"
                           (perso/gptel-prompt--choice-label
                            perso/gptel-prompt--mode-choices
                            (plist-get stage :mode)))
              #'perso/gptel-prompt--select-mode :transient t)
        (list "-m" (let ((backend (plist-get stage :backend)))
                     (if backend
                         (format "Model: %s → %s" backend (plist-get stage :model))
                       "Model: leave as-is"))
              #'perso/gptel-prompt--select-model :transient t)))

(defun perso/gptel-prompt--toggles-children (stage)
  "Suffixes of the Toggles column, given STAGE."
  (list (list "-d" (format "Date/time preamble %s"
                           (if (plist-get stage :datetime) "[x]" "[ ]"))
              #'perso/gptel-prompt--toggle-datetime :transient t)
        (list "-a" (format "Agent-aware %s"
                           (if (plist-get stage :agentic) "[x]" "[ ]"))
              #'perso/gptel-prompt--toggle-agentic :transient t)))

(defun perso/gptel-prompt--session-children (stage)
  "Suffixes of the Session column, given STAGE."
  (list (list "-s" (format "Scope: %s"
                           (perso/gptel-prompt--choice-label
                            perso/gptel-prompt--scope-choices
                            (plist-get stage :scope)))
              #'perso/gptel-prompt--select-scope :transient t)
        (list "-w" (format "Working dir: %s"
                           (let ((wd (plist-get stage :workdir)))
                             (cond ((not (plist-get stage :agentic))
                                    "(agent-aware only)")
                                   (wd (abbreviate-file-name wd))
                                   (t "buffer default"))))
              #'perso/gptel-prompt--select-workdir :transient t)
        (list :info (abbreviate-file-name
                     (expand-file-name perso/gptel-prompt-master
                                       perso/gptel-prompt-root)))))

(defun perso/gptel-prompt--category-header ()
  "Title line for the generic category picker."
  (let ((cat perso/gptel-prompt--picker-category))
    (format "%s — %s" (capitalize (symbol-name cat))
            (if (plist-get (perso/gptel-prompt--category cat) :multi)
                "multi-select" "single-select"))))

(defun perso/gptel-prompt--category-specs ()
  "Ordered toggle suffix specs for every fragment of the current category."
  (let* ((cat perso/gptel-prompt--picker-category)
         (stage (perso/gptel-prompt--stage))
         (multi (plist-get (perso/gptel-prompt--category cat) :multi))
         (files (perso/gptel-prompt--category-files cat))
         (selected (cdr (assq cat (plist-get stage :selections))))
         (keys (perso/gptel-prompt--toggle-keys (length files) nil)))
    (cl-mapcar
     (lambda (key file)
       (list key
             (format "%s %s"
                     (cond ((not (member file selected)) (if multi "[ ]" "( )"))
                           (multi "[x]")
                           (t "(•)"))
                     (file-name-sans-extension file))
             `(lambda () (interactive)
                (perso/gptel-prompt--toggle-file
                 (perso/gptel-prompt--stage) ',cat ,file))
             :transient t))
     keys files)))

(defun perso/gptel-prompt--category-controls ()
  "Control suffixes for the category picker (rendered below a blank line)."
  (let ((multi (plist-get (perso/gptel-prompt--category
                           perso/gptel-prompt--picker-category)
                          :multi)))
    (append
     (unless (perso/gptel-prompt--category-specs)
       (list (list :info "(no fragments)")))
     (when multi
       (list (list "*" "Select all" #'perso/gptel-prompt--category-select-all
                   :transient t)))
     (list (list "DEL" "Clear" #'perso/gptel-prompt--category-clear :transient t)
           (list "C-g" "Back" #'transient-quit-one)))))

(defun perso/gptel-prompt--category-select-all ()
  "Select every fragment of the current category (multi only)."
  (interactive)
  (let ((cat perso/gptel-prompt--picker-category))
    (perso/gptel-prompt--set-category
     (perso/gptel-prompt--stage) cat
     (copy-sequence (perso/gptel-prompt--category-files cat)))))

(defun perso/gptel-prompt--category-clear ()
  "Clear the current category's selection."
  (interactive)
  (perso/gptel-prompt--set-category
   (perso/gptel-prompt--stage) perso/gptel-prompt--picker-category nil))

(defun perso/gptel-prompt--all-tool-pairs ()
  "Return every registered tool as a flat list of (CATEGORY . NAME) pairs."
  (cl-loop for (cat . entries) in (bound-and-true-p gptel--known-tools)
           append (mapcar (lambda (e) (cons cat (car e))) entries)))

(defun perso/gptel-prompt--tools-specs ()
  "Ordered toggle suffix specs for every registered tool."
  (let* ((stage (perso/gptel-prompt--stage))
         (all (perso/gptel-prompt--all-tool-pairs))
         (staged (plist-get stage :tools))
         (keys (perso/gptel-prompt--toggle-keys (length all) '(?C))))
    (cl-mapcar
     (lambda (key pair)
       (list key
             (format "%s %s/%s" (if (member pair staged) "[x]" "[ ]")
                     (car pair) (cdr pair))
             `(lambda () (interactive)
                (perso/gptel-prompt--toggle-tool
                 (perso/gptel-prompt--stage) ,(car pair) ,(cdr pair)))
             :transient t))
     keys all)))

(defun perso/gptel-prompt--tools-controls ()
  "Control suffixes for the Tools picker (rendered below a blank line)."
  (let ((stage (perso/gptel-prompt--stage)))
    (append
     (unless (perso/gptel-prompt--all-tool-pairs)
       (list (list :info "(no tools registered)")))
     (list (list "-t" (format "Use tools: %s"
                              (perso/gptel-prompt--choice-label
                               perso/gptel-prompt--use-tools-choices
                               (plist-get stage :use-tools)))
                 #'perso/gptel-prompt--select-use-tools :transient t)
           (list "-c" (format "Confirm calls: %s"
                              (perso/gptel-prompt--choice-label
                               perso/gptel-prompt--confirm-choices
                               (plist-get stage :confirm)))
                 #'perso/gptel-prompt--select-confirm :transient t))
     (when (plist-get stage :agentic)
       (list (list "-r" (format "Main-agent tools: %s"
                                (if (eq (plist-get stage :main-agent-tools) 'restrict)
                                    "restrict (only staged)" "extend gptel-agent"))
                   #'perso/gptel-prompt--toggle-main-agent-tools :transient t)))
     (list (list "C" "Connect MCP servers…" #'perso/gptel-prompt--connect-mcp
                 :transient t)
           (list "C-g" "Back" #'transient-quit-one))
     (cl-loop for pair in (plist-get stage :tools)
              unless (member pair (perso/gptel-prompt--all-tool-pairs))
              collect (list :info (format "[x] %s/%s (not connected)"
                                          (car pair) (cdr pair)))))))

(defun perso/gptel-prompt--subagents-specs ()
  "Ordered roster toggle specs; nil when agent-aware is off."
  (let ((stage (perso/gptel-prompt--stage)))
    (when (plist-get stage :agentic)
      (let* ((avail (perso/gptel-prompt--available-subagents))
             (selected (plist-get stage :subagents))
             (keys (perso/gptel-prompt--toggle-keys (length avail) '(?E ?U))))
        (cl-mapcar
         (lambda (key name)
           (list key
                 (format "%s %s  %s"
                         (if (member name selected) "[x]" "[ ]") name
                         (perso/gptel-prompt--agent-tools-desc name))
                 `(lambda () (interactive)
                    (perso/gptel-prompt--toggle-subagent
                     (perso/gptel-prompt--stage) ,name))
                 :transient t))
         keys avail)))))

(defun perso/gptel-prompt--subagents-controls ()
  "Control suffixes for the Sub-agents picker (rendered below a blank line).
When agent-aware is off, offer the =-a= toggle in place of the roster."
  (let ((stage (perso/gptel-prompt--stage)))
    (if (not (plist-get stage :agentic))
        (list (list "-a" (format "Agent-aware %s"
                                 (if (plist-get stage :agentic) "[x]" "[ ]"))
                    #'perso/gptel-prompt--toggle-agentic :transient t)
              (list "E" "Edit a saved agent's tools…"
                    #'perso/gptel-prompt-edit-agent-tools :transient t)
              (list "C-g" "Back" #'transient-quit-one))
      (append
       (unless (perso/gptel-prompt--available-subagents)
         (list (list :info "(no sub-agents found — save one from the builder)")))
       (list (list "U" "Refresh list (gptel-agent-update)"
                   #'perso/gptel-prompt--update-agents :transient t)
             (list "E" "Edit a saved agent's tools…"
                   #'perso/gptel-prompt-edit-agent-tools :transient t)
             (list "C-g" "Back" #'transient-quit-one))))))

;;;;; Transients

(defmacro perso/gptel-prompt--define-category-picker (cat)
  "Define the columnar picker sub-prefix for category CAT (a symbol).
The command sets `perso/gptel-prompt--picker-category' in its own body and then
sets itself up, so it behaves as a genuine transient sub-prefix of the
dashboard: entering it pushes the dashboard onto the transient stack, and its
=C-g Back= (a plain `transient-quit-one') pops back to it.  All rendering reuses
the shared `perso/gptel-prompt--category-*' helpers, which read the category
variable this command sets."
  (let ((name (perso/gptel-prompt--category-picker-symbol cat)))
    `(transient-define-prefix ,name ()
       ,(format "Pick fragments for the `%s' prompt category." cat)
       :refresh-suffixes t
       [:class transient-column
        :setup-children
        (lambda (_)
          (transient-parse-suffixes ',name
            (list (list :info (perso/gptel-prompt--category-header)))))]
       [[:class transient-column
         :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--category-specs) 0))
         :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                      ',name (perso/gptel-prompt--category-specs) 0))]
        [:class transient-column
         :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--category-specs) 1))
         :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                      ',name (perso/gptel-prompt--category-specs) 1))]
        [:class transient-column
         :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--category-specs) 2))
         :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                      ',name (perso/gptel-prompt--category-specs) 2))]
        [:class transient-column
         :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--category-specs) 3))
         :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                      ',name (perso/gptel-prompt--category-specs) 3))]]
       [:class transient-column
        :setup-children
        (lambda (_)
          (transient-parse-suffixes ',name
            (perso/gptel-prompt--category-controls)))]
       (interactive)
       (setq perso/gptel-prompt--picker-category ',cat)
       (unless perso/gptel-prompt--current-stage
         (setq perso/gptel-prompt--current-stage (perso/gptel-prompt--initial-stage)))
       (transient-setup ',name))))

;; Generate one picker sub-prefix per configured category.
(dolist (perso/gptel-prompt--spec perso/gptel-prompt-categories)
  (eval `(perso/gptel-prompt--define-category-picker
          ,(car perso/gptel-prompt--spec))
        t))

(transient-define-prefix perso/gptel-prompt-tools-picker ()
  "Stage tools and tool options."
  :refresh-suffixes t
  [:class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'perso/gptel-prompt-tools-picker
       (list (list :info "Tools"))))]
  [[:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--tools-specs) 0))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-tools-picker
                                 (perso/gptel-prompt--tools-specs) 0))]
   [:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--tools-specs) 1))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-tools-picker
                                 (perso/gptel-prompt--tools-specs) 1))]
   [:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--tools-specs) 2))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-tools-picker
                                 (perso/gptel-prompt--tools-specs) 2))]
   [:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--tools-specs) 3))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-tools-picker
                                 (perso/gptel-prompt--tools-specs) 3))]]
  [:class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'perso/gptel-prompt-tools-picker
       (perso/gptel-prompt--tools-controls)))]
  (interactive)
  (unless perso/gptel-prompt--current-stage
    (setq perso/gptel-prompt--current-stage (perso/gptel-prompt--initial-stage)))
  (transient-setup 'perso/gptel-prompt-tools-picker))

(transient-define-prefix perso/gptel-prompt-subagents-picker ()
  "Choose the sub-agent roster and edit saved agents."
  :refresh-suffixes t
  [:class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'perso/gptel-prompt-subagents-picker
       (list (list :info "Sub-agents"))))]
  [[:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--subagents-specs) 0))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-subagents-picker
                                 (perso/gptel-prompt--subagents-specs) 0))]
   [:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--subagents-specs) 1))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-subagents-picker
                                 (perso/gptel-prompt--subagents-specs) 1))]
   [:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--subagents-specs) 2))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-subagents-picker
                                 (perso/gptel-prompt--subagents-specs) 2))]
   [:class transient-column
    :if (lambda () (perso/gptel-prompt--col-shown-p (perso/gptel-prompt--subagents-specs) 3))
    :setup-children (lambda (_) (perso/gptel-prompt--col-children
                                 'perso/gptel-prompt-subagents-picker
                                 (perso/gptel-prompt--subagents-specs) 3))]]
  [:class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'perso/gptel-prompt-subagents-picker
       (perso/gptel-prompt--subagents-controls)))]
  (interactive)
  (unless perso/gptel-prompt--current-stage
    (setq perso/gptel-prompt--current-stage (perso/gptel-prompt--initial-stage)))
  (transient-setup 'perso/gptel-prompt-subagents-picker))

(transient-define-prefix perso/gptel-prompt-dashboard ()
  "gptel prompt builder — dashboard."
  :refresh-suffixes t
  [["Modules" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-dashboard
       (perso/gptel-prompt--modules-children)))]
   ["Parameters" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-dashboard
       (perso/gptel-prompt--parameters-children (perso/gptel-prompt--stage))))]
   ["Toggles" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-dashboard
       (perso/gptel-prompt--toggles-children (perso/gptel-prompt--stage))))]
   ["Session" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-dashboard
       (perso/gptel-prompt--session-children (perso/gptel-prompt--stage))))]]
  [["Favorites" :class transient-column
    :if (lambda () perso/gptel-prompt-favorites)
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-dashboard
       (perso/gptel-prompt--favorites-children nil)))]]
  [["Status" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-dashboard
       (list (list :info (perso/gptel-prompt--status-desc)))))]]
  [["Apply / Preview"
    ("RET" "Assemble & apply" perso/gptel-prompt-apply-now)
    ("P" "Preview" perso/gptel-prompt-open-preview)]
   ["Presets"
    ("A" "Apply preset…" perso/gptel-prompt-apply-preset)
    ("L" "Load preset…" perso/gptel-prompt-load-preset)]
   ["Save / Export"
    ("S" "Save preset" perso/gptel-prompt-save-preset)
    ("X" "Export as Elisp…" perso/gptel-prompt-export-preset)
    ("G" "Save as sub-agent" perso/gptel-prompt-save-subagent)]
   ["Utility"
    ("C" "Check" perso/gptel-prompt-check)
    ("F" "Manage favorites…" perso/gptel-prompt-manage-favorites)
    ("R" "Reset" perso/gptel-prompt-reset :transient t)
    ("C-g" "Quit" transient-quit-one)]]
  (interactive)
  (unless perso/gptel-prompt--current-stage
    (setq perso/gptel-prompt--current-stage (perso/gptel-prompt--initial-stage)))
  (transient-setup 'perso/gptel-prompt-dashboard))

(defun perso/gptel-prompt-builder ()
  "Open the prompt builder, starting a fresh working stage."
  (interactive)
  (setq perso/gptel-prompt--current-stage (perso/gptel-prompt--initial-stage))
  (perso/gptel-prompt-dashboard))

(transient-define-prefix perso/llm-menu ()
  "LLM stack management menu."
  ["LLM Stack"
    (:info "")
    (:info #'perso/llm-menu--running-desc)
    (:info "")
    ("b" perso/llm-menu--backend-desc perso/llm-menu--select-backend :transient t)
    ("m" perso/llm-menu--model-desc   perso/llm-menu--select-model :transient t)
    ("s" perso/llm-menu--stack-desc   perso/llm-menu--activate-stack :transient t)]
  [["Gptel"
    ("g" "Start gptel" perso/gptel)
    ("p" "Prompt builder" perso/gptel-prompt-builder)]
   ["Eca"
    ("e" "Start eca" eca)]])

;;; llm.el ends here
