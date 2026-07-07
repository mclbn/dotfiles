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
    (projects :dir "projects" :tag "prompt_projects" :key "P" :multi nil)
    (outputs  :dir "outputs"  :tag "prompt_outputs"  :key "O" :multi t
              :default ("general.org")))
  "Prompt slot categories, as (NAME . PLIST) entries.
PLIST keys: :dir (directory relative to `perso/gptel-prompt-root'),
:tag (org tag of the slot heading in the master file), :key (transient
key prefix for the category's toggles; must not collide with the
builder's fixed keys), :multi (non-nil to allow several selections),
:default (list of file names selected by default)."
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

(defcustom perso/gptel-prompt-agentic-file "agentic.org"
  "File name (in the skills directory) of the agentic capability fragment."
  :type 'string :group 'gptel)

(defcustom perso/gptel-prompt-subagent-file "subagent.org"
  "File name (in the skills directory) of the sub-agent operating fragment."
  :type 'string :group 'gptel)

(defcustom perso/gptel-prompt-subagent-directory
  (expand-file-name "gptel-agents/" user-emacs-directory)
  "Directory where sub-agent definitions are written.
Must be a member of `gptel-agent-dirs' for gptel-agent to load them."
  :type 'directory :group 'gptel)

(defvar perso/gptel-prompt--last-stage nil
  "Stage of the last apply/preview/save, restored when the builder opens.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'perso/gptel-prompt--last-stage))

;;;;; Stage

(defun perso/gptel-prompt--category (cat)
  "Return the plist of category CAT."
  (or (cdr (assq cat perso/gptel-prompt-categories))
      (error "Unknown prompt category %s" cat)))

(defun perso/gptel-prompt--category-files (cat)
  "Return the sorted org files of category CAT, or nil.
Reserved agentic/sub-agent fragments are hidden from the skills category."
  (let* ((dir (perso/gptel-prompt--category-dir cat))
         (files (and (file-directory-p dir)
                     (directory-files dir nil "\\`[^.].*\\.org\\'"))))
    (if (eq cat 'skills)
        (seq-remove
         (lambda (f) (member f (list perso/gptel-prompt-agentic-file
                                     perso/gptel-prompt-subagent-file)))
         files)
      files)))

(defun perso/gptel-prompt--category-dir (cat)
  "Return the absolute directory of category CAT."
  (expand-file-name (plist-get (perso/gptel-prompt--category cat) :dir)
                    perso/gptel-prompt-root))

(defun perso/gptel-prompt--default-stage ()
  "Return a fresh stage plist holding the configured defaults."
  (list :selections (mapcar (lambda (c)
                              (cons (car c)
                                    (copy-sequence (plist-get (cdr c) :default))))
                            perso/gptel-prompt-categories)
        :tools (copy-tree perso/gptel-prompt-default-tools)
        :use-tools t :confirm 'auto :datetime t :temperature nil :thinking 'unset
        :mode 'frozen :backend nil :model nil :scope 'global
        :agentic nil :subagents nil :workdir nil))

(defun perso/gptel-prompt--initial-stage ()
  "Default stage overlaid with the persisted last stage, stale files dropped."
  (let ((stage (perso/gptel-prompt--default-stage)))
    (cl-loop for (k v) on (copy-tree perso/gptel-prompt--last-stage) by #'cddr
             do (plist-put stage k v))
    (let ((sels (plist-get stage :selections)))
      (dolist (c perso/gptel-prompt-categories)
        (let ((cell (assq (car c) sels)))
          (unless cell
            (setq cell (cons (car c)
                             (copy-sequence (plist-get (cdr c) :default))))
            (setq sels (append sels (list cell))))
          (let ((files (perso/gptel-prompt--category-files (car c))))
            (setcdr cell (seq-filter (lambda (f) (member f files)) (cdr cell))))))
      (plist-put stage :selections sels))
    (when (plist-get stage :agentic)
      (when (require 'gptel-agent nil t)
        (unless (bound-and-true-p gptel-agent--agents)
          (ignore-errors (gptel-agent-update)))))
    (when (featurep 'gptel-agent)
      (let ((avail (perso/gptel-prompt--available-subagents)))
        (plist-put stage :subagents
                   (seq-filter (lambda (n) (member n avail))
                               (plist-get stage :subagents)))))
    stage))

(defun perso/gptel-prompt--stage ()
  "Return the stage of the currently open builder menu."
  (transient-scope 'perso/gptel-prompt-builder))

(defun perso/gptel-prompt--toggle-file (stage cat file)
  "Toggle FILE of category CAT in STAGE, honoring single/multi selection."
  (let* ((cell (assq cat (plist-get stage :selections)))
         (multi (plist-get (perso/gptel-prompt--category cat) :multi))
         (current (cdr cell)))
    (setcdr cell (cond ((member file current) (delete file current))
                       (multi (append current (list file)))
                       (t (list file))))))

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
SELECTIONS is an alist of (CATEGORY . FILES).  Selected files are
injected as #+INCLUDE lines at the end of their slot's subtree, slots
without a selection are removed, slot tags are stripped, and all
includes are resolved.  Return the prompt as a string, without the
date/time preamble."
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
              ;; Normalize the subtree's trailing blank lines, then append
              ;; the includes after all hand-written content.
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

(defun perso/gptel-prompt--body (stage)
  "Assemble STAGE's prompt body, injecting the agentic layer and roster."
  (perso/gptel-prompt--expand-agents
   (perso/gptel-prompt--assemble-body
    (perso/gptel-prompt--effective-selections stage))
   stage))

(defun perso/gptel-prompt--system-value (stage)
  "Return STAGE's system prompt: a string, or a function in live modes."
  (let ((dt (plist-get stage :datetime)))
    (pcase (plist-get stage :mode)
      ('live
       (lambda ()
         (perso/gptel--maybe-prepend-datetime
          (perso/gptel-prompt--body stage) dt)))
      ('datetime-live
       (let ((body (perso/gptel-prompt--body stage)))
         (if dt
             (lambda () (perso/gptel--maybe-prepend-datetime body t))
           body)))
      (_ (perso/gptel--maybe-prepend-datetime
          (perso/gptel-prompt--body stage) dt)))))

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
With WITH-PRE, include a :pre hook connecting the MCP servers the staged
tools require (for saved presets).  When agent-aware, inherit the gptel-agent
preset (tools + delegation) via :parents and add staged tools with :append."
  (let* ((agentic (plist-get stage :agentic))
         (tools (copy-tree (plist-get stage :tools)))
         (tool-names (mapcar #'cdr tools))
         (servers (perso/gptel-prompt--mcp-servers tools))
         (params (pcase (plist-get stage :thinking)
                   ('on (copy-tree perso/gptel-prompt-thinking-on))
                   ('off (copy-tree perso/gptel-prompt-thinking-off))))
         (use-tools (if agentic
                        (or (plist-get stage :use-tools) t)
                      (plist-get stage :use-tools)))
         (spec nil))
    (when (and with-pre servers)
      (setq spec (list :pre (lambda () (gptel-mcp-connect servers 'sync nil)))))
    (when agentic
      (setq spec (nconc spec (list :parents '(gptel-agent)))))
    (when (plist-get stage :backend)
      (setq spec (nconc spec (list :backend (plist-get stage :backend)
                                   :model (plist-get stage :model)))))
    (nconc spec
           (list :system system
                 :tools (if agentic `(:append ,tool-names) tool-names)
                 :use-tools use-tools
                 :confirm-tool-calls (plist-get stage :confirm)
                 :temperature (plist-get stage :temperature)
                 :request-params (and params `(:eval (quote ,params)))))))

(defun perso/gptel-prompt--scope-flag (stage)
  "Translate STAGE's scope to a `gptel--set-with-scope' flag."
  (pcase (plist-get stage :scope) ('buffer t) ('oneshot 1)))

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
  "Save the staged configuration as a named gptel preset."
  (interactive)
  (let* ((stage (copy-tree (perso/gptel-prompt--stage)))
         (input (string-trim
                 (completing-read "Save as preset: "
                                  (mapcar (lambda (p) (symbol-name (car p)))
                                          gptel--known-presets))))
         (sels (plist-get stage :selections)))
    (when (string-empty-p input)
      (user-error "Preset name cannot be empty"))
    (apply #'gptel-make-preset (intern input)
           :description
           (format "Prompt builder: %s"
                   (string-join
                    (delq nil
                          (mapcar (lambda (c)
                                    (when-let* ((files (cdr (assq (car c) sels))))
                                      (format "%s=%s" (car c)
                                              (mapconcat #'file-name-sans-extension
                                                         files "+"))))
                                  perso/gptel-prompt-categories))
                    " "))
           (perso/gptel-prompt--spec
            stage (perso/gptel-prompt--system-value stage) 'with-pre))
    (setq perso/gptel-prompt--last-stage (copy-tree stage))
    (message "gptel preset saved: @%s" input)))

(defun perso/gptel-prompt-reset ()
  "Reset the staged configuration to the configured defaults."
  (interactive)
  (let ((stage (perso/gptel-prompt--stage)))
    (cl-loop for (k v) on (perso/gptel-prompt--default-stage) by #'cddr
             do (plist-put stage k v))))

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
  "Label/value choices for `:confirm' (gptel's `gptel-confirm-tool-calls').")

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
  "Pick one of CHOICES via `completing-read', preselecting CURRENT.
CHOICES is an alist of (LABEL . VALUE); return the chosen VALUE."
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
  "Choose tool-call confirmation from the minibuffer.
Sets gptel's `gptel-confirm-tool-calls' value on apply."
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
  "Stage a backend and model, or clear them to leave gptel untouched."
  (interactive)
  (let* ((stage (perso/gptel-prompt--stage))
         (none "(leave as-is)")
         (name (completing-read
                "Backend: " (cons none (mapcar #'car gptel--known-backends))
                nil t)))
    (if (equal name none)
        (progn (plist-put stage :backend nil)
               (plist-put stage :model nil))
      (let ((model (completing-read
                    "Model: "
                    (mapcar #'gptel--model-name
                            (gptel-backend-models (gptel-get-backend name)))
                    nil t)))
        (plist-put stage :backend name)
        (plist-put stage :model (intern model))))))

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

 (defun perso/gptel-prompt--subagent-file-content (name description stage body nmsg)
  "Return the Org text for a sub-agent definition.
Reads tools/model/temperature from STAGE (dropping the Agent tool), derives a
:pre MCP hook from staged MCP tools, and appends NMSG when non-nil.
Also embeds a :prompt-recipe: property (STAGE's selections + relative template
root) so the prompt can be re-assembled from the live fragments on every
delegation; see `perso/gptel-subagent--dynamize'.  BODY is still written in
full and serves as a frozen fallback."
  (let* ((tools (cl-remove "Agent" (mapcar #'cdr (plist-get stage :tools))
                           :test #'string=))
         (servers (perso/gptel-prompt--mcp-servers (plist-get stage :tools)))
         (backend (plist-get stage :backend))
         (recipe (perso/gptel-subagent--serialize-recipe stage))
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
            (format " Sub-agent preview → %s — C-c C-c write, C-c C-k cancel (non-delegating)"
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

(defun perso/gptel-subagent--serialize-recipe (stage)
  "Serialize what re-assembly needs into one `read'-able line.
Stores STAGE's selections and the template root expressed relative to
`perso/gptel-prompt-subagent-directory', so the recipe stays valid after the
prompt project is cloned to another location."
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil)
        (root-rel (file-relative-name
                   (expand-file-name perso/gptel-prompt-root)
                   (expand-file-name perso/gptel-prompt-subagent-directory))))
    (prin1-to-string (list :selections (plist-get stage :selections)
                           :root-rel root-rel))))

(defun perso/gptel-subagent--deserialize-recipe (recipe-str)
  "Parse RECIPE-STR from a :prompt-recipe: property into a plist, or nil.
Parsing is pure `read' (data only, never evaluated).  Returns nil on any
error or when the result carries no :selections."
  (condition-case err
      (let ((form (car (read-from-string recipe-str))))
        (and (listp form) (plist-member form :selections) form))
    (error
     (message "gptel-subagent: unreadable :prompt-recipe: — %s"
              (error-message-string err))
     nil)))

(defun perso/gptel-subagent--root-for-file (recipe agent-file)
  "Template root for AGENT-FILE, from RECIPE's :root-rel (default \"../templates/\")."
  (expand-file-name (or (plist-get recipe :root-rel) "../templates/")
                    (file-name-directory agent-file)))

(defun perso/gptel-subagent--split-drawer (text)
  "Split sub-agent file TEXT into (DRAWER . BODY), or return nil.
DRAWER is the leading :PROPERTIES:...:END: block, up to and including the
FIRST :END: line; BODY is everything after it.  Using only the first :END:
means a :END: occurring inside the body is never mistaken for the drawer
terminator."
  (when (string-prefix-p ":PROPERTIES:" (string-trim-left text))
    (let ((case-fold-search t))
      (when (string-match "^[ \t]*:END:[ \t]*\n" text)
        (let ((end (match-end 0)))
          (cons (substring text 0 end)
                (substring text end)))))))

(defun perso/gptel-subagent--sync-fallback (agent-file new-body)
  "Refresh AGENT-FILE's frozen body with NEW-BODY, preserving its drawer.
No-op unless AGENT-FILE is readable and writable, parses into a drawer + body,
and the trimmed on-disk body actually differs from NEW-BODY (so repeated
identical assemblies cause no writes and no version-control churn).  The
property drawer, including :prompt-recipe:, is kept byte-for-byte.  The write
is atomic: a temp file in the same directory is written and then renamed over
the target, and the temp is always cleaned up.  Errors propagate to the
caller, which calls this inside `ignore-errors'."
  (when (and (stringp new-body)
             (file-readable-p agent-file)
             (file-writable-p agent-file))
    (let* ((current (with-temp-buffer
                      (insert-file-contents agent-file)
                      (buffer-string)))
           (parts (perso/gptel-subagent--split-drawer current)))
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

(defun perso/gptel-subagent--system-thunk (recipe agent-file fallback)
  "Return an arity-safe function that re-assembles a sub-agent system prompt.
RECIPE is the parsed :prompt-recipe: plist, AGENT-FILE anchors the portable
template root, and FALLBACK (the frozen body) is returned verbatim if live
assembly fails for any reason.  On a successful assembly the on-disk frozen
body is refreshed as a best-effort side effect (see
`perso/gptel-subagent--sync-fallback'); a write failure never affects the
value returned to the delegation."
  (lambda (&rest _)
    (condition-case err
        (if (fboundp 'perso/gptel-prompt--subagent-body)
            (let* ((perso/gptel-prompt-root
                    (perso/gptel-subagent--root-for-file recipe agent-file))
                   (body (perso/gptel-prompt--subagent-body recipe)))
              (when (and (stringp body) (not (string-empty-p (string-trim body))))
                (ignore-errors
                  (perso/gptel-subagent--sync-fallback agent-file body)))
              body)
          (user-error "perso/gptel-prompt--subagent-body is unavailable"))
      (error
       (message "gptel-subagent: dynamic prompt for %s failed (%s); using frozen copy"
                (file-name-nondirectory agent-file)
                (error-message-string err))
       (or fallback "")))))

(defun perso/gptel-subagent--dynamize (orig agent-file &rest args)
  "Around-advice for `gptel-agent-read-file' enabling dynamic sub-agent prompts.
On the full (non metadata-only) read of an agent file carrying a
:prompt-recipe: property, replace its static string :system with a function
that re-assembles the prompt from the current fragments on every delegation,
and drop the recipe key.  Files without the property pass through untouched."
  (let ((result (apply orig agent-file args)))
    ;; ARGS is (TEMPLATES METADATA-ONLY); only rewrite the full read.
    (when (and (consp result) (not (cadr args)))
      (let* ((plist (cdr result))
             (recipe-str (and plist (plist-get plist :prompt-recipe))))
        (when recipe-str
          (let ((fallback (plist-get plist :system))
                (recipe (perso/gptel-subagent--deserialize-recipe recipe-str)))
            (cl-remf plist :prompt-recipe)      ;keep it out of gptel's preset code
            (when recipe
              (setq plist (plist-put plist :system
                                     (perso/gptel-subagent--system-thunk
                                      recipe agent-file fallback))))
            (setcdr result plist)))))
    result))

(with-eval-after-load 'gptel-agent
  (advice-add 'gptel-agent-read-file :around #'perso/gptel-subagent--dynamize))

;;;;; Menu layout

(defconst perso/gptel-prompt--keychars "abcdefghijklmnopqrstuvwxyz0123456789")

(defun perso/gptel-prompt--category-children (stage cat)
  "Toggle suffixes for the files of category CAT, given STAGE."
  (let* ((plist (perso/gptel-prompt--category cat))
         (prefix (plist-get plist :key))
         (files (seq-take (perso/gptel-prompt--category-files cat)
                          (length perso/gptel-prompt--keychars)))
         (selected (cdr (assq cat (plist-get stage :selections))))
         (i -1))
    (or (mapcar
         (lambda (file)
           (setq i (1+ i))
           (list (format "%s%c" prefix (aref perso/gptel-prompt--keychars i))
                 (format "%s %s"
                         (if (member file selected) "[x]" "[ ]")
                         (file-name-sans-extension file))
                 `(lambda ()
                    (interactive)
                    (perso/gptel-prompt--toggle-file
                     (perso/gptel-prompt--stage) ',cat ,file))
                 :transient t))
         files)
        (list (list :info "(no files)")))))

(defun perso/gptel-prompt--tools-children (stage)
  "Toggle suffixes for all registered tools, given STAGE."
  (let ((staged (plist-get stage :tools))
        (known nil)
        (toggles nil)
        (i -1))
    (dolist (group gptel--known-tools)
      (let ((cat (car group)))
        (dolist (entry (cdr group))
          (let ((name (car entry)))
            (push (cons cat name) known)
            (when (< i (1- (length perso/gptel-prompt--keychars)))
              (setq i (1+ i))
              (push (list (format "t%c" (aref perso/gptel-prompt--keychars i))
                          (format "%s %s/%s"
                                  (if (member (cons cat name) staged) "[x]" "[ ]")
                                  cat name)
                          `(lambda ()
                             (interactive)
                             (perso/gptel-prompt--toggle-tool
                              (perso/gptel-prompt--stage) ,cat ,name))
                          :transient t)
                    toggles))))))
    (append
     (list (list "-t" (format "Use tools: %s"
                              (perso/gptel-prompt--choice-label
                               perso/gptel-prompt--use-tools-choices
                               (plist-get stage :use-tools)))
                 #'perso/gptel-prompt--select-use-tools :transient t)
           (list "-c" (format "Confirm calls: %s"
                              (perso/gptel-prompt--choice-label
                               perso/gptel-prompt--confirm-choices
                               (plist-get stage :confirm)))
                 #'perso/gptel-prompt--select-confirm :transient t)
           (list "C" "Connect MCP servers" #'perso/gptel-prompt--connect-mcp
                 :transient t))
     (nreverse toggles)
     (cl-loop for pair in staged
              unless (member pair known)
              collect (list :info (format "[x] %s/%s (not connected)"
                                          (car pair) (cdr pair)))))))

(defun perso/gptel-prompt--toggles-children (stage)
  "Suffixes of the Toggles column, given STAGE."
  (list (list "-d" (format "Date/time preamble %s"
                           (if (plist-get stage :datetime) "[x]" "[ ]"))
              #'perso/gptel-prompt--toggle-datetime :transient t)
        (list "-a" (format "Agent-aware %s"
                           (if (plist-get stage :agentic) "[x]" "[ ]"))
              #'perso/gptel-prompt--toggle-agentic :transient t)))

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

(defun perso/gptel-prompt--subagents-children (stage)
  "Suffixes of the Sub-agents column, given STAGE."
  (if (not (plist-get stage :agentic))
      (list (list :info "Enable Agent-aware (-a) to choose sub-agents"))
    (let* ((avail (perso/gptel-prompt--available-subagents))
           (selected (plist-get stage :subagents))
           (i -1)
           (toggles
            (mapcar
             (lambda (name)
               (setq i (1+ i))
               (list (format "A%c" (aref perso/gptel-prompt--keychars i))
                     (format "%s %s"
                             (if (member name selected) "[x]" "[ ]") name)
                     `(lambda ()
                        (interactive)
                        (perso/gptel-prompt--toggle-subagent
                         (perso/gptel-prompt--stage) ,name))
                     :transient t))
             (seq-take avail (length perso/gptel-prompt--keychars)))))
      (append
       (list (list "u" "Refresh list (gptel-agent-update)"
                   #'perso/gptel-prompt--update-agents :transient t))
       (or toggles
           (list (list :info "(no sub-agents found — save one below)")))))))

(transient-define-prefix perso/gptel-prompt-builder ()
  "Assemble a modular gptel system prompt from the org template tree.
Selections, tools and parameters are staged in this menu and only take
effect when applied (RET, or C-c C-c from the preview)."
  :refresh-suffixes t
  [["Roles" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--category-children (perso/gptel-prompt--stage) 'roles)))]
   ["Skills" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--category-children (perso/gptel-prompt--stage) 'skills)))]
   ["Projects" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--category-children (perso/gptel-prompt--stage) 'projects)))]
   ["Outputs" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--category-children (perso/gptel-prompt--stage) 'outputs)))]]
  [["Tools" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--tools-children (perso/gptel-prompt--stage))))]
   ["Toggles" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--toggles-children (perso/gptel-prompt--stage))))]
   ["Parameters" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--parameters-children (perso/gptel-prompt--stage))))]
   ["Session" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--session-children (perso/gptel-prompt--stage))))]]
  [["Sub-agents" :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'perso/gptel-prompt-builder
       (perso/gptel-prompt--subagents-children (perso/gptel-prompt--stage))))]]
  [["Actions"
    ("RET" "Assemble & apply" perso/gptel-prompt-apply-now)
    ("p" "Preview" perso/gptel-prompt-open-preview)
    ("s" "Save as preset" perso/gptel-prompt-save-preset)
    ("a" "Save as sub-agent" perso/gptel-prompt-save-subagent)
    ("r" "Reset to defaults" perso/gptel-prompt-reset :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup 'perso/gptel-prompt-builder nil nil
                   :scope (perso/gptel-prompt--initial-stage)))

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
