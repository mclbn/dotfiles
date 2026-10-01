;;; init-ai.el --- LLM chat, agents and tools: gptel, MCP, eca -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-rss.  Disabled with EMACS_NOAI=Y.
;; The movie assistant is in init-ai-movies.el.

;;; Code:

;;; "AI" stuff
;; Values from perso.el (a server or backend whose value is not set is skipped)
(perso/defsetting perso/mcp-searxng-url "URL of the searxng MCP server")
(perso/defsetting perso/mcp-wikipedia-url "URL of the wikipedia MCP server")
(perso/defsetting perso/mcp-datagouv-url "URL of the datagouv MCP server")
(perso/defsetting perso/llm-host-main "Host of the main llama.cpp server")
(perso/defsetting perso/llm-port-main "Port of the main llama.cpp server (a string)")
(perso/defsetting perso/llm-host-backup "Host of the backup llama.cpp server")
(perso/defsetting perso/llm-port-backup "Port of the backup llama.cpp server (a string)")

;; MCP
(use-package mcp
  :after gptel
  :custom
  (mcp-hub-servers
   ;; Servers without a URL are dropped
   (seq-filter
    (lambda (server) (plist-get (cdr server) :url))
    `(("searxng" . (:url ,perso/mcp-searxng-url :timeout 30))
      ("wikipedia" . (:url ,perso/mcp-wikipedia-url :timeout 30))
      ("datagouv" . (:url ,perso/mcp-datagouv-url :timeout 30))
      ("exa"      . (:url "https://mcp.exa.ai/mcp?tools=web_search_exa,web_fetch_exa"
                          :timeout 90
                          :token ,(apply-partially #'gptel-api-key-from-auth-source "api.exa.ai")))))))

;; GPTel : chat with LLMs
(use-package gptel
  :demand t ; loaded at startup: its backends, presets, tools and keys are needed right away
  :preface
  (defun perso/gptel-reasoning-buffer-name ()
    "Name of the reasoning sink dedicated to the current buffer."
    (format "*gptel-reasoning: %s*" (buffer-name)))

  (defun perso/gptel-reasoning-buffer-setup ()
    "Redirect this buffer's gptel reasoning to its own buffer.
Meant for `gptel-mode-hook', so that redirection is on by default."
    (when (bound-and-true-p gptel-mode)
      (setq-local gptel-include-reasoning (perso/gptel-reasoning-buffer-name))))

  (defun perso/gptel-reasoning-buffer-toggle ()
    "Toggle redirection of this buffer's gptel reasoning to its own buffer."
    (interactive)
    (if (stringp gptel-include-reasoning)
        (kill-local-variable 'gptel-include-reasoning)
      (setq-local gptel-include-reasoning (perso/gptel-reasoning-buffer-name)))
    (message "gptel reasoning: %s"
             (if (stringp gptel-include-reasoning) gptel-include-reasoning "inline")))

  (defun perso/gptel-reasoning-buffer-new-entry ()
    "Open a new, separated entry in this buffer's reasoning sink."
    (when (stringp gptel-include-reasoning)
      (with-current-buffer (get-buffer-create gptel-include-reasoning)
        (unless visual-line-mode (visual-line-mode 1))
        (save-excursion
          (goto-char (point-max))
          (unless (bobp) (insert "\n\n"))
          (insert (format "──────── %s ────────\n\n"
                          (format-time-string "%H:%M:%S")))))))
  :hook (gptel-mode . perso/gptel-reasoning-buffer-setup)
  :config
  (add-hook 'gptel-pre-response-hook #'perso/gptel-reasoning-buffer-new-entry)
  (require 'gptel-integrations)
  (require 'gptel-org)
  (when (executable-find "curl")
    (setq gptel-use-curl t))

  ;;; OpenCode Go: required x-opencode-session / User-Agent headers

  (defvar my/opencode-host-regexp "opencode\\.ai\\'"
    "Backends whose host matches this get OpenCode Go's required headers.")

  (defvar my/opencode-user-agent "gptel/Emacs"
    "User-Agent announced to OpenCode Go, which rejects broad agents.")

  (defvar-local my/opencode-session-id nil
    "Stable `x-opencode-session' value for this buffer's conversation.")

  (defvar my/opencode--patched (make-hash-table :test #'eq :weakness 'key)
    "Backends already carrying the OpenCode header wrapper.")

  (defun my/opencode-session-id (&optional info)
    "Return a stable session id for the gptel request described by INFO."
    (let ((buf (plist-get info :buffer)))
      (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
        (or my/opencode-session-id
            (setq my/opencode-session-id
                  (concat "ses_" (substring
                                  (md5 (format "%s%s%s" (buffer-name)
                                               (float-time) (random)))
                                  0 24)))))))

  (defun my/opencode--eval-header (header info)
    "Evaluate a gptel backend HEADER (alist or function) for request INFO."
    (if (functionp header)
        (let ((max (cdr (func-arity header))))
          ;; gptel now calls header functions with the request plist; older
          ;; versions call them with no arguments.
          (if (or (eq max 'many) (>= max 1))
              (funcall header info)
            (funcall header)))
      header))

  (defun my/opencode--wrap-header (header)
    "Return a header function adding OpenCode Go's headers to HEADER."
    (lambda (&optional info)
      (let ((extra `(("x-opencode-session" . ,(my/opencode-session-id info))
                     ("User-Agent"         . ,my/opencode-user-agent))))
        (append extra
                (seq-remove (lambda (cell) (assoc-string (car cell) extra t))
                            (my/opencode--eval-header header info))))))

  (defun my/opencode--patch-backend (backend)
    "Add OpenCode Go headers to BACKEND if it is served from opencode.ai.
Idempotent.  Returns BACKEND, for use as `:filter-return' advice."
    (when (and (gptel-backend-p backend)
               (string-match-p my/opencode-host-regexp
                               (or (gptel-backend-host backend) ""))
               (not (gethash backend my/opencode--patched)))
      (puthash backend t my/opencode--patched)
      (aset backend
            (cl-struct-slot-offset 'gptel-backend 'header)
            (my/opencode--wrap-header (gptel-backend-header backend))))
    backend)

  (defun my/opencode-patch-backends ()
    "Patch every registered gptel backend served from opencode.ai."
    (interactive)
    (mapc (lambda (entry) (my/opencode--patch-backend (cdr entry)))
          gptel--known-backends))

  (dolist (fn '(gptel-make-openai gptel-make-anthropic))
    (advice-add fn :filter-return #'my/opencode--patch-backend))

  ;; Local llama.cpp servers, only when perso.el gives their host and port
  (when (and perso/llm-host-main perso/llm-port-main)
    (gptel-make-openai "llama-cpp-main"
      :stream t
      :protocol "http"
      :host (concat perso/llm-host-main
                    ":" perso/llm-port-main)
      :models '(qwen36-27b-opti
                qwen36-27b-opti-large
                qwen36-35b-quality
                qwen35-9b-quality
                qwen35-9b-extra-quality
                qwen3-coder-next
                gemma-12b-quality
                glm47-flash)))
  (when (and perso/llm-host-backup perso/llm-port-backup)
    (gptel-make-openai "llama-cpp-back"
      :stream t
      :protocol "http"
      :host (concat perso/llm-host-backup
                    ":" perso/llm-port-backup)
      :models '(qwen35-4b
                qwen25-coder-7b
                gemma4-12b)))

  (gptel-make-anthropic "Claude"
    :host "api.anthropic.com"
    :key #'gptel-api-key-from-auth-source
    :stream t
    :models '(claude-sonnet-5
              claude-opus-5
              claude-haiku-4-5-20251001))

  ;; Update the file with my/opencode-go-gptel-config
  (let ((f (expand-file-name "gptel-opencode-models.el" user-emacs-directory)))
    (when (file-readable-p f)
      (condition-case err
          (load f nil t t)
        (error (message "gptel: OpenCode Go models failed to load: %s"
                        (error-message-string err))))))

  (gptel-make-anthropic "OpenCode Go (Qwen Plus non streaming)"
    :host "opencode.ai"
    :endpoint "/zen/go/v1/messages"
    :protocol "https"
    :stream nil
    :key #'gptel-api-key-from-auth-source
    :models '((qwen3.7-plus
               :description "Qwen3.7 Plus [stream off]"
               :capabilities (tool-use reasoning cache media)
               :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
               :context-window 1000
               :input-cost 0.4
               :output-cost 1.6
               :request-params (:model "qwen3.7-plus"))))

  (my/opencode-patch-backends)
  (setq gptel-backend (gptel-get-backend "OpenCode Go")
        gptel-model 'glm-5.2)

  ;; General settings
  (setq
   gptel-default-mode 'org-mode
   gptel-use-tools t
   gptel-confirm-tool-calls 'auto
   gptel-include-tool-results 'auto)

  (with-eval-after-load 'gptel-transient
    (transient-define-infix perso/gptel--infix-branching-context ()
      "Toggle `gptel-org-branching-context' from the gptel menu."
      :description "Branching context (Org)"
      :class 'gptel--switches
      :variable 'gptel-org-branching-context
      :set-value #'gptel--set-with-scope
      :display-if-true  "On"
      :display-if-false "Off"
      :key "B")
    (transient-append-suffix 'gptel-menu "y"
      '(perso/gptel--infix-branching-context
        :if (lambda () (derived-mode-p 'org-mode))))
    (transient-append-suffix 'gptel-menu "y"
      '("p" "Prompt builder" gptel-builder)))

  ;;; Tools
  (gptel-make-tool
   :name "read_file"
   :function (lambda (path)
               (let ((full-path (expand-file-name path)))
                 (cond
                  ((not (file-exists-p full-path))
                   (format "Error: file does not exist: %s" full-path))
                  ((not (file-readable-p full-path))
                   (format "Error: file is not readable: %s" full-path))
                  ((> (file-attribute-size (file-attributes full-path)) (* 1024 1024))
                   (if (yes-or-no-p (format "File %s is large (> 1 MB). Read anyway? " full-path))
                       (with-temp-buffer
                         (insert-file-contents full-path)
                         (buffer-string))
                     "Aborted by user."))
                  (t
                   (with-temp-buffer
                     (insert-file-contents full-path)
                     (buffer-string))))))
   :description "Read and return the contents of a file (read-only)."
   :args (list '(:name "path" :type "string" :description "Path to the file."))
   :category "filesystem")

  (gptel-make-tool
   :name "list_directory"
   :function (lambda (path)
               (let ((full-path (expand-file-name path)))
                 (if (file-directory-p full-path)
                     (string-join
                      (directory-files full-path nil nil t)
                      "\n")
                   (format "Error: not a directory: %s" full-path))))
   :description "List the entries in a directory."
   :args (list '(:name "path" :type "string" :description "Directory path."))
   :category "filesystem")

  (gptel-make-tool
   :name "current_datetime"
   :function (lambda ()
               (format-time-string "%A %Y-%m-%d %H:%M:%S %Z (UTC%z)"))
   :description "Return the current local date and time."
   :category "time")

  ;;; Presets
  (gptel-make-preset 'eli5
    :system "Explain like I am 5 years old."
    :use-tools t
    :tools '("web_url_read" "searxng_web_search"))

  (gptel-make-preset 'websearch
    :description "Web search"
    :pre (lambda ()
           (gptel-mcp-connect '("searxng") 'sync nil))
    :use-tools t
    :tools '("web_url_read" "searxng_web_search")
    :system "Use the provided tools to search the web for up-to-date information.")

  (gptel-make-preset 'creative
    :description "Quick creative — high temp, no tools"
    :temperature 1
    :tools nil :use-tools nil
    :system "You are an imaginative creative collaborator. Offer vivid, varied ideas.")

  (gptel-make-preset 'research
    :description "Research — autonomous web search, bounded"
    :temperature 0.6
    :pre (lambda ()
           (gptel-mcp-connect '("searxng") 'sync nil))
    :use-tools t :tools '("searxng_web_search" "web_url_read" "searxng_instance_info" "searxng_search_suggestions")
    :system "You are a research assistant. Plan, then perform AT MOST 10 searches, reading the most relevant results, then synthesize a sourced answer. Stop searching once you can answer. You can use search engine suggestions to find related information.")

  (gptel-make-preset 'programmer
    :description "Careful senior programmer — precise, fs-read + web"
    :temperature 0.7
    :pre (lambda ()
           (gptel-mcp-connect '("searxng") 'sync nil))
    :use-tools t
    :tools '("read_file" "list_directory" "searxng_web_search" "web_url_read")
    :system "You are a careful senior programmer. Reason carefully about design tradeoffs. Use file reads and web search to ground claims. Try to provide code and only code as output without any additional text, prompt or note. If you cannot provide only code, be clear and concise.")

  (gptel-make-preset 'architect
    :description "Architecture/brainstorm — precise, fs-read + web"
    :temperature 0.7
    :pre (lambda ()
           (gptel-mcp-connect '("searxng") 'sync nil))
    :use-tools t
    :tools '("read_file" "list_directory" "searxng_web_search" "web_url_read")
    :system "You are a senior software architect. Reason carefully about design tradeoffs. Use file reads and web search to ground claims.")

  (gptel-make-preset 'rag
    :description "Document RAG — low temp, grounded"
    :temperature 0.1
    :pre (lambda ()
           (gptel-mcp-connect '("searxng") 'sync nil))
    :use-tools t
    :tools '("read_file" "list_directory" "searxng_web_search" "web_url_read")
    :system "Answer strictly from retrieved context (corpus or fetched pages). If the sources don't contain the answer, say so. Do not speculate. Only provide information from your context.")

  ;; A custom function to open a single gptel session
  (defun perso/gptel ()
    "Wrapper to load gptel"
    (interactive)
    (gptel "GPTel")
    (switch-to-buffer "GPTel")
    (disable-text-analysis-modes)
    (delete-other-windows))

  (use-package gptel-quick
    :vc (:url "https://github.com/karthink/gptel-quick" :branch master :rev :newest)
    :after gptel
    :bind ("C-z q" . gptel-quick)
    :config
    ;; Without the backup llama.cpp server, gptel-quick uses the default backend
    (when (and perso/llm-host-backup perso/llm-port-backup)
      (setq gptel-quick-backend
            (gptel-make-openai "llama-cpp-quick"
              :stream t
              :protocol "http"
              :host (concat perso/llm-host-backup ":" perso/llm-port-backup)
              :models '(qwen35-4b)
              :request-params '(:chat_template_kwargs (:enable_thinking :json-false)
                                                      :temperature 0))
            gptel-quick-model 'qwen35-4b))
    (setq gptel-quick-timeout 30))

  (when (file-directory-p "~/.emacs.d/prompts")
    (use-package gptel-prompts
      :vc (:url "https://github.com/jwiegley/gptel-prompts" :branch main :rev :newest)
      :after (gptel)
      :demand t
      :config
      (gptel-prompts-update)
      ;; Ensure prompts are updated if prompt files change
      (gptel-prompts-add-update-watchers)))
  :bind (("C-z g" . gptel-menu)
         ("C-z C-g" . perso/gptel)))

(use-package gptel-custom-tools
  :vc ( :url "https://github.com/mclbn/gptel-custom-tools"
        :branch main)
  ;; :load-path "~/dev/gptel-custom-tools/"
  :after gptel
  :custom
  (gptel-custom-tools-tasklist-directory (expand-file-name "gptel-tasks/" user-emacs-directory)))

(use-package gptel-tool-policy
  :vc (:url "https://github.com/mclbn/gptel-tool-policy"
            :branch main)
  :after gptel
  :demand t
  :custom
  (gptel-tool-policy-bypass-tools
   '("current_datetime"
     "Agent"
     "TaskLoad" "TaskSave" "TaskGet" "TaskList" "TaskCreate" "TaskUpdate"
     "web_search" "web_fetch"
     ;;; following line mostly obsolete ?
     "web_url_read" "searxng_instance_info" "searxng_search_suggestions" "searxng_web_search" "YouTube"))
  (gptel-tool-policy-rules
   '((deny  read  "~/.ssh/**"        "Never expose SSH keys")
     (deny  write "~/.ssh/**"        "Never write inside ~/.ssh")
     (deny  read  "~/.gnupg/**"      "Never expose GPG keys")
     (deny  write "~/.gnupg/**"      "Never write inside ~/.gnupg")
     (deny  read  "~/.authinfo*"     "Never expose stored credentials")
     (deny  write "~/.authinfo*"     "Never write stored credentials")
     (deny  read  "~/.netrc"         "Never expose stored credentials")
     (deny  write "~/.netrc"         "Never write stored credentials")
     (deny  read  "~/.aws/**"        "Cloud credentials")
     (deny  read  "~/.kube/**"       "Cluster credentials"))))

(use-package gptel-agent
  :defer t
  :after gptel
  :config
  (let ((my-agents (expand-file-name "gptel-agents/" user-emacs-directory)))
    (unless (file-directory-p my-agents)
      (make-directory my-agents t))
    (add-to-list 'gptel-agent-dirs my-agents))

  ;; (gptel-mcp-connect '("searxng") 'sync nil)
  (gptel-agent-update))

(use-package gptel-web-tools-bridge
  :vc (:url "https://github.com/mclbn/gptel-web-tools-bridge" :rev :newest)
  :after gptel
  :demand t ; must register its tools at load
  :custom
  (gptel-web-tools-bridge-provider 'exa)
  (gptel-web-tools-bridge-max-characters 8000)
  (gptel-web-tools-bridge-default-results 5)
  (gptel-web-tools-bridge-override-agent-tools nil))

(use-package gptel-builder
  :vc (:url "https://github.com/mclbn/gptel-builder" :rev :newest)
  :after gptel
  :demand t ; presets must exist before you @-call them
  :bind (("C-z p" . gptel-builder))

  :custom
  (gptel-builder-root (locate-user-emacs-file "prompts/templates/"))
  (gptel-builder-compiled-directory (locate-user-emacs-file "prompts/"))
  (gptel-builder-subagent-directory (locate-user-emacs-file "gptel-agents/"))
  (gptel-builder-default-tools
   '(("time"            . "current_datetime")
     ("custom-tasklist" . "TaskCreate")
     ("custom-tasklist" . "TaskList")
     ("custom-tasklist" . "TaskGet")
     ("custom-tasklist" . "TaskUpdate")
     ("custom-tasklist" . "TaskSave")
     ("custom-tasklist" . "TaskLoad")))
  :config
  (gptel-builder-define-preset 'dev-stage1-discovery-partner-divergent-exploration
                               :description "A discovery partner for divergent exploration"
                               :recipe '(:selections
                                         ((roles "dev/stage1-role-discovery-partner.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage1-skill-divergent-exploration.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage1-output-discovery-digest.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go"
                               :model 'glm-5.2
                               :request-params '(:thinking (:type "enabled") :reasoning_effort "high")
                               :temperature 1)

  (gptel-builder-define-preset 'dev-stage2-analyst-architect-requirements-synthesis
                               :description "An architect analyst for requirements synthesis"
                               :recipe '(:selections
                                         ((roles "dev/stage2-4-role-analyst-architect.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage2-skill-requirements-synthesis.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage2-output-design-brief.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go"
                               :model 'glm-5.2
                               :request-params '(:thinking (:type "enabled") :reasoning_effort "high")
                               :temperature 1)

  (gptel-builder-define-preset 'dev-stage3-analyst-architect-design-resolution
                               :description "An architect analyst for design resolution"
                               :recipe '(:selections
                                         ((roles "dev/stage2-4-role-analyst-architect.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage3-skill-design-resolution.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage3-output-design-spec.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go"
                               :model 'glm-5.2
                               :request-params '(:thinking (:type "enabled") :reasoning_effort "max")
                               :temperature 0.7)

  (gptel-builder-define-preset 'dev-stage4-analyst-architect-work-breakdown
                               :description "An architect analyst for work breakdown"
                               :recipe '(:selections
                                         ((roles "dev/stage2-4-role-analyst-architect.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage4-skill-work-breakdown.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage4-output-task-plan.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :model 'glm-5.2
                               :request-params '(:thinking (:type "enabled") :reasoning_effort "high")
                               :temperature 0.5)

  (gptel-builder-define-preset 'dev-stage5-implementer-interface-scaffolding
                               :description "An implementer for interface scaffolding"
                               :recipe '(:selections
                                         ((roles "dev/stage5-6-role-implementer.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage5-skill-interface-scaffolding.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage5-output-scaffold.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :stream nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go (Qwen Plus non streaming)"
                               :model 'qwen3.7-plus
                               :include-reasoning nil
                               :request-params '(:thinking (:type "disabled"))
                               :temperature 0.1)

  (gptel-builder-define-preset 'dev-stage6-implementer-implementation
                               :description "An implementer to implement (wow)"
                               :recipe '(:selections
                                         ((roles "dev/stage5-6-role-implementer.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage6-skill-implementation.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage6-output-implementation.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :stream nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go"
                               :model 'glm-5.2
                               :request-params '(:thinking (:type "enabled") :reasoning_effort "max"))

  (gptel-builder-define-preset 'dev-stage7-verifier-verification
                               :description "A verifier to verify (amazing)"
                               :recipe '(:selections
                                         ((roles "dev/stage7-role-verifier.org")
                                          (skills "dev/stage1-7-skill-org-markup-output.org" "dev/stage7-skill-verification.org")
                                          (projects "dev/stages.org")
                                          (outputs "dev/stage7-output-verification-report.org"))
                                         :datetime t :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :org-convert-response nil
                               :tools '("current_datetime")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go (Anthropic)"
                               :model 'minimax-m3
                               :request-params '(:thinking (:type "adaptive"))
                               :temperature 1)

  (gptel-builder-define-preset 'debate_orchestrator-no_debater
                               :description "Debate orchestrator that pick most relevant debaters (but debate roster is empty)"
                               :recipe '(:selections ((roles "debate/debate_orchestrator.org")
                                                      (skills "debate/floor_mgmt_relevant.org")
                                                      (projects)
                                                      (outputs))
                                                     :datetime nil :mode frozen :agentic t :agentic-skills ("_agentic_debate.org") :subagents nil)
                               :parents '(gptel-agent)
                               :tools '("Agent")
                               :use-tools t
                               :confirm-tool-calls 'auto)

  (gptel-builder-define-preset 'game_design
                               :description "A game designer partner, focused on emergent design and procedural generation"
                               :recipe '(:selections
                                         ((roles "game/game_designer.org")
                                          (skills "game/algorithmic_design.org" "game/character_design.org" "game/emergent_design.org" "game/game_system_design.org" "game/gameplay_ideation.org" "game/narrative_design.org" "game/playtest_critique.org" "game/procedural_generation.org" "game/ux_ergonomics.org")
                                          (projects)
                                          (outputs))
                                         :datetime nil :mode frozen :agentic nil :agentic-skills nil :subagents nil)
                               :tools 'nil
                               :use-tools t
                               :confirm-tool-calls 'auto))

;;;; Make MCP tool-call timeouts report instead of hanging ------------------
;; mcp-async-call-tool sets :timeout but no :timeout-fn, so a timed-out
;; tools/call fires neither callback and gptel's tool counter never completes
;; ("Calling agent" forever).  This override is the stock function plus the
;; missing :timeout-fn.
(defun perso/mcp-async-call-tool (connection name arguments callback error-callback)
  "Timeout-reporting replacement for `mcp-async-call-tool'."
  (jsonrpc-async-request
   connection :tools/call
   (list :name name :arguments (if arguments arguments #s(hash-table)))
   :timeout (mcp--timeout connection)
   :success-fn (lambda (res) (funcall callback res))
   :error-fn   (jsonrpc-lambda (&key code message _data)
                 (funcall error-callback code message))
   :timeout-fn (lambda ()
                 (funcall error-callback 'timeout
                          (format "MCP tool '%s' timed out after %ss"
                                  name (or (mcp--timeout connection)
                                           jsonrpc-default-request-timeout))))))

(with-eval-after-load 'mcp
  (advice-add 'mcp-async-call-tool :override #'perso/mcp-async-call-tool))

(use-package eca
  :custom
  (eca-chat-use-side-window t)
  (eca-chat-window-side 'left)
  (eca-chat-window-width 0.35)
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :hook (eca-chat-mode . disable-text-analysis-modes))

;; LLM stack management (Portainer + menu) is in another dedicated file
(when (locate-library "perso-llm")
  (autoload 'perso/llm-menu "perso-llm" nil t)
  (global-set-key (kbd "C-z @") #'perso/llm-menu))

;; Fetching Opencode Go models from subscription
(when (locate-library "opencode-go-gptel")
  (autoload 'my/opencode-go-gptel-config "opencode-go-gptel" nil t))

;; ;; Claude code integration
;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-z c" . claude-code-ide-menu)
;;   :custom
;;   (claude-code-ide-window-side 'left)
;;   (claude-code-ide-window-width 80)
;;   :config
;;   (use-package vterm
;;     :ensure t)
;;   (claude-code-ide-emacs-tools-setup))

(provide 'init-ai)
;;; init-ai.el ends here
