;;; opencode-go-gptel.el --- Generate gptel backends for OpenCode Go -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `my/opencode-go-gptel-config' asks your OpenCode Go subscription which
;; models your key can reach, works out each model's wire protocol and
;; reasoning controls, optionally PROBES THE GATEWAY to confirm both, and
;; writes ready-to-paste `gptel-make-openai' / `gptel-make-anthropic' blocks
;; into a buffer.
;;
;; Every model exposing more than one reasoning-effort value gets one
;; explicitly-named gptel model symbol per value:
;;
;;     glm-5.3-flash-effort-low
;;     glm-5.3-flash-effort-high
;;     glm-5.3-flash-effort-max
;;
;; PROBING -- see `my/ocgo-probe'
;;
;; Metadata alone is not enough.  models.dev tags gpt-5.6-luna, grok-4.5,
;; grok-4.6 and muse-spark-1.2-contributor with npm `@ai-sdk/openai', i.e.
;; the Responses API, for which gptel has no public constructor targeting a
;; third-party host.  The gateway nevertheless appears to serve that family
;; on /chat/completions.  Only a real request can settle it, so by default
;; this sends one 16-token request per such model and promotes the ones that
;; answer into the openai-compatible backend.  Set `my/ocgo-probe' to `full'
;; to also validate every model and every effort value empirically, which
;; makes `my/ocgo-skip-efforts' unnecessary.
;;
;; HOW THE EFFORT VARIANTS WORK
;;
;; gptel derives the wire model id from the symbol name
;; (`gptel--model-name' is just `symbol-name'), so `glm-5.3-flash-effort-low'
;; would be sent verbatim and rejected.  Each variant therefore carries
;; `:request-params (:model "glm-5.3-flash" ...)', which rewrites it.
;;
;; This is reliable because `gptel--request-data' (identical in
;; gptel-openai.el and gptel-anthropic.el) ends with:
;;
;;     (gptel--merge-plists
;;      prompts-plist              ; contains :model from the symbol name
;;      gptel--request-params      ; buffer-local / set by presets
;;      (gptel-backend-request-params gptel-backend)
;;      (gptel--model-request-params gptel-model))
;;
;; and `gptel--merge-plists' documents "Later plists in the sequence take
;; precedence over earlier ones".  Model params merge LAST, so they override
;; both the derived :model and anything your presets set.  Your
;; prompt-focused presets stay model-agnostic and are unaffected.
;;
;; Effort field by protocol:
;;   openai-compatible (/chat/completions) -> :reasoning_effort "low"
;;   anthropic         (/messages)         -> :output_config (:effort "low")
;;
;; Usage:
;;   M-x my/opencode-go-gptel-config          ; probe the responses bucket
;;   C-u M-x my/opencode-go-gptel-config      ; also refresh models.dev
;;   (setq my/ocgo-probe 'full)               ; validate everything
;;   (setq my/ocgo-probe nil)                 ; metadata only, no requests

;;; Code:

(require 'url)
(require 'seq)
(require 'subr-x)

;;;; Configuration

(defvar my/ocgo-models-url "https://opencode.ai/zen/go/v1/models"
  "Endpoint listing the models available to your OpenCode Go key.")

(defvar my/ocgo-catalog-url "https://models.dev/api.json"
  "models.dev catalog, used for protocol and reasoning metadata.")

(defvar my/ocgo-provider-id "opencode-go"
  "Provider key for OpenCode Go inside `my/ocgo-catalog-url'.")

(defvar my/ocgo-api-key
  (lambda () (gptel-api-key-from-auth-source "opencode.ai"))
  "OpenCode Go API key: a string, or a function returning one.")

(defvar my/ocgo-host "opencode.ai"
  "Host for the generated backends.")

(defvar my/ocgo-openai-endpoint "/zen/go/v1/chat/completions"
  "Endpoint path for openai-compatible models.")

(defvar my/ocgo-anthropic-endpoint "/zen/go/v1/messages"
  "Endpoint path for Anthropic Messages models.")

(defvar my/ocgo-openai-name "OpenCode Go"
  "Name of the generated `gptel-make-openai' backend.")

(defvar my/ocgo-anthropic-name "OpenCode Go (Anthropic)"
  "Name of the generated `gptel-make-anthropic' backend.")

(defvar my/ocgo-probe 'responses
  "How much to verify against the live gateway before emitting.

nil          Metadata only.  No requests are sent.  Responses-protocol
             models are skipped, as models.dev classifies them.

`responses'  (default) Send one request per responses-protocol model to
             /chat/completions and promote the ones that answer.  Costs
             about four 16-token requests, so a fraction of a cent
             against your usage limits.

`full'       Additionally send one request per model to confirm it is
             really usable, and one per effort value to confirm the
             gateway accepts it.  Rejected values are dropped and
             rejected models are reported rather than emitted.  This
             makes `my/ocgo-skip-efforts' redundant but costs roughly
             one request per emitted model symbol -- on the order of
             60-70 requests and a minute or two of wall time.")

(defvar my/ocgo-probe-max-tokens 16
  "Output cap for probe requests.  Keep this small; probes are billable.")

(defvar my/ocgo-probe-fallbacks '(openai anthropic)
  "Surfaces to try, in order, for a model models.dev calls Responses-only.
The gateway enforces format per model, so a model refused on
chat/completions may still answer on Messages.  Whichever surface returns
200 decides which backend the model is emitted into.")

(defvar my/ocgo-skip-efforts
  '(("gpt-5.6-luna" . ("none")))
  "Effort values to omit when `my/ocgo-probe' is not `full'.

Either a flat list of strings applied to every model, or an alist of
\(MODEL-ID . VALUES) with the symbol t usable as a catch-all key.

The default is per-model on purpose.  Zen's OpenAI-family upstream
validates effort against minimal|low|medium|high|xhigh|max and returns
HTTP 400 on `none', so gpt-5.6-luna must not emit it.  But `none' is also
published for hy3 and hy4-preview, which are served on chat/completions
and appear to accept it -- a global (\"none\") would silently cost you
those two variants.  With `my/ocgo-probe' set to `full' this variable is
ignored in favour of what the gateway actually accepts.")

(defvar my/ocgo-protocol-overrides
  '(;; The Go endpoint table puts every Qwen on /zen/go/v1/messages, but
    ;; models.dev only carries the `[provider] npm = "@ai-sdk/anthropic"'
    ;; override on qwen3.8-flash.  Without these, the rest would be
    ;; classified from the provider default (@ai-sdk/openai-compatible) and
    ;; misrouted into the chat/completions backend.
    ("qwen3.8-max"  . anthropic)
    ("qwen3.7-max"  . anthropic)
    ("qwen3.7-plus" . anthropic)
    ("qwen3.6-plus" . anthropic)
    ;; Not in the published Go table; inferred from its siblings.
    ("qwen3.5-plus" . anthropic))
  "Alist of (MODEL-ID-STRING . PROTOCOL) forcing classification.
PROTOCOL is one of the symbols `openai', `anthropic' or `responses'.  An
entry here wins over both models.dev and probing, so use it only where you
know better than the gateway's own answer.")

(defvar my/ocgo--catalog nil
  "Cached models.dev payload, so repeat runs skip the multi-MB download.")

;;;; HTTP

(defun my/ocgo--key ()
  "Resolve `my/ocgo-api-key' to a string."
  (if (functionp my/ocgo-api-key)
      (funcall my/ocgo-api-key)
    my/ocgo-api-key))

(defun my/ocgo--fetch-json (url &optional bearer)
  "GET URL and return its JSON body parsed as nested alists.
BEARER, when non-nil, is sent as an Authorization header."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          (append '(("Accept" . "application/json")
                    ("Accept-Encoding" . "identity"))
                  (when bearer
                    (list (cons "Authorization" (concat "Bearer " bearer))))))
         (buf (url-retrieve-synchronously url t t 90)))
    (unless buf
      (error "No response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (let ((status (bound-and-true-p url-http-response-status)))
            (goto-char (point-min))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Malformed response from %s" url))
            (when (and status (/= status 200))
              (error "%s returned HTTP %s: %s" url status
                     (buffer-substring-no-properties
                      (point) (min (point-max) (+ (point) 400)))))
            (json-parse-buffer :object-type 'alist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)))
      (kill-buffer buf))))

(defun my/ocgo--available-ids ()
  "Return the model ids reachable with your Go key, sorted, as strings."
  (let* ((json (my/ocgo--fetch-json my/ocgo-models-url (my/ocgo--key)))
         (rows (cond ((alist-get 'data json))
                     ((alist-get 'models json))
                     (t json)))
         (ids (delq nil
                    (mapcar (lambda (row)
                              (cond ((stringp row) row)
                                    ((consp row) (alist-get 'id row))))
                            rows))))
    (unless ids
      (error "Could not find any model ids in the response from %s"
             my/ocgo-models-url))
    (sort (delete-dups ids) #'string<)))

(defun my/ocgo--catalog (&optional refresh)
  "Return the models.dev payload, downloading it unless cached.
Non-nil REFRESH forces a re-download."
  (when (or refresh (null my/ocgo--catalog))
    (setq my/ocgo--catalog (my/ocgo--fetch-json my/ocgo-catalog-url)))
  my/ocgo--catalog)

;;;; Probing

(defun my/ocgo--probe (id protocol effort key &optional cap)
  "Send one minimal request for model ID and report what happened.
PROTOCOL selects the endpoint and request shape.  EFFORT, when non-nil, is
included so the value itself is validated.  CAP is the keyword to use for
the output limit (`:max_tokens' or `:max_completion_tokens'), or nil to
send no limit at all -- which is what gptel itself does unless
`gptel-max-tokens' is set.  Returns a cons of the HTTP status (nil on a
transport failure) and a short body excerpt."
  (let* ((anthropicp (eq protocol 'anthropic))
         (url (concat "https://" my/ocgo-host
                      (if anthropicp
                          my/ocgo-anthropic-endpoint
                        my/ocgo-openai-endpoint)))
         (body (append
                (list :model id
                      :messages (vector (list :role "user" :content "hi")))
                ;; Messages always requires a limit; chat/completions does not.
                (cond (anthropicp (list :max_tokens my/ocgo-probe-max-tokens))
                      (cap (list cap my/ocgo-probe-max-tokens)))
                (when effort
                  (if anthropicp
                      (list :output_config (list :effort effort))
                    (list :reasoning_effort effort)))))
         (url-request-method "POST")
         (url-request-extra-headers
          ;; Send only headers appropriate to the protocol.  Putting
          ;; anthropic-version on a chat/completions probe risks the gateway
          ;; dispatching on it and reporting a misleading failure.
          (if anthropicp
              (list (cons "Content-Type" "application/json")
                    (cons "Accept-Encoding" "identity")
                    (cons "Authorization" (concat "Bearer " key))
                    (cons "x-api-key" key)
                    (cons "anthropic-version" "2023-06-01"))
            (list (cons "Content-Type" "application/json")
                  (cons "Accept-Encoding" "identity")
                  (cons "Authorization" (concat "Bearer " key)))))
         (url-request-data
          (encode-coding-string (json-serialize body) 'utf-8))
         (buf (ignore-errors (url-retrieve-synchronously url t t 60))))
    (if (not buf)
        (cons nil "no response")
      (unwind-protect
          (with-current-buffer buf
            (let ((status (bound-and-true-p url-http-response-status)))
              (goto-char (point-min))
              (if (re-search-forward "\r?\n\r?\n" nil t)
                  (cons status
                        (replace-regexp-in-string
                         "[\n\r\t ]+" " "
                         (buffer-substring-no-properties
                          (point) (min (point-max) (+ (point) 240)))))
                (cons status "unparseable"))))
        (kill-buffer buf)))))

(defun my/ocgo--ok-p (result)
  "Return non-nil when a `my/ocgo--probe' RESULT indicates acceptance."
  (equal (car result) 200))

(defvar my/ocgo--probe-shape nil
  "Records which request shape last succeeded, for reporting.")

(defun my/ocgo--probe-retry (id protocol effort key)
  "Probe ID, escalating through request shapes before giving up.

A 500 from the gateway is ambiguous.  It can mean the model is not routed
on that surface -- but it can equally mean our probe's request shape is
wrong, which is easy to get wrong here: gptel sends no output limit at
all unless `gptel-max-tokens' is set, and the OpenAI reasoning family
wants `max_completion_tokens' rather than the deprecated `max_tokens'.
A probe that only ever sends `max_tokens' is therefore stricter than
gptel and can report a false negative.

So on anything that is not a clean typed refusal, retry with no limit and
then with `max_completion_tokens' before recording a failure.  Messages
requires a limit, so that surface gets a single attempt."
  (let ((shapes (if (eq protocol 'anthropic)
                    '(:max_tokens)
                  (list :max_tokens nil :max_completion_tokens)))
        (last nil) (win nil))
    (while (and shapes (not win))
      (let* ((cap (car shapes))
             (r (my/ocgo--probe id protocol effort key cap)))
        (setq last r shapes (cdr shapes))
        (cond ((my/ocgo--ok-p r)
               (setq win r my/ocgo--probe-shape (or cap 'none)))
              ;; A typed "not supported for format" is final; don't waste
              ;; further requests on it.
              ((string-match-p "not supported for format" (or (cdr r) ""))
               (setq shapes nil))
              (shapes (sleep-for 1)))))
    (or win last)))

;;;; Metadata extraction

(defun my/ocgo--protocol (meta provider-npm)
  "Classify the wire protocol for META, a models.dev model entry.
PROVIDER-NPM is the provider-level default.  Per-model `provider.shape'
and `provider.npm' override it."
  (let* ((pv (alist-get 'provider meta))
         (shape (alist-get 'shape pv))
         (npm (or (alist-get 'npm pv)
                  (alist-get 'npm meta)
                  provider-npm)))
    (cond ((equal shape "responses") 'responses)
          ((equal shape "completions") 'openai)
          ((equal npm "@ai-sdk/anthropic") 'anthropic)
          ((equal npm "@ai-sdk/openai") 'responses)
          (t 'openai))))

(defun my/ocgo--skipped (id)
  "Return the effort values to omit for model ID."
  (cond ((null my/ocgo-skip-efforts) nil)
        ((stringp (car my/ocgo-skip-efforts)) my/ocgo-skip-efforts)
        (t (append (cdr (assoc id my/ocgo-skip-efforts))
                   (cdr (assq t my/ocgo-skip-efforts))))))

(defun my/ocgo--efforts (id meta)
  "Return META's published reasoning-effort values for model ID.
Reads the `effort' member of `reasoning_options'; `toggle' and
`budget_tokens' members are ignored, so models offering only those
produce no variants."
  (let ((skip (my/ocgo--skipped id)))
    (seq-remove
     (lambda (v) (member v skip))
     (or (seq-some (lambda (opt)
                     (and (equal (alist-get 'type opt) "effort")
                          (alist-get 'values opt)))
                   (alist-get 'reasoning_options meta))
         '()))))

(defun my/ocgo--capabilities (meta)
  "Return gptel capability symbols for META."
  (let ((mods (alist-get 'input (alist-get 'modalities meta)))
        (caps '()))
    (when (alist-get 'tool_call meta) (push 'tool-use caps))
    (when (alist-get 'reasoning meta) (push 'reasoning caps))
    (when (alist-get 'structured_output meta) (push 'json caps))
    (when (alist-get 'cache_read (alist-get 'cost meta)) (push 'cache caps))
    (when (seq-intersection mods '("image" "pdf" "audio" "video") #'equal)
      (push 'media caps))
    (nreverse caps)))

(defun my/ocgo--mime-types (meta)
  "Return MIME types for META's supported input modalities."
  (let ((mods (alist-get 'input (alist-get 'modalities meta))))
    (append (when (member "image" mods)
              '("image/jpeg" "image/png" "image/webp" "image/gif"))
            (when (member "pdf" mods) '("application/pdf")))))

;;;; Rendering

(defun my/ocgo--entry (sym id meta protocol effort)
  "Render one gptel model spec as a string."
  (let* ((name (or (alist-get 'name meta) id))
         (caps (my/ocgo--capabilities meta))
         (mimes (my/ocgo--mime-types meta))
         (ctx (alist-get 'context (alist-get 'limit meta)))
         (cost (alist-get 'cost meta))
         (out '()))
    (push (format "    (%s" sym) out)
    (push (format "     :description %S"
                  (if effort (format "%s [effort %s]" name effort) name))
          out)
    (when caps
      (push (format "     :capabilities (%s)"
                    (mapconcat #'symbol-name caps " "))
            out))
    (when mimes
      (push (format "     :mime-types %S" mimes) out))
    (when (numberp ctx)
      (push (format "     :context-window %d" (max 1 (round (/ ctx 1000.0))))
            out))
    (when (numberp (alist-get 'input cost))
      (push (format "     :input-cost %s" (alist-get 'input cost)) out))
    (when (numberp (alist-get 'output cost))
      (push (format "     :output-cost %s" (alist-get 'output cost)) out))
    (when (stringp (alist-get 'knowledge meta))
      (push (format "     :cutoff-date %S" (alist-get 'knowledge meta)) out))
    (push (format "     :request-params (:model %S%s))"
                  id
                  (cond ((null effort) "")
                        ((eq protocol 'anthropic)
                         (format " :output_config (:effort %S)" effort))
                        (t (format " :reasoning_effort %S" effort))))
          out)
    (string-join (nreverse out) "\n")))

(defun my/ocgo--specs (rec)
  "Return the list of spec strings for record REC.
REC is (ID META PROTOCOL EFFORTS)."
  (let ((id (nth 0 rec)) (meta (nth 1 rec))
        (proto (nth 2 rec)) (efforts (nth 3 rec)))
    (if (< (length efforts) 2)
        (list (my/ocgo--entry (intern id) id meta proto nil))
      (cons (my/ocgo--entry (intern id) id meta proto nil)
            (mapcar (lambda (e)
                      (my/ocgo--entry (intern (format "%s-effort-%s" id e))
                                      id meta proto e))
                    efforts)))))

(defun my/ocgo--backend-block (constructor name endpoint records)
  "Render a backend definition, or nil when RECORDS is empty."
  (let ((specs (apply #'append (mapcar #'my/ocgo--specs records))))
    (when specs
      (concat
       (format "(%s %S\n" constructor name)
       (format "  :host %S\n" my/ocgo-host)
       (format "  :endpoint %S\n" endpoint)
       "  :protocol \"https\"\n"
       "  :stream t\n"
       "  :key #'gptel-api-key-from-auth-source\n"
       "  :models\n"
       "  '(\n"
       (string-join specs "\n")
       "))\n"))))

;;;; Entry point

;;;###autoload
(defun my/opencode-go-gptel-config (&optional refresh)
  "Generate gptel backend blocks for your OpenCode Go subscription.
Lists the models your key can reach, classifies each against models.dev,
verifies that against the live gateway per `my/ocgo-probe', and writes the
result to a buffer.  With prefix argument REFRESH, also re-download the
models.dev catalog."
  (interactive "P")
  (let ((key (my/ocgo--key)))
    (message "Fetching OpenCode Go model list...")
    (let* ((available (my/ocgo--available-ids))
           (_ (message "Fetching models.dev catalog (a few MB)..."))
           (catalog (my/ocgo--catalog refresh))
           (provider (alist-get (intern my/ocgo-provider-id) catalog))
           (provider-npm (alist-get 'npm provider))
           (models (alist-get 'models provider))
           (fullp (eq my/ocgo-probe 'full))
           (records '()) (unknown '()) (overridden '())
           (promoted '()) (rejected '()) (dropped '()) (n 0))
      (unless provider
        (user-error "No provider %S in %s" my/ocgo-provider-id
                    my/ocgo-catalog-url))
      (dolist (id available)
        (setq n (1+ n))
        (let* ((meta (alist-get (intern id) models))
               (forced (cdr (assoc id my/ocgo-protocol-overrides)))
               (proto (or forced (my/ocgo--protocol meta provider-npm)))
               (efforts (my/ocgo--efforts id meta))
               (dead nil))
          (when forced (push id overridden))
          (unless meta (push id unknown))
          ;; Responses-protocol models have no gptel constructor.  Ask the
          ;; gateway which surface, if any, it will serve them on.  grok-4.6
          ;; answers "not supported for format oa-compat" on chat/completions,
          ;; which proves the gateway enforces format per model -- so trying
          ;; Messages as a second surface is worth the extra request.
          (when (and (null forced) (eq proto 'responses) my/ocgo-probe)
            (let ((found nil))
              (dolist (cand my/ocgo-probe-fallbacks)
                (unless found
                  (message "[%d/%d] probing %s on %s..."
                           n (length available) id cand)
                  (let ((r (my/ocgo--probe-retry id cand nil key)))
                    (if (my/ocgo--ok-p r)
                        (setq found cand)
                      (push (cons (format "%s [%s]" id cand) (cdr r))
                            rejected)))))
              (when found
                (setq proto found)
                (push (format "%s->%s (limit: %s)" id found
                              (or my/ocgo--probe-shape "?"))
                      promoted))))
          ;; Full mode: confirm the model works at all, then confirm each
          ;; effort value, ignoring what the catalog claims.
          (when (and fullp (memq proto '(openai anthropic)))
            (message "[%d/%d] probing %s..." n (length available) id)
            (let ((r (my/ocgo--probe-retry id proto nil key)))
              (unless (my/ocgo--ok-p r)
                (setq dead t)
                (push (cons id (cdr r)) rejected)))
            (unless dead
              (setq efforts
                    (seq-filter
                     (lambda (e)
                       (message "[%d/%d] probing %s effort=%s..."
                                n (length available) id e)
                       (or (my/ocgo--ok-p (my/ocgo--probe-retry id proto e key))
                           (ignore (push (format "%s=%s" id e) dropped))))
                     ;; Ignore `my/ocgo-skip-efforts' here: the gateway's
                     ;; own answer supersedes the hand-maintained list.
                     (let ((my/ocgo-skip-efforts nil))
                       (my/ocgo--efforts id meta))))))
          (unless dead
            (push (list id meta proto efforts) records))))
      (setq records (nreverse records) unknown (nreverse unknown)
            overridden (nreverse overridden) promoted (nreverse promoted)
            rejected (nreverse rejected) dropped (nreverse dropped))
      (let* ((by (lambda (p) (seq-filter (lambda (r) (eq (nth 2 r) p)) records)))
             (openai-recs (funcall by 'openai))
             (anthro-recs (funcall by 'anthropic))
             (resp-recs (funcall by 'responses))
             (stale (seq-remove
                     (lambda (mid) (member mid available))
                     (mapcar (lambda (c) (symbol-name (car c))) models)))
             (buf (get-buffer-create "*opencode-go-gptel*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (emacs-lisp-mode)
            (insert
             (format ";;; gptel backends for OpenCode Go -- generated %s\n"
                     (format-time-string "%Y-%m-%d %H:%M"))
             (format ";;; probe mode: %s\n" (or my/ocgo-probe "off"))
             (format ";;; %d models: %d openai-compatible, %d anthropic, %d unavailable\n"
                     (length records) (length openai-recs)
                     (length anthro-recs) (length resp-recs))
             ";;;\n"
             ";;; Effort variants override :model via :request-params, which gptel\n"
             ";;; merges last, so they beat both the symbol name and any preset.\n")
            (when promoted
              (insert (format ";;; Promoted to chat/completions by probe: %s\n"
                              (string-join promoted " "))))
            (when overridden
              (insert (format ";;; Protocol forced by config: %s\n"
                              (string-join overridden " "))))
            (when dropped
              (insert (format ";;; Effort values rejected by the gateway: %s\n"
                              (string-join dropped " "))))
            (insert "\n")
            (when-let* ((b (my/ocgo--backend-block
                            "gptel-make-openai" my/ocgo-openai-name
                            my/ocgo-openai-endpoint openai-recs)))
              (insert b "\n"))
            (when-let* ((b (my/ocgo--backend-block
                            "gptel-make-anthropic" my/ocgo-anthropic-name
                            my/ocgo-anthropic-endpoint anthro-recs)))
              (insert b "\n"))
            (when resp-recs
              (insert ";; ---- Not emitted: responses protocol, probe declined ----\n")
              (dolist (r resp-recs)
                (insert (format ";;   %s\n" (nth 0 r)))))
            (when rejected
              (insert "\n;; ---- Probe failures ----\n")
              (dolist (p rejected)
                (insert (format ";;   %-28s %s\n" (car p) (cdr p))))
              (when (seq-some (lambda (p)
                                (string-match-p "DataPolicyError" (or (cdr p) "")))
                              rejected)
                (insert ";;\n;; ACTION: at least one model needs an explicit data-policy\n"
                        ";; opt-in in your workspace console (see the URL above).\n"
                        ";; Opt in, then re-run to pick it up.\n"))
              (when (seq-some (lambda (p)
                                (string-match-p "oa-compat" (or (cdr p) "")))
                              rejected)
                (insert ";;\n;; NOTE: \"not supported for format oa-compat\" is the gateway\n"
                        ";; enforcing protocol per model.  No client config can work\n"
                        ";; around it; that model is Responses-only.\n")))
            (when unknown
              (insert "\n;; ---- Reachable but absent from models.dev ----\n")
              (dolist (id unknown) (insert (format ";;   %s\n" id)))
              (insert ";; Emitted bare: no limits, capabilities or effort variants.\n"))
            (when stale
              (insert "\n;; ---- In models.dev but not reachable with your key ----\n")
              (dolist (id stale) (insert (format ";;   %s\n" id)))))
          (goto-char (point-min)))
        (pop-to-buffer buf)
        (message "OpenCode Go: %d models (%d openai, %d anthropic)%s%s"
                 (length records) (length openai-recs) (length anthro-recs)
                 (if promoted (format ", %d promoted" (length promoted)) "")
                 (if rejected (format ", %d probe failures" (length rejected)) ""))))))

(provide 'opencode-go-gptel)
;;; opencode-go-gptel.el ends here
