;;; commandcode-gptel.el --- Generate gptel backends for Command Code -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `my/commandcode-gptel-config' asks Command Code which models its
;; Provider API serves and which of them your plan includes, reads each
;; model's wire protocol and reasoning controls from Command Code's own
;; catalog, optionally PROBES THE GATEWAY to confirm them, and writes
;; ready-to-paste `gptel-make-openai' / `gptel-make-anthropic' blocks into
;; a buffer.  Save that buffer as gptel-commandcode-models.el.
;;
;; This is the Command Code twin of opencode-go-gptel.el.  Every model
;; exposing more than one reasoning-effort value gets one explicitly-named
;; gptel model symbol per value:
;;
;;     deepseek/deepseek-v4.1-flash-effort-low
;;     deepseek/deepseek-v4.1-flash-effort-high
;;     deepseek/deepseek-v4.1-flash-effort-max
;;
;; WHERE THE DATA COMES FROM
;;
;; Unlike OpenCode Go, Command Code publishes everything needed, so
;; models.dev is not used (it also lacks about a quarter of the models):
;;
;;   /provider/v1/models              `supported_endpoints' -> backend
;;   /provider/v1/models?view=client  minimumPlan, reasoningEfforts,
;;                                    pricing, inputModalities -- the
;;                                    list the CLI itself reads
;;   /alpha/billing/subscriptions     your plan id (undocumented; the
;;                                    CLI's own source)
;;
;; The two model lists need no key.  The plan filter follows the CLI's
;; rule: a model is kept when your plan ranks at or above its minimumPlan
;; in `my/cmdc-plan-ladder', and anything unreadable keeps every model.
;;
;; PROBING -- see `my/cmdc-probe'
;;
;; Off by default: there is no protocol to guess, since the gateway names
;; each model's endpoints itself.  Set `my/cmdc-probe' to `full' to send
;; one 16-token request per model and per effort value, and emit only what
;; the gateway accepts.
;;
;; HOW THE EFFORT VARIANTS WORK
;;
;; gptel derives the wire model id from the symbol name
;; (`gptel--model-name' is just `symbol-name'), so
;; `deepseek/deepseek-v4.1-flash-effort-low' would be sent verbatim and
;; rejected.  Each variant therefore carries
;; `:request-params (:model "deepseek/deepseek-v4.1-flash" ...)', which
;; rewrites it.  `gptel--request-data' merges the model's request params
;; last, after the backend's and those set by presets, so they win.
;;
;; Effort field by protocol:
;;   openai-compatible (/chat/completions) -> :reasoning_effort "low"
;;   anthropic         (/messages)         -> :output_config (:effort "low")
;;
;; Usage:
;;   M-x my/commandcode-gptel-config          ; catalog only
;;   (setq my/cmdc-probe 'full)               ; validate everything
;;   (setq my/cmdc-plan "individual-goat")    ; skip plan detection
;;   (setq my/cmdc-plan t)                    ; keep every model

;;; Code:

(require 'url)
(require 'seq)
(require 'subr-x)

(declare-function gptel-api-key-from-auth-source "gptel")

;;;; Configuration

(defvar my/cmdc-host "api.commandcode.ai"
  "Host for the generated backends and for every request.")

(defvar my/cmdc-models-path "/provider/v1/models"
  "Models list.  Each entry carries `supported_endpoints'.")

(defvar my/cmdc-catalog-path "/provider/v1/models?view=client"
  "Client view of the models list, the one the CLI reads.
Adds minimumPlan, reasoningEfforts, pricing and inputModalities.")

(defvar my/cmdc-subscription-path "/alpha/billing/subscriptions"
  "Undocumented endpoint the CLI reads your plan id from.")

(defvar my/cmdc-api-key
  (lambda () (gptel-api-key-from-auth-source my/cmdc-host))
  "Command Code API key: a string, or a function returning one.")

(defvar my/cmdc-openai-endpoint "/provider/v1/chat/completions"
  "Endpoint path for openai-compatible models.")

(defvar my/cmdc-anthropic-endpoint "/provider/v1/messages"
  "Endpoint path for Anthropic Messages models.")

(defvar my/cmdc-openai-name "Command Code"
  "Name of the generated `gptel-make-openai' backend.")

(defvar my/cmdc-anthropic-name "Command Code (Anthropic)"
  "Name of the generated `gptel-make-anthropic' backend.")

(defvar my/cmdc-plan nil
  "Your Command Code plan id, or nil to ask the billing endpoint.

A string from `my/cmdc-plan-ladder', such as \"individual-goat\", skips
the billing request.  t keeps every model: use it if you buy extra usage
credits, which unlock the models above your plan at on-demand rates.")

(defvar my/cmdc-plan-ladder
  '("individual-go" "individual-go-v1" "individual-goat" "individual-pro"
    "individual-pro-v1" "individual-provider" "individual-max"
    "individual-ultra" "teams-pro")
  "Plan ids from lowest to highest, as the CLI (1.88.2) ranks them.
A model is kept when your plan ranks at or above its minimumPlan.  A plan
missing from this list keeps every model, as the CLI does.")

(defvar my/cmdc-probe nil
  "How much to verify against the live gateway before emitting.

nil      (default) Catalog only.  No requests are sent besides the
         listings and the plan lookup.

`full'   Send one request per model to confirm it is really usable,
         and one per effort value to confirm the gateway accepts it.
         Rejected values are dropped and rejected models are reported
         rather than emitted.  This ignores `my/cmdc-skip-efforts', so
         it also settles \"off\".  On the GOAT plan that is about 220
         requests capped at `my/cmdc-probe-max-tokens' output tokens
         each: cheap, but several minutes of a blocked Emacs.")

(defvar my/cmdc-probe-max-tokens 16
  "Output cap for probe requests.  Keep this small; probes are billable.")

(defvar my/cmdc-skip-efforts '("off")
  "Effort values to omit when `my/cmdc-probe' is not `full'.

Either a flat list of strings applied to every model, or an alist of
\(MODEL-ID . VALUES) with the symbol t usable as a catch-all key.

The catalog lists \"off\" for the DeepSeek models, but the Provider API
was reported in September 2026 to answer HTTP 400 to it: its
`reasoning_effort' accepts only low|medium|high|xhigh|max.  The CLI sends
\"off\" on its own private route, which proves nothing for this one.
With `my/cmdc-probe' set to `full' this variable is ignored in favour of
what the gateway actually accepts.")

;;;; HTTP

(defun my/cmdc--key ()
  "Resolve `my/cmdc-api-key' to a string."
  (if (functionp my/cmdc-api-key)
      (funcall my/cmdc-api-key)
    my/cmdc-api-key))

(defun my/cmdc--fetch-json (path &optional bearer)
  "GET PATH on `my/cmdc-host' and return its JSON body as nested alists.
BEARER, when non-nil, is sent as an Authorization header."
  (let* ((url (concat "https://" my/cmdc-host path))
         (url-request-method "GET")
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

(defun my/cmdc--rows (path)
  "Return the models listed at PATH as an alist of (ID-STRING . ROW)."
  (let ((rows (alist-get 'data (my/cmdc--fetch-json path))))
    (unless rows
      (error "Could not find any models in the response from %s" path))
    (mapcar (lambda (row) (cons (alist-get 'id row) row)) rows)))

(defun my/cmdc--plan (key)
  "Return (PLAN . SOURCE) describing your plan id and how it was found.
PLAN is nil when it cannot be read, which keeps every model.  KEY is
your API key, used only when the billing endpoint must be asked."
  (cond ((eq my/cmdc-plan t)
         (cons nil "not filtered: my/cmdc-plan is t"))
        ((stringp my/cmdc-plan)
         (cons my/cmdc-plan "from my/cmdc-plan"))
        (t
         (condition-case err
             (let* ((sub (alist-get 'data (my/cmdc--fetch-json
                                           my/cmdc-subscription-path key)))
                    (plan (alist-get 'planId sub))
                    (status (alist-get 'status sub)))
               ;; The CLI ignores a plan whose subscription is not live.
               (if (and plan (member status '("active" "trialing" "past_due")))
                   (cons plan "from the billing endpoint")
                 (cons nil (format "unread, so not filtered: planId %S, status %S"
                                   plan status))))
           (error (cons nil (format "unread, so not filtered: %s"
                                    (error-message-string err))))))))

;;;; Probing

(defun my/cmdc--probe (id protocol effort key &optional cap)
  "Send one minimal request for model ID and report what happened.
PROTOCOL selects the endpoint and request shape.  EFFORT, when non-nil, is
included so the value itself is validated.  KEY is your API key.  CAP
is the keyword to use for
the output limit (`:max_tokens' or `:max_completion_tokens'), or nil to
send no limit at all -- which is what gptel itself does unless
`gptel-max-tokens' is set.  Returns a cons of the HTTP status (nil on a
transport failure) and a short body excerpt."
  (let* ((anthropicp (eq protocol 'anthropic))
         (url (concat "https://" my/cmdc-host
                      (if anthropicp
                          my/cmdc-anthropic-endpoint
                        my/cmdc-openai-endpoint)))
         (body (append
                (list :model id
                      :messages (vector (list :role "user" :content "hi")))
                ;; Messages always requires a limit; chat/completions does not.
                (cond (anthropicp (list :max_tokens my/cmdc-probe-max-tokens))
                      (cap (list cap my/cmdc-probe-max-tokens)))
                (when effort
                  (if anthropicp
                      (list :output_config (list :effort effort))
                    (list :reasoning_effort effort)))))
         (url-request-method "POST")
         (url-request-extra-headers
          ;; Authenticate as gptel will: `gptel-make-anthropic' sends
          ;; x-api-key, `gptel-make-openai' a bearer token.
          (if anthropicp
              (list (cons "Content-Type" "application/json")
                    (cons "Accept-Encoding" "identity")
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

(defun my/cmdc--ok-p (result)
  "Return non-nil when a `my/cmdc--probe' RESULT indicates acceptance."
  (equal (car result) 200))

(defun my/cmdc--final-p (result)
  "Return non-nil when RESULT is a refusal no other request shape can fix.
That is any 401, 403 or 429 -- key, plan, region or usage window -- and
a 400 that names the model or an option value rather than the body."
  (or (memq (car result) '(401 403 429))
      (string-match-p "unsupported_model\\|Invalid option" (or (cdr result) ""))))

(defun my/cmdc--probe-retry (id protocol effort key)
  "Probe ID, escalating through request shapes before giving up.
PROTOCOL, EFFORT and KEY are as for `my/cmdc--probe'.

gptel sends no output limit at all unless `gptel-max-tokens' is set, and
the OpenAI reasoning family wants `max_completion_tokens' rather than the
deprecated `max_tokens'.  A probe that only ever sends `max_tokens' is
therefore stricter than gptel and can report a false negative.

So on anything that is not a final refusal (see `my/cmdc--final-p'),
retry with no limit and then with `max_completion_tokens' before
recording a failure.  Messages requires a limit, so that surface gets a
single attempt."
  (let ((shapes (if (eq protocol 'anthropic)
                    '(:max_tokens)
                  (list :max_tokens nil :max_completion_tokens)))
        (last nil) (win nil))
    (while (and shapes (not win))
      (let ((r (my/cmdc--probe id protocol effort key (car shapes))))
        (setq last r shapes (cdr shapes))
        (cond ((my/cmdc--ok-p r) (setq win r))
              ((my/cmdc--final-p r) (setq shapes nil))
              (shapes (sleep-for 1)))))
    (or win last)))

;;;; Metadata extraction

(defun my/cmdc--protocol (endpoints)
  "Classify the wire protocol from ENDPOINTS, a model's `supported_endpoints'.
Returns `openai', `anthropic', or `responses' when the model is served on
neither surface gptel can reach."
  (cond ((member "/chat/completions" endpoints) 'openai)
        ((member "/messages" endpoints) 'anthropic)
        (t 'responses)))

(defun my/cmdc--in-plan-p (plan row)
  "Return non-nil when PLAN includes the model described by catalog ROW.
A nil PLAN, or a plan or minimumPlan missing from `my/cmdc-plan-ladder',
counts as included."
  (let ((have (and plan (seq-position my/cmdc-plan-ladder plan)))
        (need (seq-position my/cmdc-plan-ladder (alist-get 'minimumPlan row))))
    (or (null have) (null need) (>= have need))))

(defun my/cmdc--skipped (id)
  "Return the effort values to omit for model ID."
  (cond ((null my/cmdc-skip-efforts) nil)
        ((stringp (car my/cmdc-skip-efforts)) my/cmdc-skip-efforts)
        (t (append (cdr (assoc id my/cmdc-skip-efforts))
                   (cdr (assq t my/cmdc-skip-efforts))))))

(defun my/cmdc--efforts (id row)
  "Return catalog ROW's reasoning-effort values for model ID."
  (let ((skip (my/cmdc--skipped id)))
    (seq-remove (lambda (v) (member v skip))
                (alist-get 'reasoningEfforts row))))

(defun my/cmdc--capabilities (row)
  "Return gptel capability symbols for catalog ROW.
Every model gets `tool-use', since the CLI drives each one with tools.
gptel itself only acts on `media' and, for Anthropic, `cache'; the rest
is shown in its model menu."
  (let ((caps (list 'tool-use)))
    (when (alist-get 'reasoningEfforts row) (push 'reasoning caps))
    (when (alist-get 'cacheReadCost (alist-get 'pricing row)) (push 'cache caps))
    ;; The gateway takes text and images only; it rejects file parts.
    (when (member "image" (alist-get 'inputModalities row)) (push 'media caps))
    (nreverse caps)))

;;;; Rendering

(defun my/cmdc--entry (sym id row protocol effort)
  "Render the gptel model spec SYM for model ID as a string.
ROW is the model's catalog entry, or nil to emit it bare.  PROTOCOL
decides how EFFORT, when non-nil, is sent."
  (let* ((name (or (alist-get 'name row) id))
         (caps (and row (my/cmdc--capabilities row)))
         (ctx (alist-get 'context_length row))
         (price (alist-get 'pricing row))
         (out '()))
    (push (format "    (%S" sym) out)
    (push (format "     :description %S"
                  (if effort (format "%s [effort %s]" name effort) name))
          out)
    (when caps
      (push (format "     :capabilities (%s)"
                    (mapconcat #'symbol-name caps " "))
            out))
    (when (memq 'media caps)
      (push "     :mime-types (\"image/jpeg\" \"image/png\" \"image/webp\" \"image/gif\")"
            out))
    (when (numberp ctx)
      (push (format "     :context-window %d" (max 1 (round (/ ctx 1000.0))))
            out))
    (when (numberp (alist-get 'inputCost price))
      (push (format "     :input-cost %s" (alist-get 'inputCost price)) out))
    (when (numberp (alist-get 'outputCost price))
      (push (format "     :output-cost %s" (alist-get 'outputCost price)) out))
    (push (format "     :request-params (:model %S%s))"
                  id
                  (cond ((null effort) "")
                        ((eq protocol 'anthropic)
                         (format " :output_config (:effort %S)" effort))
                        (t (format " :reasoning_effort %S" effort))))
          out)
    (string-join (nreverse out) "\n")))

(defun my/cmdc--specs (rec)
  "Return the list of spec strings for record REC.
REC is (ID ROW PROTOCOL EFFORTS)."
  (let ((id (nth 0 rec)) (row (nth 1 rec))
        (proto (nth 2 rec)) (efforts (nth 3 rec)))
    (cons (my/cmdc--entry (intern id) id row proto nil)
          (when (>= (length efforts) 2)
            (mapcar (lambda (e)
                      (my/cmdc--entry (intern (format "%s-effort-%s" id e))
                                      id row proto e))
                    efforts)))))

(defun my/cmdc--backend-block (constructor name endpoint records)
  "Render a CONSTRUCTOR call defining backend NAME on ENDPOINT.
Return nil when RECORDS is empty."
  (let ((specs (apply #'append (mapcar #'my/cmdc--specs records))))
    (when specs
      (concat
       (format "(%s %S\n" constructor name)
       (format "  :host %S\n" my/cmdc-host)
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
(defun my/commandcode-gptel-config ()
  "Generate gptel backend blocks for your Command Code subscription.
Lists the models the Provider API serves, keeps those your plan includes,
reads each one's protocol and effort values from Command Code's catalog,
verifies them against the live gateway per `my/cmdc-probe', and writes
the result to a buffer."
  (interactive)
  (message "Fetching Command Code model lists...")
  (let* ((served (my/cmdc--rows my/cmdc-models-path))
         (catalog (my/cmdc--rows my/cmdc-catalog-path))
         (fullp (eq my/cmdc-probe 'full))
         (key (when (or fullp (null my/cmdc-plan)) (my/cmdc--key)))
         (_ (message "Reading your Command Code plan..."))
         (plan-info (my/cmdc--plan key))
         (plan (car plan-info))
         (ids (sort (mapcar #'car served)
                    (lambda (a b) (string< (downcase a) (downcase b)))))
         (records '()) (unknown '()) (locked '())
         (rejected '()) (dropped '()) (n 0))
    (dolist (id ids)
      (setq n (1+ n))
      (let* ((row (cdr (assoc id catalog)))
             (proto (my/cmdc--protocol
                     (alist-get 'supported_endpoints (cdr (assoc id served)))))
             (efforts (my/cmdc--efforts id row))
             (dead nil))
        (unless row (push id unknown))
        (if (not (my/cmdc--in-plan-p plan row))
            (push (cons id (alist-get 'minimumPlan row)) locked)
          ;; Full mode: confirm the model works at all, then confirm each
          ;; effort value, ignoring what the catalog claims.
          (when (and fullp (memq proto '(openai anthropic)))
            (message "[%d/%d] probing %s..." n (length ids) id)
            (let ((r (my/cmdc--probe-retry id proto nil key)))
              (unless (my/cmdc--ok-p r)
                (setq dead t)
                (push (cons id (cdr r)) rejected)))
            (unless dead
              (setq efforts
                    (seq-filter
                     (lambda (e)
                       (message "[%d/%d] probing %s effort=%s..."
                                n (length ids) id e)
                       (or (my/cmdc--ok-p (my/cmdc--probe-retry id proto e key))
                           (ignore (push (format "%s=%s" id e) dropped))))
                     ;; Ignore `my/cmdc-skip-efforts' here: the gateway's
                     ;; own answer supersedes the hand-maintained list.
                     (let ((my/cmdc-skip-efforts nil))
                       (my/cmdc--efforts id row))))))
          (unless dead
            (push (list id row proto efforts) records)))))
    (setq records (nreverse records) unknown (nreverse unknown)
          locked (nreverse locked) rejected (nreverse rejected)
          dropped (nreverse dropped))
    (let* ((by (lambda (p) (seq-filter (lambda (r) (eq (nth 2 r) p)) records)))
           (openai-recs (funcall by 'openai))
           (anthro-recs (funcall by 'anthropic))
           (resp-recs (funcall by 'responses))
           (buf (get-buffer-create "*commandcode-gptel*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (emacs-lisp-mode)
          (insert
           (format ";;; gptel backends for Command Code -- generated %s  -*- lexical-binding: t; -*-\n"
                   (format-time-string "%Y-%m-%d %H:%M"))
           (format ";;; plan: %s (%s)\n" (or plan "unknown") (cdr plan-info))
           (format ";;; probe mode: %s\n" (or my/cmdc-probe "off"))
           (format ";;; %d models: %d openai-compatible, %d anthropic, %d unavailable\n"
                   (length records) (length openai-recs)
                   (length anthro-recs) (length resp-recs))
           ";;;\n"
           ";;; Effort variants override :model via :request-params, which gptel\n"
           ";;; merges last, so they beat both the symbol name and any preset.\n")
          (when (and (not fullp) my/cmdc-skip-efforts)
            (insert (format ";;; Effort values skipped by config: %S\n"
                            my/cmdc-skip-efforts)))
          (when dropped
            (insert (format ";;; Effort values rejected by the gateway: %s\n"
                            (string-join dropped " "))))
          (insert "\n")
          (when-let* ((b (my/cmdc--backend-block
                          "gptel-make-openai" my/cmdc-openai-name
                          my/cmdc-openai-endpoint openai-recs)))
            (insert b "\n"))
          (when-let* ((b (my/cmdc--backend-block
                          "gptel-make-anthropic" my/cmdc-anthropic-name
                          my/cmdc-anthropic-endpoint anthro-recs)))
            (insert b "\n"))
          (when resp-recs
            (insert ";; ---- Not emitted: served on neither chat/completions nor messages ----\n")
            (dolist (r resp-recs)
              (insert (format ";;   %s\n" (nth 0 r)))))
          (when rejected
            (insert "\n;; ---- Probe failures ----\n")
            (dolist (p rejected)
              (insert (format ";;   %-36s %s\n" (car p) (cdr p)))))
          (when unknown
            (insert "\n;; ---- Served but absent from the client catalog ----\n")
            (dolist (id unknown) (insert (format ";;   %s\n" id)))
            (insert ";; Emitted bare: no limits, capabilities or effort variants.\n"))
          (when locked
            (insert "\n;; ---- Served but not in your plan (minimum plan) ----\n")
            (dolist (l locked)
              (insert (format ";;   %-36s %s\n" (car l) (cdr l))))))
        (goto-char (point-min)))
      (pop-to-buffer buf)
      (message "Command Code: %d models (%d openai, %d anthropic), %d outside your plan%s"
               (length records) (length openai-recs) (length anthro-recs)
               (length locked)
               (if rejected (format ", %d probe failures" (length rejected)) "")))))

(provide 'commandcode-gptel)
;;; commandcode-gptel.el ends here
