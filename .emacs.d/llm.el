;;; llm.el --- LLM stack management -*- lexical-binding: t; -*-

(require 'auth-source)
(require 'gptel)
(require 'transient)

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
    ("g" "Start gptel" perso/gptel)]
   ["Eca"
    ("e" "Start eca" eca)]])

(global-set-key (kbd "C-z @") #'perso/llm-menu)

;;; llm.el ends here
