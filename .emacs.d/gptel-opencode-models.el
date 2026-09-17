;;; gptel backends for OpenCode Go -- generated 2026-09-14 16:28  -*- lexical-binding: t; -*-
;;; probe mode: responses
;;; 37 models: 23 openai-compatible, 9 anthropic, 5 unavailable
;;;
;;; Effort variants override :model via :request-params, which gptel
;;; merges last, so they beat both the symbol name and any preset.
;;; Protocol forced by config: qwen3.5-plus qwen3.6-plus qwen3.7-max qwen3.7-plus qwen3.8-max

(gptel-make-openai "OpenCode Go"
  :host "opencode.ai"
  :endpoint "/zen/go/v1/chat/completions"
  :protocol "https"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models
  '(
    (deepseek-flash
     :description "deepseek-flash"
     :request-params (:model "deepseek-flash"))
    (deepseek-v4-flash
     :description "DeepSeek V4 Flash"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-flash"))
    (deepseek-v4-flash-effort-low
     :description "DeepSeek V4 Flash [effort low]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-flash" :reasoning_effort "low"))
    (deepseek-v4-flash-effort-high
     :description "DeepSeek V4 Flash [effort high]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-flash" :reasoning_effort "high"))
    (deepseek-v4-flash-effort-max
     :description "DeepSeek V4 Flash [effort max]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-flash" :reasoning_effort "max"))
    (deepseek-v4-flash-vision-exp
     :description "DeepSeek V4 Flash Vision Exp"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek-v4-flash-vision-exp"))
    (deepseek-v4-flash-vision-exp-effort-low
     :description "DeepSeek V4 Flash Vision Exp [effort low]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek-v4-flash-vision-exp" :reasoning_effort "low"))
    (deepseek-v4-flash-vision-exp-effort-high
     :description "DeepSeek V4 Flash Vision Exp [effort high]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek-v4-flash-vision-exp" :reasoning_effort "high"))
    (deepseek-v4-flash-vision-exp-effort-max
     :description "DeepSeek V4 Flash Vision Exp [effort max]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek-v4-flash-vision-exp" :reasoning_effort "max"))
    (deepseek-v4-pro
     :description "DeepSeek V4 Pro (New)"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.66
     :output-cost 1.98
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-pro"))
    (deepseek-v4-pro-effort-high
     :description "DeepSeek V4 Pro (New) [effort high]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.66
     :output-cost 1.98
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-pro" :reasoning_effort "high"))
    (deepseek-v4-pro-effort-max
     :description "DeepSeek V4 Pro (New) [effort max]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 0.66
     :output-cost 1.98
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4-pro" :reasoning_effort "max"))
    (deepseek-v4.1-flash
     :description "DeepSeek V4.1 Flash"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4.1-flash"))
    (deepseek-v4.1-flash-effort-low
     :description "DeepSeek V4.1 Flash [effort low]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4.1-flash" :reasoning_effort "low"))
    (deepseek-v4.1-flash-effort-high
     :description "DeepSeek V4.1 Flash [effort high]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4.1-flash" :reasoning_effort "high"))
    (deepseek-v4.1-flash-effort-max
     :description "DeepSeek V4.1 Flash [effort max]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :cutoff-date "2025-05"
     :request-params (:model "deepseek-v4.1-flash" :reasoning_effort "max"))
    (glm-5
     :description "GLM-5"
     :capabilities (tool-use reasoning cache)
     :context-window 203
     :input-cost 1
     :output-cost 3.2
     :cutoff-date "2025-04"
     :request-params (:model "glm-5"))
    (glm-5.1
     :description "GLM-5.1"
     :capabilities (tool-use reasoning cache)
     :context-window 203
     :input-cost 1.4
     :output-cost 4.4
     :cutoff-date "2025-04"
     :request-params (:model "glm-5.1"))
    (glm-5.2
     :description "GLM-5.2"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.2"))
    (glm-5.2-effort-high
     :description "GLM-5.2 [effort high]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.2" :reasoning_effort "high"))
    (glm-5.2-effort-max
     :description "GLM-5.2 [effort max]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.2" :reasoning_effort "max"))
    (glm-5.3
     :description "GLM-5.3"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.3"))
    (glm-5.3-effort-low
     :description "GLM-5.3 [effort low]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.3" :reasoning_effort "low"))
    (glm-5.3-effort-high
     :description "GLM-5.3 [effort high]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.3" :reasoning_effort "high"))
    (glm-5.3-effort-max
     :description "GLM-5.3 [effort max]"
     :capabilities (tool-use reasoning json cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "glm-5.3" :reasoning_effort "max"))
    (glm-5.3-flash
     :description "GLM-5.3-Flash"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif" "application/pdf")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "glm-5.3-flash"))
    (glm-5.3-flash-effort-low
     :description "GLM-5.3-Flash [effort low]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif" "application/pdf")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "glm-5.3-flash" :reasoning_effort "low"))
    (glm-5.3-flash-effort-high
     :description "GLM-5.3-Flash [effort high]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif" "application/pdf")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "glm-5.3-flash" :reasoning_effort "high"))
    (glm-5.3-flash-effort-max
     :description "GLM-5.3-Flash [effort max]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif" "application/pdf")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "glm-5.3-flash" :reasoning_effort "max"))
    (hy3
     :description "Hy3"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0.14
     :output-cost 0.58
     :request-params (:model "hy3"))
    (hy3-effort-none
     :description "Hy3 [effort none]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0.14
     :output-cost 0.58
     :request-params (:model "hy3" :reasoning_effort "none"))
    (hy3-effort-low
     :description "Hy3 [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0.14
     :output-cost 0.58
     :request-params (:model "hy3" :reasoning_effort "low"))
    (hy3-effort-high
     :description "Hy3 [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0.14
     :output-cost 0.58
     :request-params (:model "hy3" :reasoning_effort "high"))
    (hy3-preview
     :description "hy3-preview"
     :request-params (:model "hy3-preview"))
    (hy4-preview
     :description "Hy4 preview"
     :capabilities (tool-use reasoning cache)
     :context-window 1024
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "hy4-preview"))
    (hy4-preview-effort-none
     :description "Hy4 preview [effort none]"
     :capabilities (tool-use reasoning cache)
     :context-window 1024
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "hy4-preview" :reasoning_effort "none"))
    (hy4-preview-effort-high
     :description "Hy4 preview [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1024
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "hy4-preview" :reasoning_effort "high"))
    (kimi-k2.5
     :description "Kimi K2.5"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.6
     :output-cost 3
     :cutoff-date "2024-10"
     :request-params (:model "kimi-k2.5"))
    (kimi-k2.6
     :description "Kimi K2.6"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.95
     :output-cost 4
     :cutoff-date "2024-10"
     :request-params (:model "kimi-k2.6"))
    (kimi-k2.7-code
     :description "Kimi K2.7 Code"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.95
     :output-cost 4
     :cutoff-date "2025-01"
     :request-params (:model "kimi-k2.7-code"))
    (kimi-k3
     :description "Kimi K3"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 3
     :output-cost 15
     :request-params (:model "kimi-k3"))
    (longcat-2.0
     :description "LongCat-2.0"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "longcat-2.0"))
    (mimo-v2-omni
     :description "MiMo V2 Omni"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif" "application/pdf")
     :context-window 262
     :input-cost 0.4
     :output-cost 2
     :cutoff-date "2024-12"
     :request-params (:model "mimo-v2-omni"))
    (mimo-v2-pro
     :description "MiMo V2 Pro"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 1
     :output-cost 3
     :cutoff-date "2024-12"
     :request-params (:model "mimo-v2-pro"))
    (mimo-v2.5
     :description "MiMo V2.5"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.14
     :output-cost 0.28
     :cutoff-date "2024-12"
     :request-params (:model "mimo-v2.5"))
    (mimo-v2.5-pro
     :description "MiMo V2.5 Pro"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0.435
     :output-cost 0.87
     :cutoff-date "2024-12"
     :request-params (:model "mimo-v2.5-pro"))
    (omen-alpha
     :description "Omen Alpha"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 0.2
     :output-cost 0.66
     :request-params (:model "omen-alpha"))
    (omen-alpha-effort-low
     :description "Omen Alpha [effort low]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 0.2
     :output-cost 0.66
     :request-params (:model "omen-alpha" :reasoning_effort "low"))
    (omen-alpha-effort-high
     :description "Omen Alpha [effort high]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 0.2
     :output-cost 0.66
     :request-params (:model "omen-alpha" :reasoning_effort "high"))))

(gptel-make-anthropic "OpenCode Go (Anthropic)"
  :host "opencode.ai"
  :endpoint "/zen/go/v1/messages"
  :protocol "https"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models
  '(
    (minimax-m2.5
     :description "MiniMax-M2.5"
     :capabilities (tool-use reasoning cache)
     :context-window 205
     :input-cost 0.3
     :output-cost 1.2
     :cutoff-date "2025-01"
     :request-params (:model "minimax-m2.5"))
    (minimax-m2.7
     :description "MiniMax-M2.7"
     :capabilities (tool-use reasoning cache)
     :context-window 205
     :input-cost 0.3
     :output-cost 1.2
     :cutoff-date "2025-01"
     :request-params (:model "minimax-m2.7"))
    (minimax-m3
     :description "MiniMax-M3"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.3
     :output-cost 1.2
     :cutoff-date "2025-01"
     :request-params (:model "minimax-m3"))
    (qwen3.5-plus
     :description "Qwen3.5 Plus"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.2
     :output-cost 1.2
     :cutoff-date "2025-04"
     :request-params (:model "qwen3.5-plus"))
    (qwen3.6-plus
     :description "Qwen3.6 Plus"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.5
     :output-cost 3
     :cutoff-date "2025-04"
     :request-params (:model "qwen3.6-plus"))
    (qwen3.7-max
     :description "Qwen3.7 Max"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 2.5
     :output-cost 7.5
     :request-params (:model "qwen3.7-max"))
    (qwen3.7-plus
     :description "Qwen3.7 Plus"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.4
     :output-cost 1.6
     :request-params (:model "qwen3.7-plus"))
    (qwen3.8-flash
     :description "Qwen3.8 Flash"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "qwen3.8-flash"))
    (qwen3.8-flash-effort-low
     :description "Qwen3.8 Flash [effort low]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "qwen3.8-flash" :output_config (:effort "low")))
    (qwen3.8-flash-effort-medium
     :description "Qwen3.8 Flash [effort medium]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "qwen3.8-flash" :output_config (:effort "medium")))
    (qwen3.8-flash-effort-xhigh
     :description "Qwen3.8 Flash [effort xhigh]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "qwen3.8-flash" :output_config (:effort "xhigh")))
    (qwen3.8-max
     :description "Qwen3.8 Max"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "qwen3.8-max"))
    (qwen3.8-max-effort-low
     :description "Qwen3.8 Max [effort low]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "qwen3.8-max" :output_config (:effort "low")))
    (qwen3.8-max-effort-medium
     :description "Qwen3.8 Max [effort medium]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "qwen3.8-max" :output_config (:effort "medium")))
    (qwen3.8-max-effort-xhigh
     :description "Qwen3.8 Max [effort xhigh]"
     :capabilities (tool-use reasoning json cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "qwen3.8-max" :output_config (:effort "xhigh")))))

;; ---- Not emitted: responses protocol, probe declined ----
;;   gpt-5.6-luna
;;   grok-4.5
;;   grok-4.6
;;   muse-spark-1.2-contributor
;;   muse-spark-1.3-contributor

;; ---- Probe failures ----
;;   gpt-5.6-luna [openai]        {"type":"error","error":{"type":"error","message":"Internal server error"}}
;;   gpt-5.6-luna [anthropic]     {"type":"error","error":{"type":"error","message":"Internal server error"}}
;;   grok-4.5 [openai]            {"type":"error","error":{"type":"MissingSessionID","message":"Error from provider (Console Go): Request is missing x-opencode-session and cannot be routed efficiently. Please see https://opencode.ai/docs/go/#where-can-i-use-it"}}
;;   grok-4.5 [anthropic]         {"type":"error","error":{"type":"ModelError","message":"Model grok-4.5 is not supported for format anthropic"}}
;;   grok-4.6 [openai]            {"type":"error","error":{"type":"ModelError","message":"Model grok-4.6 is not supported for format oa-compat"}}
;;   grok-4.6 [anthropic]         {"type":"error","error":{"type":"ModelError","message":"Model grok-4.6 is not supported for format anthropic"}}
;;   muse-spark-1.2-contributor [openai] {"type":"error","error":{"type":"DataPolicyError","message":"This model collects data used to improve its quality and requires explicit opt in: https://opencode.ai/workspace/wrk_01KX6DV7M3E7NYBG6P6SKT0JNQ/go"}}
;;   muse-spark-1.2-contributor [anthropic] {"type":"error","error":{"type":"DataPolicyError","message":"This model collects data used to improve its quality and requires explicit opt in: https://opencode.ai/workspace/wrk_01KX6DV7M3E7NYBG6P6SKT0JNQ/go"}}
;;   muse-spark-1.3-contributor [openai] {"type":"error","error":{"type":"DataPolicyError","message":"This model collects data used to improve its quality and requires explicit opt in: https://opencode.ai/workspace/wrk_01KX6DV7M3E7NYBG6P6SKT0JNQ/go"}}
;;   muse-spark-1.3-contributor [anthropic] {"type":"error","error":{"type":"DataPolicyError","message":"This model collects data used to improve its quality and requires explicit opt in: https://opencode.ai/workspace/wrk_01KX6DV7M3E7NYBG6P6SKT0JNQ/go"}}
;;
;; ACTION: at least one model needs an explicit data-policy
;; opt-in in your workspace console (see the URL above).
;; Opt in, then re-run to pick it up.
;;
;; NOTE: "not supported for format oa-compat" is the gateway
;; enforcing protocol per model.  No client config can work
;; around it; that model is Responses-only.

;; ---- Reachable but absent from models.dev ----
;;   deepseek-flash
;;   hy3-preview
;; Emitted bare: no limits, capabilities or effort variants.

;; ---- In models.dev but not reachable with your key ----
;;   ox-alpha-free
