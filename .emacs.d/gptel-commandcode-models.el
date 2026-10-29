;;; gptel backends for Command Code -- generated 2026-10-29 09:12  -*- lexical-binding: t; -*-
;;; plan: individual-goat (from my/cmdc-plan)
;;; probe mode: off
;;; 68 models: 66 openai-compatible, 2 anthropic, 0 unavailable
;;;
;;; Effort variants override :model via :request-params, which gptel
;;; merges last, so they beat both the symbol name and any preset.
;;; Effort values skipped by config: ("off")

(gptel-make-openai "Command Code"
  :host "api.commandcode.ai"
  :endpoint "/provider/v1/chat/completions"
  :protocol "https"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models
  '(
    (deepseek/deepseek-v4-flash
     :description "DeepSeek V4 Flash (latest)"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4-flash"))
    (deepseek/deepseek-v4-flash-effort-high
     :description "DeepSeek V4 Flash (latest) [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4-flash" :reasoning_effort "high"))
    (deepseek/deepseek-v4-flash-effort-max
     :description "DeepSeek V4 Flash (latest) [effort max]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4-flash" :reasoning_effort "max"))
    (deepseek/deepseek-v4-flash-fast
     :description "DeepSeek V4 Flash Fast"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.28
     :output-cost 0.56
     :request-params (:model "deepseek/deepseek-v4-flash-fast"))
    (deepseek/deepseek-v4-flash-fast-effort-low
     :description "DeepSeek V4 Flash Fast [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.28
     :output-cost 0.56
     :request-params (:model "deepseek/deepseek-v4-flash-fast" :reasoning_effort "low"))
    (deepseek/deepseek-v4-flash-fast-effort-high
     :description "DeepSeek V4 Flash Fast [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.28
     :output-cost 0.56
     :request-params (:model "deepseek/deepseek-v4-flash-fast" :reasoning_effort "high"))
    (deepseek/deepseek-v4-flash-fast-effort-max
     :description "DeepSeek V4 Flash Fast [effort max]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.28
     :output-cost 0.56
     :request-params (:model "deepseek/deepseek-v4-flash-fast" :reasoning_effort "max"))
    (deepseek/deepseek-v4-flash-vision-exp
     :description "DeepSeek V4 Flash Vision (exp)"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4-flash-vision-exp"))
    (deepseek/deepseek-v4-flash-vision-exp-effort-high
     :description "DeepSeek V4 Flash Vision (exp) [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4-flash-vision-exp" :reasoning_effort "high"))
    (deepseek/deepseek-v4-flash-vision-exp-effort-max
     :description "DeepSeek V4 Flash Vision (exp) [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4-flash-vision-exp" :reasoning_effort "max"))
    (deepseek/deepseek-v4-pro
     :description "DeepSeek V4 Pro (latest)"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.66
     :output-cost 1.98
     :request-params (:model "deepseek/deepseek-v4-pro"))
    (deepseek/deepseek-v4-pro-effort-high
     :description "DeepSeek V4 Pro (latest) [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.66
     :output-cost 1.98
     :request-params (:model "deepseek/deepseek-v4-pro" :reasoning_effort "high"))
    (deepseek/deepseek-v4-pro-effort-max
     :description "DeepSeek V4 Pro (latest) [effort max]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 0.66
     :output-cost 1.98
     :request-params (:model "deepseek/deepseek-v4-pro" :reasoning_effort "max"))
    (deepseek/deepseek-v4.1-flash
     :description "DeepSeek V4.1 Flash (latest)"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4.1-flash"))
    (deepseek/deepseek-v4.1-flash-effort-low
     :description "DeepSeek V4.1 Flash (latest) [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4.1-flash" :reasoning_effort "low"))
    (deepseek/deepseek-v4.1-flash-effort-high
     :description "DeepSeek V4.1 Flash (latest) [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4.1-flash" :reasoning_effort "high"))
    (deepseek/deepseek-v4.1-flash-effort-max
     :description "DeepSeek V4.1 Flash (latest) [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "deepseek/deepseek-v4.1-flash" :reasoning_effort "max"))
    (deepseek/deepseek-v4.1-flash-fast
     :description "DeepSeek V4.1 Flash Fast (latest)"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.58
     :request-params (:model "deepseek/deepseek-v4.1-flash-fast"))
    (deepseek/deepseek-v4.1-flash-fast-effort-low
     :description "DeepSeek V4.1 Flash Fast (latest) [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.58
     :request-params (:model "deepseek/deepseek-v4.1-flash-fast" :reasoning_effort "low"))
    (deepseek/deepseek-v4.1-flash-fast-effort-high
     :description "DeepSeek V4.1 Flash Fast (latest) [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.58
     :request-params (:model "deepseek/deepseek-v4.1-flash-fast" :reasoning_effort "high"))
    (deepseek/deepseek-v4.1-flash-fast-effort-max
     :description "DeepSeek V4.1 Flash Fast (latest) [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.58
     :request-params (:model "deepseek/deepseek-v4.1-flash-fast" :reasoning_effort "max"))
    (google/gemini-3.7-flash
     :description "Gemini 3.7 Flash"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.7-flash"))
    (google/gemini-3.7-flash-effort-low
     :description "Gemini 3.7 Flash [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.7-flash" :reasoning_effort "low"))
    (google/gemini-3.7-flash-effort-medium
     :description "Gemini 3.7 Flash [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.7-flash" :reasoning_effort "medium"))
    (google/gemini-3.7-flash-effort-high
     :description "Gemini 3.7 Flash [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.7-flash" :reasoning_effort "high"))
    (google/gemini-3.8-flash
     :description "Gemini 3.8 Flash"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.8-flash"))
    (google/gemini-3.8-flash-effort-low
     :description "Gemini 3.8 Flash [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.8-flash" :reasoning_effort "low"))
    (google/gemini-3.8-flash-effort-medium
     :description "Gemini 3.8 Flash [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.8-flash" :reasoning_effort "medium"))
    (google/gemini-3.8-flash-effort-high
     :description "Gemini 3.8 Flash [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1.5
     :output-cost 7.5
     :request-params (:model "google/gemini-3.8-flash" :reasoning_effort "high"))
    (gpt-5.6-luna
     :description "GPT-5.6 Luna"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.2
     :output-cost 1.2
     :request-params (:model "gpt-5.6-luna"))
    (gpt-5.6-luna-effort-low
     :description "GPT-5.6 Luna [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.2
     :output-cost 1.2
     :request-params (:model "gpt-5.6-luna" :reasoning_effort "low"))
    (gpt-5.6-luna-effort-medium
     :description "GPT-5.6 Luna [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.2
     :output-cost 1.2
     :request-params (:model "gpt-5.6-luna" :reasoning_effort "medium"))
    (gpt-5.6-luna-effort-high
     :description "GPT-5.6 Luna [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.2
     :output-cost 1.2
     :request-params (:model "gpt-5.6-luna" :reasoning_effort "high"))
    (gpt-5.6-luna-effort-xhigh
     :description "GPT-5.6 Luna [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.2
     :output-cost 1.2
     :request-params (:model "gpt-5.6-luna" :reasoning_effort "xhigh"))
    (gpt-5.6-luna-effort-max
     :description "GPT-5.6 Luna [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.2
     :output-cost 1.2
     :request-params (:model "gpt-5.6-luna" :reasoning_effort "max"))
    (gpt-5.6-sol
     :description "GPT-5.6 Sol"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :request-params (:model "gpt-5.6-sol"))
    (gpt-5.6-sol-effort-low
     :description "GPT-5.6 Sol [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :request-params (:model "gpt-5.6-sol" :reasoning_effort "low"))
    (gpt-5.6-sol-effort-medium
     :description "GPT-5.6 Sol [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :request-params (:model "gpt-5.6-sol" :reasoning_effort "medium"))
    (gpt-5.6-sol-effort-high
     :description "GPT-5.6 Sol [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :request-params (:model "gpt-5.6-sol" :reasoning_effort "high"))
    (gpt-5.6-sol-effort-xhigh
     :description "GPT-5.6 Sol [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :request-params (:model "gpt-5.6-sol" :reasoning_effort "xhigh"))
    (gpt-5.6-sol-effort-max
     :description "GPT-5.6 Sol [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :request-params (:model "gpt-5.6-sol" :reasoning_effort "max"))
    (gpt-6-luna
     :description "GPT-6 Luna"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "gpt-6-luna"))
    (gpt-6-luna-effort-low
     :description "GPT-6 Luna [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "gpt-6-luna" :reasoning_effort "low"))
    (gpt-6-luna-effort-medium
     :description "GPT-6 Luna [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "gpt-6-luna" :reasoning_effort "medium"))
    (gpt-6-luna-effort-high
     :description "GPT-6 Luna [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "gpt-6-luna" :reasoning_effort "high"))
    (gpt-6-luna-effort-xhigh
     :description "GPT-6 Luna [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "gpt-6-luna" :reasoning_effort "xhigh"))
    (gpt-6-luna-effort-max
     :description "GPT-6 Luna [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1050
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "gpt-6-luna" :reasoning_effort "max"))
    (inclusionai/ling-3.1-flash
     :description "Ling 3.1 Flash"
     :capabilities (tool-use reasoning cache)
     :context-window 262
     :input-cost 0.3
     :output-cost 0.9
     :request-params (:model "inclusionai/ling-3.1-flash"))
    (inclusionai/ling-3.1-flash-effort-low
     :description "Ling 3.1 Flash [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 262
     :input-cost 0.3
     :output-cost 0.9
     :request-params (:model "inclusionai/ling-3.1-flash" :reasoning_effort "low"))
    (inclusionai/ling-3.1-flash-effort-medium
     :description "Ling 3.1 Flash [effort medium]"
     :capabilities (tool-use reasoning cache)
     :context-window 262
     :input-cost 0.3
     :output-cost 0.9
     :request-params (:model "inclusionai/ling-3.1-flash" :reasoning_effort "medium"))
    (inclusionai/ling-3.1-flash-effort-high
     :description "Ling 3.1 Flash [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 262
     :input-cost 0.3
     :output-cost 0.9
     :request-params (:model "inclusionai/ling-3.1-flash" :reasoning_effort "high"))
    (meituan/LongCat-2.0
     :description "LongCat 2.0"
     :capabilities (tool-use cache)
     :context-window 1049
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "meituan/LongCat-2.0"))
    (meta/muse-spark-1.2
     :description "Muse Spark 1.2"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.2"))
    (meta/muse-spark-1.2-effort-low
     :description "Muse Spark 1.2 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.2" :reasoning_effort "low"))
    (meta/muse-spark-1.2-effort-medium
     :description "Muse Spark 1.2 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.2" :reasoning_effort "medium"))
    (meta/muse-spark-1.2-effort-high
     :description "Muse Spark 1.2 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.2" :reasoning_effort "high"))
    (meta/muse-spark-1.2-effort-xhigh
     :description "Muse Spark 1.2 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.2" :reasoning_effort "xhigh"))
    (meta/muse-spark-1.2-contributor
     :description "Muse Spark 1.2 Contributor"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.2-contributor"))
    (meta/muse-spark-1.2-contributor-effort-low
     :description "Muse Spark 1.2 Contributor [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.2-contributor" :reasoning_effort "low"))
    (meta/muse-spark-1.2-contributor-effort-medium
     :description "Muse Spark 1.2 Contributor [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.2-contributor" :reasoning_effort "medium"))
    (meta/muse-spark-1.2-contributor-effort-high
     :description "Muse Spark 1.2 Contributor [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.2-contributor" :reasoning_effort "high"))
    (meta/muse-spark-1.2-contributor-effort-xhigh
     :description "Muse Spark 1.2 Contributor [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.2-contributor" :reasoning_effort "xhigh"))
    (meta/muse-spark-1.3
     :description "Muse Spark 1.3"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.3"))
    (meta/muse-spark-1.3-effort-low
     :description "Muse Spark 1.3 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.3" :reasoning_effort "low"))
    (meta/muse-spark-1.3-effort-medium
     :description "Muse Spark 1.3 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.3" :reasoning_effort "medium"))
    (meta/muse-spark-1.3-effort-high
     :description "Muse Spark 1.3 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.3" :reasoning_effort "high"))
    (meta/muse-spark-1.3-effort-xhigh
     :description "Muse Spark 1.3 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.3" :reasoning_effort "xhigh"))
    (meta/muse-spark-1.3-effort-max
     :description "Muse Spark 1.3 [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-1.3" :reasoning_effort "max"))
    (meta/muse-spark-1.3-contributor
     :description "Muse Spark 1.3 Contributor"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.3-contributor"))
    (meta/muse-spark-1.3-contributor-effort-low
     :description "Muse Spark 1.3 Contributor [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.3-contributor" :reasoning_effort "low"))
    (meta/muse-spark-1.3-contributor-effort-medium
     :description "Muse Spark 1.3 Contributor [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.3-contributor" :reasoning_effort "medium"))
    (meta/muse-spark-1.3-contributor-effort-high
     :description "Muse Spark 1.3 Contributor [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.3-contributor" :reasoning_effort "high"))
    (meta/muse-spark-1.3-contributor-effort-xhigh
     :description "Muse Spark 1.3 Contributor [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.1
     :output-cost 0.2
     :request-params (:model "meta/muse-spark-1.3-contributor" :reasoning_effort "xhigh"))
    (meta/muse-spark-open
     :description "Muse Spark Open"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-open"))
    (meta/muse-spark-open-effort-low
     :description "Muse Spark Open [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-open" :reasoning_effort "low"))
    (meta/muse-spark-open-effort-medium
     :description "Muse Spark Open [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-open" :reasoning_effort "medium"))
    (meta/muse-spark-open-effort-high
     :description "Muse Spark Open [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 1.25
     :output-cost 4.25
     :request-params (:model "meta/muse-spark-open" :reasoning_effort "high"))
    (minimax/minimax-m3.1-flash
     :description "MiniMax M3.1 Flash"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "minimax/minimax-m3.1-flash"))
    (minimax/minimax-m3.1-flash-effort-low
     :description "MiniMax M3.1 Flash [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "minimax/minimax-m3.1-flash" :reasoning_effort "low"))
    (minimax/minimax-m3.1-flash-effort-medium
     :description "MiniMax M3.1 Flash [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "minimax/minimax-m3.1-flash" :reasoning_effort "medium"))
    (minimax/minimax-m3.1-flash-effort-high
     :description "MiniMax M3.1 Flash [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "minimax/minimax-m3.1-flash" :reasoning_effort "high"))
    (minimax/minimax-m3.1-flash-effort-xhigh
     :description "MiniMax M3.1 Flash [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "minimax/minimax-m3.1-flash" :reasoning_effort "xhigh"))
    (minimax/minimax-m3.1-flash-effort-max
     :description "MiniMax M3.1 Flash [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.6
     :request-params (:model "minimax/minimax-m3.1-flash" :reasoning_effort "max"))
    (MiniMaxAI/MiniMax-M2.5
     :description "MiniMax M2.5"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "MiniMaxAI/MiniMax-M2.5"))
    (MiniMaxAI/MiniMax-M2.7
     :description "MiniMax M2.7"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "MiniMaxAI/MiniMax-M2.7"))
    (MiniMaxAI/MiniMax-M3
     :description "MiniMax M3"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "MiniMaxAI/MiniMax-M3"))
    (MiniMaxAI/MiniMax-M3-effort-low
     :description "MiniMax M3 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "MiniMaxAI/MiniMax-M3" :reasoning_effort "low"))
    (MiniMaxAI/MiniMax-M3-effort-medium
     :description "MiniMax M3 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "MiniMaxAI/MiniMax-M3" :reasoning_effort "medium"))
    (MiniMaxAI/MiniMax-M3-effort-high
     :description "MiniMax M3 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.3
     :output-cost 1.2
     :request-params (:model "MiniMaxAI/MiniMax-M3" :reasoning_effort "high"))
    (mistral/mistral-large-4
     :description "Mistral Large 4"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 524
     :input-cost 1.36
     :output-cost 4.18
     :request-params (:model "mistral/mistral-large-4"))
    (mistral/mistral-large-4-effort-low
     :description "Mistral Large 4 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 524
     :input-cost 1.36
     :output-cost 4.18
     :request-params (:model "mistral/mistral-large-4" :reasoning_effort "low"))
    (mistral/mistral-large-4-effort-medium
     :description "Mistral Large 4 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 524
     :input-cost 1.36
     :output-cost 4.18
     :request-params (:model "mistral/mistral-large-4" :reasoning_effort "medium"))
    (mistral/mistral-large-4-effort-high
     :description "Mistral Large 4 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 524
     :input-cost 1.36
     :output-cost 4.18
     :request-params (:model "mistral/mistral-large-4" :reasoning_effort "high"))
    (moonshotai/Kimi-K2.5
     :description "Kimi K2.5"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 256
     :input-cost 0.6
     :output-cost 3
     :request-params (:model "moonshotai/Kimi-K2.5"))
    (moonshotai/Kimi-K2.6
     :description "Kimi K2.6"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 256
     :input-cost 0.95
     :output-cost 4
     :request-params (:model "moonshotai/Kimi-K2.6"))
    (moonshotai/Kimi-K2.7-Code
     :description "Kimi K2.7 Code"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 256
     :input-cost 0.95
     :output-cost 4
     :request-params (:model "moonshotai/Kimi-K2.7-Code"))
    (moonshotai/Kimi-K2.7-Code-Highspeed
     :description "Kimi K2.7 Code HighSpeed"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 1.9
     :output-cost 8
     :request-params (:model "moonshotai/Kimi-K2.7-Code-Highspeed"))
    (moonshotai/Kimi-K3
     :description "Kimi K3"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :request-params (:model "moonshotai/Kimi-K3"))
    (moonshotai/Kimi-K3-effort-low
     :description "Kimi K3 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :request-params (:model "moonshotai/Kimi-K3" :reasoning_effort "low"))
    (moonshotai/Kimi-K3-effort-high
     :description "Kimi K3 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :request-params (:model "moonshotai/Kimi-K3" :reasoning_effort "high"))
    (moonshotai/Kimi-K3-effort-max
     :description "Kimi K3 [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 3
     :output-cost 15
     :request-params (:model "moonshotai/Kimi-K3" :reasoning_effort "max"))
    (moonshotai/Kimi-K3.1-Pro
     :description "Kimi K3.1 Pro"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 2
     :output-cost 10
     :request-params (:model "moonshotai/Kimi-K3.1-Pro"))
    (moonshotai/Kimi-K3.1-Pro-effort-low
     :description "Kimi K3.1 Pro [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 2
     :output-cost 10
     :request-params (:model "moonshotai/Kimi-K3.1-Pro" :reasoning_effort "low"))
    (moonshotai/Kimi-K3.1-Pro-effort-high
     :description "Kimi K3.1 Pro [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 2
     :output-cost 10
     :request-params (:model "moonshotai/Kimi-K3.1-Pro" :reasoning_effort "high"))
    (moonshotai/Kimi-K3.1-Pro-effort-max
     :description "Kimi K3.1 Pro [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 2
     :output-cost 10
     :request-params (:model "moonshotai/Kimi-K3.1-Pro" :reasoning_effort "max"))
    (nvidia/nemotron-3-ultra-550b-a55b
     :description "Nemotron 3 Ultra"
     :capabilities (tool-use cache)
     :context-window 1000
     :input-cost 0.6
     :output-cost 2.4
     :request-params (:model "nvidia/nemotron-3-ultra-550b-a55b"))
    (nvidia/nemotron-3.5-super-vl
     :description "Nemotron 3.5 Super VL"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.2
     :output-cost 0.5
     :request-params (:model "nvidia/nemotron-3.5-super-vl"))
    (nvidia/nemotron-3.5-super-vl-effort-low
     :description "Nemotron 3.5 Super VL [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.2
     :output-cost 0.5
     :request-params (:model "nvidia/nemotron-3.5-super-vl" :reasoning_effort "low"))
    (nvidia/nemotron-3.5-super-vl-effort-medium
     :description "Nemotron 3.5 Super VL [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.2
     :output-cost 0.5
     :request-params (:model "nvidia/nemotron-3.5-super-vl" :reasoning_effort "medium"))
    (nvidia/nemotron-3.5-super-vl-effort-high
     :description "Nemotron 3.5 Super VL [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.2
     :output-cost 0.5
     :request-params (:model "nvidia/nemotron-3.5-super-vl" :reasoning_effort "high"))
    (ornith/ornith-1.5-122b
     :description "Ornith 1.5 122B"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.16
     :output-cost 0.64
     :request-params (:model "ornith/ornith-1.5-122b"))
    (poolside/laguna-s-2.1-free
     :description "Laguna S 2.1"
     :capabilities (tool-use cache)
     :context-window 256
     :input-cost 0
     :output-cost 0
     :request-params (:model "poolside/laguna-s-2.1-free"))
    (Qwen/Qwen3.6-Max-Preview
     :description "Qwen 3.6 Max Preview"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 1.3
     :output-cost 7.8
     :request-params (:model "Qwen/Qwen3.6-Max-Preview"))
    (Qwen/Qwen3.6-Plus
     :description "Qwen 3.6 Plus"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 200
     :input-cost 0.5
     :output-cost 3
     :request-params (:model "Qwen/Qwen3.6-Plus"))
    (Qwen/Qwen3.7-Flash
     :description "Qwen 3.7 Flash"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.03
     :output-cost 0.13
     :request-params (:model "Qwen/Qwen3.7-Flash"))
    (Qwen/Qwen3.7-Max
     :description "Qwen 3.7 Max"
     :capabilities (tool-use cache)
     :context-window 1000
     :input-cost 2.5
     :output-cost 7.5
     :request-params (:model "Qwen/Qwen3.7-Max"))
    (Qwen/Qwen3.7-Plus
     :description "Qwen 3.7 Plus"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.4
     :output-cost 1.6
     :request-params (:model "Qwen/Qwen3.7-Plus"))
    (Qwen/Qwen3.8-27B
     :description "Qwen 3.8 27B"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.4
     :output-cost 3
     :request-params (:model "Qwen/Qwen3.8-27B"))
    (Qwen/Qwen3.8-27B-effort-low
     :description "Qwen 3.8 27B [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.4
     :output-cost 3
     :request-params (:model "Qwen/Qwen3.8-27B" :reasoning_effort "low"))
    (Qwen/Qwen3.8-27B-effort-medium
     :description "Qwen 3.8 27B [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.4
     :output-cost 3
     :request-params (:model "Qwen/Qwen3.8-27B" :reasoning_effort "medium"))
    (Qwen/Qwen3.8-27B-effort-xhigh
     :description "Qwen 3.8 27B [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 262
     :input-cost 0.4
     :output-cost 3
     :request-params (:model "Qwen/Qwen3.8-27B" :reasoning_effort "xhigh"))
    (Qwen/Qwen3.8-Flash
     :description "Qwen 3.8 Flash"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Flash"))
    (Qwen/Qwen3.8-Flash-effort-low
     :description "Qwen 3.8 Flash [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Flash" :reasoning_effort "low"))
    (Qwen/Qwen3.8-Flash-effort-medium
     :description "Qwen 3.8 Flash [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Flash" :reasoning_effort "medium"))
    (Qwen/Qwen3.8-Flash-effort-xhigh
     :description "Qwen 3.8 Flash [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.16
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Flash" :reasoning_effort "xhigh"))
    (Qwen/Qwen3.8-Max
     :description "Qwen 3.8 Max"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max"))
    (Qwen/Qwen3.8-Max-effort-low
     :description "Qwen 3.8 Max [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max" :reasoning_effort "low"))
    (Qwen/Qwen3.8-Max-effort-medium
     :description "Qwen 3.8 Max [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max" :reasoning_effort "medium"))
    (Qwen/Qwen3.8-Max-effort-xhigh
     :description "Qwen 3.8 Max [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max" :reasoning_effort "xhigh"))
    (Qwen/Qwen3.8-Max-0902
     :description "Qwen 3.8 Max 0902"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max-0902"))
    (Qwen/Qwen3.8-Max-0902-effort-low
     :description "Qwen 3.8 Max 0902 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max-0902" :reasoning_effort "low"))
    (Qwen/Qwen3.8-Max-0902-effort-medium
     :description "Qwen 3.8 Max 0902 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max-0902" :reasoning_effort "medium"))
    (Qwen/Qwen3.8-Max-0902-effort-xhigh
     :description "Qwen 3.8 Max 0902 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 6
     :request-params (:model "Qwen/Qwen3.8-Max-0902" :reasoning_effort "xhigh"))
    (Qwen/Qwen3.8-Max-Prime
     :description "Qwen 3.8 Max Prime"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 4
     :output-cost 12
     :request-params (:model "Qwen/Qwen3.8-Max-Prime"))
    (Qwen/Qwen3.8-Max-Prime-effort-low
     :description "Qwen 3.8 Max Prime [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 4
     :output-cost 12
     :request-params (:model "Qwen/Qwen3.8-Max-Prime" :reasoning_effort "low"))
    (Qwen/Qwen3.8-Max-Prime-effort-medium
     :description "Qwen 3.8 Max Prime [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 4
     :output-cost 12
     :request-params (:model "Qwen/Qwen3.8-Max-Prime" :reasoning_effort "medium"))
    (Qwen/Qwen3.8-Max-Prime-effort-xhigh
     :description "Qwen 3.8 Max Prime [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 4
     :output-cost 12
     :request-params (:model "Qwen/Qwen3.8-Max-Prime" :reasoning_effort "xhigh"))
    (Qwen/Qwen3.8-Omni-Flash
     :description "Qwen 3.8 Omni Flash"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Omni-Flash"))
    (Qwen/Qwen3.8-Omni-Flash-effort-low
     :description "Qwen 3.8 Omni Flash [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Omni-Flash" :reasoning_effort "low"))
    (Qwen/Qwen3.8-Omni-Flash-effort-medium
     :description "Qwen 3.8 Omni Flash [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Omni-Flash" :reasoning_effort "medium"))
    (Qwen/Qwen3.8-Omni-Flash-effort-xhigh
     :description "Qwen 3.8 Omni Flash [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.15
     :output-cost 0.47
     :request-params (:model "Qwen/Qwen3.8-Omni-Flash" :reasoning_effort "xhigh"))
    (reflection/beam:free
     :description "Beam 1.0"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0
     :output-cost 0
     :request-params (:model "reflection/beam:free"))
    (reflection/beam:free-effort-low
     :description "Beam 1.0 [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0
     :output-cost 0
     :request-params (:model "reflection/beam:free" :reasoning_effort "low"))
    (reflection/beam:free-effort-medium
     :description "Beam 1.0 [effort medium]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0
     :output-cost 0
     :request-params (:model "reflection/beam:free" :reasoning_effort "medium"))
    (reflection/beam:free-effort-high
     :description "Beam 1.0 [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0
     :output-cost 0
     :request-params (:model "reflection/beam:free" :reasoning_effort "high"))
    (reflection/beam:free-effort-xhigh
     :description "Beam 1.0 [effort xhigh]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0
     :output-cost 0
     :request-params (:model "reflection/beam:free" :reasoning_effort "xhigh"))
    (reflection/beam:free-effort-max
     :description "Beam 1.0 [effort max]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0
     :output-cost 0
     :request-params (:model "reflection/beam:free" :reasoning_effort "max"))
    (stealth/glyph-cluster:free
     :description "Glyph Cluster"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0
     :output-cost 0
     :request-params (:model "stealth/glyph-cluster:free"))
    (stealth/glyph-cluster:free-effort-low
     :description "Glyph Cluster [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0
     :output-cost 0
     :request-params (:model "stealth/glyph-cluster:free" :reasoning_effort "low"))
    (stealth/glyph-cluster:free-effort-medium
     :description "Glyph Cluster [effort medium]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0
     :output-cost 0
     :request-params (:model "stealth/glyph-cluster:free" :reasoning_effort "medium"))
    (stealth/glyph-cluster:free-effort-high
     :description "Glyph Cluster [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0
     :output-cost 0
     :request-params (:model "stealth/glyph-cluster:free" :reasoning_effort "high"))
    (stealth/glyph-cluster:free-effort-xhigh
     :description "Glyph Cluster [effort xhigh]"
     :capabilities (tool-use reasoning cache)
     :context-window 256
     :input-cost 0
     :output-cost 0
     :request-params (:model "stealth/glyph-cluster:free" :reasoning_effort "xhigh"))
    (stepfun/Step-3.5-Flash
     :description "Step 3.5 Flash"
     :capabilities (tool-use cache)
     :context-window 262
     :input-cost 0.09
     :output-cost 0.3
     :request-params (:model "stepfun/Step-3.5-Flash"))
    (stepfun/Step-3.7-Flash
     :description "Step 3.7 Flash"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 256
     :input-cost 0.2
     :output-cost 1.15
     :request-params (:model "stepfun/Step-3.7-Flash"))
    (stepfun/Step-5-Preview
     :description "Step 5 Preview"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1
     :output-cost 2.7
     :request-params (:model "stepfun/Step-5-Preview"))
    (stepfun/Step-5-Preview-effort-low
     :description "Step 5 Preview [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1
     :output-cost 2.7
     :request-params (:model "stepfun/Step-5-Preview" :reasoning_effort "low"))
    (stepfun/Step-5-Preview-effort-medium
     :description "Step 5 Preview [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1
     :output-cost 2.7
     :request-params (:model "stepfun/Step-5-Preview" :reasoning_effort "medium"))
    (stepfun/Step-5-Preview-effort-high
     :description "Step 5 Preview [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 1
     :output-cost 2.7
     :request-params (:model "stepfun/Step-5-Preview" :reasoning_effort "high"))
    (tencent/hy3-paid
     :description "Tencent Hy3"
     :capabilities (tool-use cache)
     :context-window 262
     :input-cost 0.14
     :output-cost 0.58
     :request-params (:model "tencent/hy3-paid"))
    (tencent/hy4-preview
     :description "Tencent Hy4 Preview"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "tencent/hy4-preview"))
    (tencent/hy4-preview-effort-low
     :description "Tencent Hy4 Preview [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "tencent/hy4-preview" :reasoning_effort "low"))
    (tencent/hy4-preview-effort-medium
     :description "Tencent Hy4 Preview [effort medium]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "tencent/hy4-preview" :reasoning_effort "medium"))
    (tencent/hy4-preview-effort-high
     :description "Tencent Hy4 Preview [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1049
     :input-cost 0.834
     :output-cost 2.501
     :request-params (:model "tencent/hy4-preview" :reasoning_effort "high"))
    (thinkingmachines/inkling
     :description "Inkling"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 256
     :input-cost 1
     :output-cost 4.05
     :request-params (:model "thinkingmachines/inkling"))
    (thinkingmachines/inkling-small
     :description "Inkling Small"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.5
     :output-cost 1.2
     :request-params (:model "thinkingmachines/inkling-small"))
    (xai/grok-4.5
     :description "Grok 4.5"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.5"))
    (xai/grok-4.5-effort-low
     :description "Grok 4.5 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.5" :reasoning_effort "low"))
    (xai/grok-4.5-effort-medium
     :description "Grok 4.5 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.5" :reasoning_effort "medium"))
    (xai/grok-4.5-effort-high
     :description "Grok 4.5 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.5" :reasoning_effort "high"))
    (xai/grok-4.6
     :description "Grok 4.6"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.6"))
    (xai/grok-4.6-effort-low
     :description "Grok 4.6 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.6" :reasoning_effort "low"))
    (xai/grok-4.6-effort-medium
     :description "Grok 4.6 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.6" :reasoning_effort "medium"))
    (xai/grok-4.6-effort-high
     :description "Grok 4.6 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.6" :reasoning_effort "high"))
    (xai/grok-4.6-effort-xhigh
     :description "Grok 4.6 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.6" :reasoning_effort "xhigh"))
    (xai/grok-4.7
     :description "Grok 4.7"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.7"))
    (xai/grok-4.7-effort-low
     :description "Grok 4.7 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.7" :reasoning_effort "low"))
    (xai/grok-4.7-effort-medium
     :description "Grok 4.7 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.7" :reasoning_effort "medium"))
    (xai/grok-4.7-effort-high
     :description "Grok 4.7 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.7" :reasoning_effort "high"))
    (xai/grok-4.7-effort-xhigh
     :description "Grok 4.7 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 500
     :input-cost 2
     :output-cost 6
     :request-params (:model "xai/grok-4.7" :reasoning_effort "xhigh"))
    (xiaomi/mimo-v2.6-flash
     :description "MiMo V2.6 Flash"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.14
     :output-cost 0.28
     :request-params (:model "xiaomi/mimo-v2.6-flash"))
    (xiaomi/mimo-v2.6-pro
     :description "MiMo V2.6 Pro"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.435
     :output-cost 0.87
     :request-params (:model "xiaomi/mimo-v2.6-pro"))
    (xiaomi/mimo-v2.6-pro-ultraspeed
     :description "MiMo V2.6 Pro UltraSpeed"
     :capabilities (tool-use cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 4.35
     :output-cost 8.7
     :request-params (:model "xiaomi/mimo-v2.6-pro-ultraspeed"))
    (z-ai/glm-5.3-flash
     :description "GLM-5.3 Flash"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "z-ai/glm-5.3-flash"))
    (z-ai/glm-5.3-flash-effort-low
     :description "GLM-5.3 Flash [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "z-ai/glm-5.3-flash" :reasoning_effort "low"))
    (z-ai/glm-5.3-flash-effort-high
     :description "GLM-5.3 Flash [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "z-ai/glm-5.3-flash" :reasoning_effort "high"))
    (z-ai/glm-5.3-flash-effort-max
     :description "GLM-5.3 Flash [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1049
     :input-cost 0.15
     :output-cost 0.5
     :request-params (:model "z-ai/glm-5.3-flash" :reasoning_effort "max"))
    (z-ai/glm-5.3-flashx
     :description "GLM-5.3 FlashX"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.37
     :output-cost 1.25
     :request-params (:model "z-ai/glm-5.3-flashx"))
    (z-ai/glm-5.3-flashx-effort-low
     :description "GLM-5.3 FlashX [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.37
     :output-cost 1.25
     :request-params (:model "z-ai/glm-5.3-flashx" :reasoning_effort "low"))
    (z-ai/glm-5.3-flashx-effort-high
     :description "GLM-5.3 FlashX [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.37
     :output-cost 1.25
     :request-params (:model "z-ai/glm-5.3-flashx" :reasoning_effort "high"))
    (z-ai/glm-5.3-flashx-effort-max
     :description "GLM-5.3 FlashX [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.37
     :output-cost 1.25
     :request-params (:model "z-ai/glm-5.3-flashx" :reasoning_effort "max"))
    (zai-org/GLM-5
     :description "GLM-5"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 1
     :output-cost 3.2
     :request-params (:model "zai-org/GLM-5"))
    (zai-org/GLM-5.1
     :description "GLM-5.1"
     :capabilities (tool-use cache)
     :context-window 200
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.1"))
    (zai-org/GLM-5.2
     :description "GLM-5.2"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.2"))
    (zai-org/GLM-5.2-effort-high
     :description "GLM-5.2 [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.2" :reasoning_effort "high"))
    (zai-org/GLM-5.2-effort-max
     :description "GLM-5.2 [effort max]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.2" :reasoning_effort "max"))
    (zai-org/GLM-5.2-Fast
     :description "GLM-5.2 Fast"
     :capabilities (tool-use cache)
     :context-window 1000
     :input-cost 3
     :output-cost 10.25
     :request-params (:model "zai-org/GLM-5.2-Fast"))
    (zai-org/GLM-5.3
     :description "GLM-5.3"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.3"))
    (zai-org/GLM-5.3-effort-low
     :description "GLM-5.3 [effort low]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.3" :reasoning_effort "low"))
    (zai-org/GLM-5.3-effort-high
     :description "GLM-5.3 [effort high]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.3" :reasoning_effort "high"))
    (zai-org/GLM-5.3-effort-max
     :description "GLM-5.3 [effort max]"
     :capabilities (tool-use reasoning cache)
     :context-window 1000
     :input-cost 1.4
     :output-cost 4.4
     :request-params (:model "zai-org/GLM-5.3" :reasoning_effort "max"))))

(gptel-make-anthropic "Command Code (Anthropic)"
  :host "api.commandcode.ai"
  :endpoint "/provider/v1/messages"
  :protocol "https"
  :stream t
  :key #'gptel-api-key-from-auth-source
  :models
  '(
    (claude-haiku-5-5
     :description "Claude Haiku 5.5"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "claude-haiku-5-5"))
    (claude-haiku-5-5-effort-low
     :description "Claude Haiku 5.5 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "claude-haiku-5-5" :output_config (:effort "low")))
    (claude-haiku-5-5-effort-medium
     :description "Claude Haiku 5.5 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "claude-haiku-5-5" :output_config (:effort "medium")))
    (claude-haiku-5-5-effort-high
     :description "Claude Haiku 5.5 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "claude-haiku-5-5" :output_config (:effort "high")))
    (claude-haiku-5-5-effort-xhigh
     :description "Claude Haiku 5.5 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "claude-haiku-5-5" :output_config (:effort "xhigh")))
    (claude-haiku-5-5-effort-max
     :description "Claude Haiku 5.5 [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 0.1
     :output-cost 0.5
     :request-params (:model "claude-haiku-5-5" :output_config (:effort "max")))
    (claude-sonnet-5-5
     :description "Claude Sonnet 5.5"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 10
     :request-params (:model "claude-sonnet-5-5"))
    (claude-sonnet-5-5-effort-low
     :description "Claude Sonnet 5.5 [effort low]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 10
     :request-params (:model "claude-sonnet-5-5" :output_config (:effort "low")))
    (claude-sonnet-5-5-effort-medium
     :description "Claude Sonnet 5.5 [effort medium]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 10
     :request-params (:model "claude-sonnet-5-5" :output_config (:effort "medium")))
    (claude-sonnet-5-5-effort-high
     :description "Claude Sonnet 5.5 [effort high]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 10
     :request-params (:model "claude-sonnet-5-5" :output_config (:effort "high")))
    (claude-sonnet-5-5-effort-xhigh
     :description "Claude Sonnet 5.5 [effort xhigh]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 10
     :request-params (:model "claude-sonnet-5-5" :output_config (:effort "xhigh")))
    (claude-sonnet-5-5-effort-max
     :description "Claude Sonnet 5.5 [effort max]"
     :capabilities (tool-use reasoning cache media)
     :mime-types ("image/jpeg" "image/png" "image/webp" "image/gif")
     :context-window 1000
     :input-cost 2
     :output-cost 10
     :request-params (:model "claude-sonnet-5-5" :output_config (:effort "max")))))


;; ---- Served but not in your plan (minimum plan) ----
;;   claude-fable-5                       individual-provider
;;   claude-fable-5-1                     individual-provider
;;   claude-haiku-4-5-20251001            individual-pro
;;   claude-opus-4-7                      individual-provider
;;   claude-opus-4-8                      individual-provider
;;   claude-opus-5                        individual-provider
;;   claude-opus-5-5                      individual-provider
;;   claude-sonnet-4-6                    individual-pro
;;   claude-sonnet-5                      individual-pro
;;   google/gemini-3.1-flash-lite         individual-pro
;;   google/gemini-3.5-flash              individual-pro
;;   google/gemini-3.5-flash-lite         individual-pro
;;   google/gemini-3.6-flash              individual-pro
;;   google/gemini-4-argon                individual-pro
;;   gpt-5.3-codex                        individual-pro
;;   gpt-5.4                              individual-pro
;;   gpt-5.4-mini                         individual-pro
;;   gpt-5.5                              individual-pro
;;   gpt-5.6-terra                        individual-pro
;;   gpt-6-astra                          individual-provider
;;   gpt-6-sol                            individual-pro
;;   gpt-6.1-sol                          individual-max
;;   gpt-6.1-terra                        individual-pro
;;   meta/muse-spark-1.1                  individual-pro
;;   meta/muse-wonder-1.0                 individual-max
;;   sakana/fugu-ultra                    individual-provider
