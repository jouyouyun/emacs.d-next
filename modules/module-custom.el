;;; module-custom.el --- Define configuration variables.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file defines some custom variables for modules.

;;; Code:

(defgroup wen-ai nil
  "Emacs Wen ai configuration."
  :prefix "wen-ai-"
  :group 'convenience)

(defcustom wen-ai-aidermacs-args '("--no-auto-commits" "--model" "gemini/gemini-1.5-pro")
  "The aider args, such as: model."
  :type '(repeat string)
  :group 'wen-ai)
(defcustom wen-ai-aidermacs-model ""
  "The aider default model."
  :type 'string
  :group 'wen-ai)

;; gemini/gemini-1.5-pro
;; gemini/gemini-2.0-flash
;; groq/llama3-70b-8192
;; deepseek/deepseek-chat
(defcustom wen-ai-aider-args '("--no-auto-commits" "--model" "gemini/gemini-1.5-pro")
  "The aider args, such as: model."
  :type '(repeat string)
  :group 'wen-ai)
;; GEMINI_API_KEY
;; GROQ_API_KEY
;; DEEPSEEK_API_KEY
;; OLLAMA_API_BASE=http://127.0.0.1:11434
(defcustom wen-ai-aider-key-env "GEMINI_API_KEY"
  "The aider key env."
  :type 'string
  :group 'wen-ai)
(defcustom wen-ai-aider-key ""
  "The aider key."
  :type 'string
  :group 'wen-ai)

(defcustom wen-ai-gptel-model 'gemini-2.5-flash
  "The default gptel groq model."
  :type 'symbol
  :group 'wen-ai)
(defcustom wen-ai-gptel-host ""
  "The gptel service host."
  :type 'string
  :group 'wen-ai)
(defcustom wen-ai-gptel-endpoint "/v1/chat/completions"
  "The gptel service endpoint."
  :type 'string
  :group 'wen-ai)
(defcustom wen-ai-gptel-key ""
  "The gptel service key."
  :type 'string
  :group 'wen-ai)
(defcustom wen-ai-gptel-models '("gemini-2.5-flash" "gemini-2.5-flash-live-preview")
  "The gptel service models."
  :type '(repeat string)
  :group 'wen-ai)

(defcustom wen-ai-gptel-mcp-hub nil
  "The gptel mcp hub servers."
  :type '(list (cons string (list symbol string)))
  :group 'wen-ai)

(defgroup wen-obsidian nil
  "Emacs Wen obsidian configuration."
  :prefix "wen-obsidian-"
  :group 'convenience)

(defcustom wen-obsidian-knowledge ""
  "The obsidian knowledge base."
  :type 'string
  :group 'wen-obsidian)

(defcustom wen-obsidian-inbox ""
  "The obsidian capture base."
  :type 'string
  :group 'wen-obsidian)

(defcustom wen-obsidian-daily ""
  "The obsidian daily base."
  :type 'string
  :group 'wen-obsidian)

(provide 'module-custom)

;;; module-custom.el ends here
