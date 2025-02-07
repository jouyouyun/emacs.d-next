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

(defcustom wen-ai-gptel-groq-key ""
  "The gptel groq key."
  :type 'string
  :group 'wen-ai)

(defcustom wen-ai-llm-gemini-key ""
  "The llm gemini key."
  :type 'string
  :group 'wen-ai)

(defcustom wen-ai-llm-gemini-model ""
  "The llm gemini model."
  :type 'string
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
