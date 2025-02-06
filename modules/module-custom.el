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
;; groq/llama3-70b-8192
;; deepseek/deepseek-chat
(defcustom wen-ai-aider-model ""
  "The aider model."
  :type 'string
  :group 'wen-ai)

;; GEMINI_API_KEY
;; GROQ_API_KEY
;; DEEPSEEK_API_KEY
;; OLLAMA_API_BASE=http://127.0.0.1:11434
(defcustom wen-ai-aider-key-env ""
  "The aider key env."
  :type 'string
  :group 'wen-ai)

(defcustom wen-ai-aider-key ""
  "The aider key."
  :type 'string
  :group 'wen-ai)

(provide 'module-custom)

;;; module-custom.el ends here
