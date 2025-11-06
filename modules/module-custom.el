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
