;;; module-ai --- AI configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up ai.

;;; Code:

(use-package vterm
  :straight t
  :ensure t)

(use-package ai-code-interface
  :straight (:host github :repo "tninja/ai-code-interface.el")
  :ensure t
  :config
  (ai-code-set-backend  'ai-code-codex-cli) ;; use codex as backend
  ;; Enable global keybinding for the main menu
  ;; (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(use-package eca-emacs
  :straight (:host github :repo "editor-code-assistant/eca-emacs")
  :ensure t)

;; gptel
(use-package gptel
  :ensure t
  :straight t
  :config
  (require 'gptel-integrations) ; for MCP integration
  (setq mcp-hub-servers wen-ai-gptel-mcp-hub)
  (setq gptel--known-backends nil) ; Remove default backends (ChatGPT)
  (setq gptel-backend
        (gptel-make-openai "Self"
          :host wen-ai-gptel-host
          :endpoint wen-ai-gptel-endpoint
          :stream t
          :key wen-ai-gptel-key
          :models wen-ai-gptel-models))
  (setq gptel-backend (gptel-get-backend "Self")) ; Default backend
  (setq gptel-model (car (gptel-backend-models gptel-backend))) ; Default model
  )

(provide 'module-ai)

;;; module-ai.el ends here
