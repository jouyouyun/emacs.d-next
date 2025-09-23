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

;; aidermacs
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :config
  (setq aidermacs-extra-args wen-ai-aidermacs-args)
  (setq aidermacs-auto-accept-architect t)
  (setq aidermacs-comint-multiline-newline-key "S-<return>")
  (setq aidermacs-watch-files t)
  ;; Use vterm backend (default is comint)
  (setq aidermacs-backend 'vterm)
  ;; don't match emacs theme colors
  (setopt aidermacs-vterm-use-theme-colors nil)
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model wen-ai-aidermacs-model)
  )

;; aider
(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setenv wen-ai-aider-key-env wen-ai-aider-key)
  (setq aider-args wen-ai-aider-args)
  )

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

;; mcp
(use-package mcp
  :ensure t
  :straight (:host github :repo "lizqwerscott/mcp.el")
  :after gptel
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server)
  )

(provide 'module-ai)

;;; module-ai.el ends here
