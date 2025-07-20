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
  ;; use groq as default backend
  (setq gptel-model wen-ai-gptel-model
        gptel-backend
        (gptel-make-openai "self-hosted"
          :host wen-ai-gptel-host
          :endpoint wen-ai-gptel-endpoint
          :stream t
          :key wen-ai-gptel-key
          :models wen-ai-gptel-models))
  )

;; mcp
;; install fetch: pip install mcp-server-fetch --break-system-packages 
(use-package mcp
  :ensure t
  :straight (:host github :repo "lizqwerscott/mcp.el")
  :after gptel
  :custom (mcp-hub-servers
           `(("memory" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-memory")))
             ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
             ("fetch" . (:command "python" :args ("-m" "mcp_server_fetch")))
			 ))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server)
  )

(provide 'module-ai)

;;; module-ai.el ends here
