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

;; for slash commands popup
(use-package popup :ensure t)
(use-package gemini-cli
  :straight (:type git :host github :repo "linchen2chris/gemini-cli.el" :branch "main")
  :bind-keymap
  ("C-c c" . gemini-cli-command-map)
  :config
  (setq gemini-cli-terminal-backend 'vterm)
  (gemini-cli-mode))

(use-package eca
  :straight (:type git :host github :repo "editor-code-assistant/eca-emacs")
  :ensure t)

(use-package codex-cli
  :straight (:type git :host github :repo "bennfocus/codex-cli.el")
  ;; :bind (("C-c c t" . codex-cli-toggle)
  ;;        ("C-c c s" . codex-cli-start)
  ;;        ("C-c c q" . codex-cli-stop)
  ;;        ("C-c c Q" . codex-cli-stop-all)
  ;;        ("C-c c p" . codex-cli-send-prompt)
  ;;        ("C-c c r" . codex-cli-send-region)
  ;;        ("C-c c f" . codex-cli-send-file)
  ;;        ;; Show-all layout + paging
  ;;        ("C-c c a" . codex-cli-toggle-all)
  ;;        ("C-c c n" . codex-cli-toggle-all-next-page)
  ;;        ("C-c c b" . codex-cli-toggle-all-prev-page))
  :init
  (setq codex-cli-executable "codex"
        codex-cli-terminal-backend 'vterm
        codex-cli-side 'right
        codex-cli-width 90))

;; ;; gptel
;; (use-package gptel
;;   :ensure t
;;   :straight t
;;   :config
;;   (require 'gptel-integrations) ; for MCP integration
;;   (setq mcp-hub-servers wen-ai-gptel-mcp-hub)
;;   (setq gptel--known-backends nil) ; Remove default backends (ChatGPT)
;;   (setq gptel-backend
;;         (gptel-make-openai "Self"
;;           :host wen-ai-gptel-host
;;           :endpoint wen-ai-gptel-endpoint
;;           :stream t
;;           :key wen-ai-gptel-key
;;           :models wen-ai-gptel-models))
;;   (setq gptel-backend (gptel-get-backend "Self")) ; Default backend
;;   (setq gptel-model (car (gptel-backend-models gptel-backend))) ; Default model
;;   )

(provide 'module-ai)

;;; module-ai.el ends here
