;;; module-ai --- AI configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up ai.

;;; Code:

;; aider
(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setenv wen-ai-aider-key-env wen-ai-aider-key)
  (setq aider-args wen-ai-aider-args)
  ;; use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu)
  )

;; gptel
(use-package gptel
  :ensure t
  :straight t
  :config
  ;; use groq as default backend
  (setq gptel-model wen-ai-gptel-groq-model
        gptel-backend
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key wen-ai-gptel-groq-key
          :models '(deepseek-r1-distill-qwen-32b
                    qwen-2.5-coder-32b
                    llama3-70b-8192
                    mixtral-8x7b-32768
                    whisper-large-v3)))
  )

;; ellama
(use-package llm
  :ensure t
  :straight t
  )
(use-package ellama
  :ensure t
  :straight t
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  (require 'llm-gemini)
  (setopt ellama-provider
          (make-llm-gemini
           :key wen-ai-llm-gemini-key
           :chat-model wen-ai-llm-gemini-model))
  )

(provide 'module-ai)

;;; module-ai.el ends here
