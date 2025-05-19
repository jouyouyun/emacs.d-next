;;; module-ai --- AI configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up ai.

;;; Code:

;; aidermacs
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :config
  (setq aidermacs-extra-args wen-ai-aidermacs-args)
  (setq aidermacs-auto-accept-architect t)
  (setq aidermacs-comint-multiline-newline-key "S-<return>")
  (setq aidermacs-watch-files t)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model wen-ai-aider-model)
  (aidermacs-architect-model wen-ai-aider-arch-model)
  (aidermacs-editor-model wen-ai-aider-model)
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
  (setq gptel-model wen-ai-gptel-groq-model
        gptel-backend
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key wen-ai-gptel-groq-key
          :models '(llama3-70b-8192
                    mixtral-8x7b-32768)))
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
