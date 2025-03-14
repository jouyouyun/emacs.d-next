;;; module-ai --- AI configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up ai.

;;; Code:

;; codeium
(use-package codeium
    :straight '(:type git :host github :repo "Exafunction/codeium.el")
    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    (add-hook 'emacs-startup-hook
      (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; aider
(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setenv wen-ai-aider-key-env wen-ai-aider-key)
  (setq aider-args wen-ai-aider-args)
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu)
  )

;; aidermacs
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :config
  (setq aidermacs-default-model wen-ai-aider-model)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-architect-model wen-ai-aider-arch-model)
  (setq aidermacs-editor-model wen-ai-aider-model)
  (setq aidermacs-extra-args wen-ai-aidermacs-args)
  ;; Multiline Input Configuration
  ;; Comint backend:
  (setq aidermacs-comint-multiline-newline-key "S-<return>")
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
