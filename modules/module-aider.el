
;;; module-aider --- aider configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up aider.

;;; Code:

(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aider-args '("--model" wen-ai-aider-model))
  (setenv wen-ai-aider-key-env wen-ai-aider-key)
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

(provide 'module-aider)

;;; module-aider.el ends here
