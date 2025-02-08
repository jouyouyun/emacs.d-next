;;; core-ivy.el --- Ivy configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up ivy, swiper and counsel.

;;; Code:

;; wgrep for ivy multi-editor
(use-package wgrep
  :ensure t
  :straight t
  )

(use-package ivy
  :ensure t
  :straight t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  )

;; enable this if you want `swiper' to use it
(use-package swiper
  :ensure t
  :straight t
  :config
  (setq search-default-mode #'char-fold-to-regexp)
  ;; :bind ("C-s" . swiper)
  )

(use-package counsel
  :ensure t
  :straight t
  :bind (("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-ag)
  ("C-x l" . counsel-locate))
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
)

(use-package all-the-icons-ivy
             :ensure t
             :config
             (all-the-icons-ivy-setup))


(use-package counsel-tramp
  :ensure t
  :straight t
  :config
  ;; counsel-tramp for sudo, ssh, docker
  (setq tramp-default-method "ssh")
  (define-key global-map (kbd "C-c s") 'counsel-tramp)
  ;; speed up tramp
  (add-hook 'counsel-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
                                               (projectile-mode 0)
                                               (editorconfig-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
                                        (projectile-mode 1)
                                        (editorconfig-mode 1)))
  (setq make-backup-files nil)
  ;; If you are using docker-tramp, docker is also supplemented.
  ;; If you are using vagrant-tramp, vagrant is also supplemented.
  )

;; counsel-projectile: project management
(use-package counsel-projectile
  :ensure t
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; counsel-gtags
(use-package counsel-gtags
  :ensure t
  :straight t
  :config
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook counsel-gtags-mode)
  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
    ;; (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-pop-stack) ;; replace with xref-pop-maker-stack
    )
  )

(use-package ivy-xref
  :ensure t
  :straight t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; gxref
(use-package gxref
  :ensure t
  :straight t
  :config
  (add-to-list 'xref-backend-functions 'gxref-xref-backend)
  )

;; move to lsp
;; (use-package lsp-ivy
;;   :ensure t
;;   :straight t
;;   )

(provide 'core-ivy)

;;; core-ivy.el ends here
