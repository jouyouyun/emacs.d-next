;;; module-lsp --- LSP configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up lsp.

;;; Code:

(use-package company
  :ensure t
  :straight t
  :defer 0.1
  :config
  (setq-default
   ;; company-auto-complete t
   company-idle-delay 0.3
   ;; company-require-match nil
   company-minimum-prefix-length 2
   company-show-numbers t
   company-tooltip-limit 10
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t

   ;; get only preview
   ;; company-frontends '(company-preview-frontend)
   ;; also get a drop down
   ;; company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
   )
  ;; Remove duplicate candidate.
  (add-to-list 'company-transformers #'delete-dups)
  ;; (global-company-mode t)
  :hook (after-init . global-company-mode)
  )

;; show icons
(use-package company-box
  :ensure t
  :straight t
  :if window-system
  :hook (company-mode . company-box-mode))

;;; GoLang
;; Dependencies:
;;     github.com/klauspost/asmfmt/cmd/asmfmt
;;     github.com/go-delve/delve/cmd/dlv
;;     github.com/kisielk/errcheck
;;     github.com/davidrjenni/reftools/cmd/fillstruct
;;     github.com/mdempsky/gocode
;;     github.com/stamblerre/gocode --- gocode for module
;;     github.com/rogpeppe/godef
;;     github.com/zmb3/gogetdoc
;;     golang.org/x/tools/cmd/goimports
;;     golang.org/x/lint/golint
;;     golang.org/x/tools/gopls@latest
;;     github.com/golangci/golangci-lint/cmd/golangci-lint
;;     github.com/fatih/gomodifytags
;;     golang.org/x/tools/cmd/gorename
;;     github.com/jstemmer/gotags
;;     golang.org/x/tools/cmd/guru
;;     github.com/josharian/impl
;;     honnef.co/go/tools/cmd/keyify
;;     github.com/fatih/motion
;;     github.com/koron/iferr
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package lsp-mode
  :ensure t
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  ;; disable lsp indent
  ;; (lsp-enable-indentation nil)
  :config
  ;; gopls
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  ;; fix 'https://github.com/emacs-lsp/lsp-mode/issues/4225'
  (setq lsp-go-server-path "gopls")
  ;; Configuration to fix LSP
  ;; we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
  (setq lsp-enable-eldoc nil)
  ;; avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
  (setq lsp-message-project-root-warning t)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (go-mode . lsp)
         (go-mode . lsp-go-install-save-hooks)
         (go-mode . lsp-deferred)
         (python-mode . lsp)
         (c++-mode . lsp)
         (c-mode . lsp)
         (c-mode-common . lsp)
         (rust-mode . lsp)
         (html-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (json-mode . lsp)
         (yaml-mode . lsp)
         (dockerfile-mode . lsp)
         (shell-mode . lsp)
         (css-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :ensure t
  :straight t
  :custom-face
  ;; (lsp-ui-doc-background ((t (:background ni))))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-include-signature t

              lsp-enable-snippet nil
              lsp-ui-sideline-enable nil
              lsp-ui-peek-enable t
              lsp-ui-doc-position              'at-point
              lsp-ui-doc-header                t
              lsp-ui-doc-border                "white"
              lsp-ui-doc-include-signature     t
              lsp-ui-sideline-update-mode      'point
              lsp-ui-sideline-delay            1
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-peek-always-show          nil
              lsp-ui-flycheck-enable           nil
              )
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :straight t
  :config
  (lsp-treemacs-sync-mode 1)
  (setq lsp-metals-treeview-show-when-views-received t)
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :straight t
  )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; LaTex
;; dependencies: texlab
(use-package lsp-latex
  :ensure t
  :straight t
  :config
  ;; "texlab" must be located at a directory contained in `exec-path'.
  ;; If you want to put "texlab" somewhere else,
  ;; you can specify the path to "texlab" as follows:
  ;; (setq lsp-latex-texlab-executable "/usr/local/bin/texlab"))
  (add-hook 'latex-mode-hook
            (lambda ()
              ;; Use 'luatex' as default tex engine
              (setq TeX-engine 'luatex
                    TeX-show-compilation t)
              (add-to-list 'tex-compile-commands '("lualatex -interaction=nonstopmode --shell-escape --synctex=1 %f" t "%r.pdf"))
              ;; (setq lsp-latex-texlab-executable-argument-list '("-vvvv" "--log-file" "/tmp/texlab.log"))
              (setq lsp-latex-build-executable "lualatex") ;; why not work, still use 'latexmk'? Now add the file '~/.latexmkrc'
              (setq lsp-latex-build-args '("-lualatex" "-interaction=nonstopmode" "--shell-escape" "-synctex=1" "%f"))))
  ;; if emacs not build with json, commentted the next line
  (when (version< emacs-version "28.2")
    (message "replace 'json-serialize' with 'json-encode'")
    (advice-add #'json-serialize :before-until #'advice-json-serialize)
  )
  (setq lsp-tex-server 'texlab)
  )

(with-eval-after-load "tex-mode"
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp))
;; For YaTeX
(with-eval-after-load "yatex"
  (add-hook 'yatex-mode-hook 'lsp))
;; For bibtex
(with-eval-after-load "bibtex"
  (add-hook 'bibtex-mode-hook 'lsp))

;; python
(use-package lsp-python-ms
  :ensure t
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

(provide 'module-lsp)

;;; module-lsp.el ends here
