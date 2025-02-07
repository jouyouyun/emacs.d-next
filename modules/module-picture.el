;;; module-picture --- Picture configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up picture.

;;; Code:

;; Depends: graphviz, default-jdk
;; (wen-require-packages '(plantuml-mode flycheck-plantuml))
(use-package plantuml-mode
  :ensure t
  :straight t
  :config
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  ;; Integration with org-mode
  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  ;; Sample jar configuration
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" config-personal-dir))
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" config-personal-dir))
  (setq plantuml-default-exec-mode 'jar)  
  ;; server mode
  ;; (setq plantuml-default-exec-mode 'server)
  ;; (setq plantuml-server-url "https://www.plantuml.com/plantuml")
  )

;; Dependencies: graphviz
;; Links: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
;;        https://coldnew.github.io/787b7d73/
;;        https://brantou.github.io/2018/04/23/ob-dot-intro/
(use-package graphviz-dot-mode
  :ensure t
  :straight t
  :config
  (setq graphviz-dot-indent-width 4)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t))) ; this line activates dot
  )

;; Depends: d2
;;  wget https://d2lang.com/install.sh
;;  chmod +x install.sh
;;  sudo ./install.sh --prefix=/usr/local
(when (executable-find "d2")
  ;; d2-mode
  (message "enable d2-mode")
  (use-package d2-mode
    :ensure t
    :straight t
    :config
    (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))
    )
  ;; Integration with org-mode
  (use-package ob-d2
    :ensure t
    :straight t
    :config
    (add-to-list 'org-src-lang-modes '("d2" . d2))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((d2 . t))) ; this line activates dot
    )
  )

;; Depends: mermaid-cli
;;    sudo npm install -g @mermaid-js/mermaid-cli
;;  or using docker
;;    docker  pull minlag/mermaid-cli:latest
;;    (setq mermaid-mmdc-location "docker")
;;    (setq mermaid-flags "run -u 1000 -v /tmp:/tmp minlag/mermaid-cli:latest")
(when (executable-find "mmdc")
  ;; mermaid-mode
  (message "enable mermaid-mode")
  (use-package mermaid-mode
    :ensure t
    :straight t
    :config
    (add-to-list 'auto-mode-alist '("\\.mmdc\\'" . mermaid-mode))
    (add-to-list 'auto-mode-alist '("\\.mermaid\\'" . mermaid-mode))
    )
  ;; Integration with org-mode
  (use-package ob-mermaid
    :ensure t
    :straight t
    :config
    (add-to-list 'org-src-lang-modes '("mermaid" . mermaid))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((mermaid . t))) ; this line activates dot
    )
  )

(provide 'module-picture)

;;; module-picture.el ends here
