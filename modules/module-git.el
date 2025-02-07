;;; module-git --- Git configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up git.

;;; Code:

;; magit
(use-package magit
    :ensure t
    :straight t
    :hook (magit-post-refresh . diff-hl-magit-post-refresh)
    :bind (("C-x g" . magit-status)
           ("C-x M-g" . magit-dispatch))
    )

(use-package git-gutter+
  :ensure t
  :straight t
  :config
  (progn
    (global-git-gutter+-mode)))

(provide 'module-git)

;;; module-git.el ends here
