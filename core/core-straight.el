;;; core-straight.el --- Straight package management configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;;

;;; Commentary:
;; This file sets up package manager.

;;; Code:

;; https://github.com/radian-software/straight.el
;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)
;; init straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://radian-software.github.io/straight.el/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; shadow clone git repo to improve the speed
(setq straight-vc-git-default-clone-depth 1)
;; use package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; install package example
;; (use-package NAME
;;   :ensure t
;;   :straight t
;;   ;; execute code before a package is loaded
;;   :init
;;   (setq xxx "")
;;   ;; execute code after a package is loaded
;;   :config
;;   (setq xxx "")
;;   ;; key-binding
;;   :bind (([remap fill-paragraph] . unfill-toggle)
;;          :map <MAP>
;;          ("C-c x" . CMD))
;;   ;; binding to keymaps only
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   ;; establish a deferred binding within the auto-mode-alist
;;   ;; and interpreter-mode-alist variables
;;   :mode "\\.rb\\'"
;;   :interpreter "ruby"
;;   ;; package is "python" but the mode is "python-mode"
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   ;; :magic and :magic-fallback to cause certain function to be
;;   ;; run if the beginning of a file matches a given regular expression
;;   ;; :magic-fallback has a lower priority than :mode
;;   :magic ("%PDF" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install :no-query)
;;   ;; :hook keyword allows adding functions onto package hooks
;;   :hook ((prog-mode . company-mode)
;;          (text-mode . company-mode))
;;   ;; :custom keyword allows customization of package custom variables
;;   :custom
;;   (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
;;   (comint-prompt-read-only t "Make the prompt read only.")
;;   ;; :custom-face keyword allows customization of package custom faces
;;   :custom-face
;;   (example-1-face ((t (:foreground "LightPink"))))
;;   (example-2-face ((t (:foreground "LightGreen"))) face-defspec-spec)
;;  )

(provide 'core-straight)

;;; core-straight.el ends here
