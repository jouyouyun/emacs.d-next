;;; core-packages.el --- Package management configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>
;;

;;; Commentary:
;; This file sets up package repository and define package management functions.

;;; Code:

;; (require 'cl-lib)

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

;; avy
(use-package avy
  :straight t
  :ensure t  ; 确保 avy 已安装
  :bind (("M-g c" . avy-goto-char)  ; 绑定 C-; 到 avy-goto-char
         ("M-g f" . avy-goto-line)  ; 绑定 M-g f 到 avy-goto-line
         ("M-g w" . avy-goto-word-1))  ; 绑定 M-g w 到 avy-goto-word-1
  )

;; ace-window
(use-package ace-window
  :straight t
  :ensure t
  :bind ("C-x o" . ace-window)
  :config
  (global-set-key [remap other-window] 'ace-window)
  )

(provide 'core-packages)

;;; core-packages.el ends here
