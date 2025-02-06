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

(provide 'core-straight)

;;; core-straight.el ends here
