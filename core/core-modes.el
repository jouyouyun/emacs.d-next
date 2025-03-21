;;; core-modes.el --- File modes configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up file modes.

;;; Code:

(use-package asn1-mode
  :ensure t
  :straight t
  :mode ("\\.asn1\\'" . asn1-mode)
  )

(use-package clojure-mode
  :ensure t
  :straight t
  :mode ("\\.clj\\'" . clojure-mode)
  )

(use-package cmake-mode
  :ensure t
  :straight t
  :mode ("\\CMakeLists\\.txt\\'" . cmake-mode)
  )

(use-package coffee-mode
  :ensure t
  :straight t
  :mode ("\\.coffee\\'" . coffee-mode)
  )

(use-package css-mode
  :ensure t
  :straight t
  :mode ("\\.css\\'" . css-mode)
  )

(use-package csv-mode
  :ensure t
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  )

(use-package cask-mode
  :ensure t
  :straight t
  :mode ("\\Cask\\'" . cask-mode)
  )

(use-package d-mode
  :ensure t
  :straight t
  :mode ("\\.d\\'" . d-mode)
  )

(use-package dart-mode
  :ensure t
  :straight t
  :mode ("\\.dart\\'" . dart-mode)
  )

(use-package elm-mode
  :ensure t
  :straight t
  :mode ("\\.elm\\'" . elm-mode)
  )

(use-package elixir-mode
  :ensure t
  :straight t
  :mode ("\\.ex\\'" . elixir-mode)
  )

(use-package erlang
  :ensure t
  :straight t
  :mode ("\\.erl\\'" . erlang-mode)
  )

(use-package feature-mode
  :ensure t
  :straight t
  :mode ("\\.feature\\'" . feature-mode)
  )

(use-package graphql-mode
  :ensure t
  :straight t
  :mode ("\\.graphql\\'" . graphql-mode)
  )

(use-package groovy-mode
  :ensure t
  :straight t
  :mode ("\\.groovy\\'" . groovy-mode)
  )

(use-package haml-mode
  :ensure t
  :straight t
  :mode ("\\.haml\\'" . haml-mode)
  )

(use-package haskell-mode
  :ensure t
  :straight t
  :mode ("\\.hs\\'" . haskell-mode)
  )

(use-package json-mode
  :ensure t
  :straight t
  :mode ("\\.json\\'" . json-mode)
  )
;; Fix json-serialize encode wrong.
;; (json-serialize (list :processId nil) :null-object nil :false-object :json-false))
;; except: "{\"processId\":null}", but got "{\"processId\":{}}"
;; So change lsp json serialize to json-encode.
(defun advice-json-serialize (params &rest args)
  (unless (plist-get args :null-object)
    (let ((json-false (plist-get args :false-object))
          (json-null (plist-get args :null-object)))
      (json-encode params))))

(use-package kotlin-mode
  :ensure t
  :straight t
  :mode ("\\.kt\\'" . kotlin-mode)
  )

(use-package kivy-mode
  :ensure t
  :straight t
  :mode ("\\.kv\\'" . kivy-mode)
  )

(use-package auctex
  :ensure t
  :straight t
  :mode ("\\.latex\\'" . LaTeX-mode)
  )

(use-package less-css-mode
  :ensure t
  :straight t
  :mode("\\.less\\'" . less-css-mode)
  )

(use-package lua-mode
  :ensure t
  :straight t
  :mode ("\\.lua\\'" . lua-mode)
  )

(use-package markdown-mode
  :ensure t
  :straight t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :commands (markdown-mode gfm-mode)
  )
(use-package markdown-preview-mode
  :ensure t
  :straight t
  :config
  ;; Add extra css to default solarized dark theme
  ;; (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/tango.css")
  ;; (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
  ;; (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/monokai.css")
  ;; Override theme completely
  ;; (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
  (setq markdown-preview-stylesheets (list "https://raw.githubusercontent.com/richleland/pygments-css/master/monokai.css"))
  ;; Extra javascript
  ;; Add MathJax
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
  )

(use-package tuareg
  :ensure t
  :straight t
  :mode ("\\.ml\\'" . tuareg-mode)
  )

(use-package puppet-mode
  :ensure t
  :straight t
  :mode ("\\.pp\\'" . puppet-mode)
  )

(use-package php-mode
  :ensure t
  :straight t
  :mode ("\\.php\\'" . php-mode)
  )

(use-package protobuf-mode
  :ensure t
  :straight t
  :mode ("\\.proto\\'" . protobuf-mode)
  )

(use-package cython-mode
  :ensure t
  :straight t
  :mode (("\\.pyd\\'" . cython-mode)
         ("\\.pyi\\'" . cython-mode)
         ("\\.pyx\\'" . cython-mode))
  )

(use-package pkgbuild-mode
  :ensure t
  :straight t
  :mode ("\\.PKGBUILD\\'" . pkgbuild-mode)
  )

(use-package go-mode
  :ensure t
  :straight t
  :mode ("\\.go\\'" . go-mode)
  )

(use-package rust-mode
  :ensure t
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  )

(use-package sass-mode
  :ensure t
  :straight t
  :mode ("\\.sass\\'" . sass-mode)
  )

(use-package scala-mode
  :ensure t
  :straight t
  :mode ("\\.scala\\'" . scala-mode)
  )

(use-package scss-mode
  :ensure t
  :straight t
  :mode ("\\.scss\\'" . scss-mode)
  )

(use-package slim-mode
  :ensure t
  :straight t
  :mode ("\\.slim\\'" . slim-mode)
  )

(use-package stylus-mode
  :ensure t
  :straight t
  :mode ("\\.styl\\'" . stylus-mode)
  )

(use-package swift-mode
  :ensure t
  :straight t
  :mode ("\\.swift\\'" . swift-mode)
  )

(use-package textile-mode
  :ensure t
  :straight t
  :mode ("\\.textile\\'" . textile-mode)
  )

(use-package thrift-mode
  :ensure t
  :straight t
  :mode ("\\.thrift\\'" . thrift-mode)
  )

(use-package yaml-mode
  :ensure t
  :straight t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  )

(use-package dockerfile-mode
  :ensure t
  :straight t
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  )

(use-package varlink-mode
  :ensure t
  :straight (:host github :repo "varlink/libvarlink" :files ("emacs/varlink-mode.el"))
  :mode ("\\.varlink\\'" . varlink-mode)
  )

(provide 'core-modes)

;;; core-modes.el ends here
