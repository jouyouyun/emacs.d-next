;;; core-ui.el --- UI tweaks.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;; This file sets up tool bar, theme, opacity etc.

;;; Code:

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))

;; hide menu bar
;; (menu-bar-mode -1)
;; Disable menubar, toolbar and scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; Disable menubar, toolbar and scrollbar
;; (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
;;   (when (fboundp mode) (funcall mode -1)))

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; auto break line
(global-visual-line-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Wen - " (:eval (if (buffer-file-name)
                                                    (abbreviate-file-name (buffer-file-name))
                                              "%b"))))

;; white-sand
(use-package github-modern-theme
  :ensure t
  :straight t
  :config
  (load-theme github-modern t)
  )
;; use zenburn theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :straight t
;;   :config
;;   (load-theme zenburn t)
;;   )

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; show the cursor when moving after big movements in the window
(use-package beacon
  :ensure t
  :straight t
  :config
  (beacon-mode +1)
  )

;; show available keybindings after you start typing
(use-package which-key
  :ensure t
  :straight t
  :config
  (which-key-mode +1)
  )

;; change frame opacity
(use-package seethru
  :ensure t
  :straight t
  ;; "C-c 8" and "C-c 9"
  :bind  ("C-c 0" . (lambda () (interactive) (seethru 100)))
  :config
  (seethru-recommended-keybinds "C-c")
  ;; hold control while wheeling mouse to change transparency
  (seethru-mouse-bindings "C")
  )

;; Compilation from Emacs
(defun wen-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'wen-colorize-compilation-buffer)

(defun wen-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `wen-clean-whitespace-on-save' is not nil."
  (when wen-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun wen-enable-whitespace ()
  "Enable `whitespace-mode' if `wen-whitespace' is not nil."
  (when wen-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'wen-cleanup-maybe nil t)
    (whitespace-mode +1)))

;; whitespace-mode config
(use-package whitespace
  :ensure t
  :straight t
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :hook
  (text-mode-hook . wen-enable-whitespace)
  )

;; ;; set tab color
;; (global-whitespace-mode)
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#9999CC")))))

;; (setq whitespace-display-mappings
;;       '((tab-mark 9 [124 9] [92 9])))

;; highlight parentheses
(use-package highlight-parentheses
  :ensure t
  :straight t
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
  )

;; disable annoying blink-matching-paren
(setq blink-matching-paren t)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(require 'rect)
(require 'crux)
(crux-with-region-or-line kill-region)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; diff-hl
(use-package diff-hl
  :ensure t
  :straight t
  :config
  (global-diff-hl-mode +1)
  :hook ((dired-mode-hook . diff-hl-dired-mode))
  )

(provide 'core-ui)

;;; core-ui.el ends here
