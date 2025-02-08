;;; core-window.el --- Widnow configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up window management shortcuts.

;;; Code:

(defun wen-fullscreen ()
  "Make Emacs window fullscreen."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; ace-window
(use-package ace-window
  :straight t
  :ensure t
  :bind (("C-x o" . ace-window)
         ([remap other-window] . ace-window))
  )

;; enable winner-mode to manage window configurations
;; C-c left --> winner-undo
;; C-c right --> winner-redo
;; (use-package winner-mode
;;   :ensure t
;;   :straight t
;;   :config
;;   (winner-mode +1)
;;   )

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(use-package super-save
  :ensure t
  :straight t
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  )

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(set-default 'imenu-auto-rescan t)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(provide 'core-window)

;;; core-window.el ends here
