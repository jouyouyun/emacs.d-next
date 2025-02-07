;;; module-obsidian --- Obsidian configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up obsidian.

;;; Code:

(use-package obsidian
  :ensure t
  :straight t
  :demand t
  :config
  (obsidian-specify-path wen-obsidian-knowledge)
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory wen-obsidian-inbox)
  ;; Create missing files in inbox? - when clicking on a wiki link
  ;; t: in inbox, nil: next to the file with the link
  ;; default: t
  ;(obsidian-wiki-link-create-file-in-inbox nil)
  ;; The directory for daily notes (file name is YYYY-MM-DD.md)
  (obsidian-daily-notes-directory wen-obsidian-daily)
  ;; Directory of note templates, unset (nil) by default
  ;(obsidian-templates-directory "Templates")
  ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
  ;(obsidian-daily-note-template "Daily Note Template.md")
  :bind (:map obsidian-mode-map
  ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
  ("C-c M-o" . obsidian-follow-link-at-point)
  ;; Jump to backlinks
  ("C-c M-b" . obsidian-backlink-jump)
  ;; If you prefer you can use `obsidian-insert-link'
  ("C-c M-l" . obsidian-insert-wikilink)))

(provide 'module-obsidian)

;;; module-obsidian.el ends here
