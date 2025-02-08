;;; core-editor.el --- Editor configurations
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary:
;;
;; This file sets up editor.

;;; Code:

;;; Selected
;; delete the selection with a keypress
(delete-selection-mode t)

;; search selected text
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

;; smart move
(use-package mwim
  :ensure t
  :straight t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; multi-cursor
(use-package multiple-cursors
  :ensure t
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-s" . mc/skip-to-next-like-this))
  )


(use-package popup
  :ensure t
  :straight t
  )

(use-package ace-popup-menu
  :ensure t
  :straight t
  :config
  (ace-popup-menu-mode 1)
  (setq ace-popup-menu-show-pane-header t)
)

;; browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :straight t
  :bind ("M-y" . browse-kill-ring)
  )

(use-package whole-line-or-region
  :ensure t
  :straight t
  :bind ("M-w" . whole-line-or-region-copy-region-as-kill)
  ;; Comment or uncomment
  ;; (global-set-key (kbd "M-;") 'whole-line-or-region-comment-dwim-2)
  )

;; smart paring for all
(use-package smartparens
  :ensure t
  :straight t
  :config
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'java-mode-hook
                 'haskell-mode-hook
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'maxima-mode-hook
                 'ielm-mode-hook
                 'sh-mode-hook
                 'makefile-gmake-mode-hook
                 'php-mode-hook
                 'python-mode-hook
                 'js-mode-hook
                 'go-mode-hook
                 'qml-mode-hook
                 'jade-mode-hook
                 'css-mode-hook
                 'ruby-mode-hook
                 'coffee-mode-hook
                 'rust-mode-hook
                 'qmake-mode-hook
                 'lua-mode-hook
                 'swift-mode-hook
                 'minibuffer-inactive-mode-hook
                 )))
    :hook
    (hook . (lambda () (smartparens-mode 1)))
  )

;; easy-kill
(use-package easy-kill
  :ensure t
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark))
  )

;; expand-region
(use-package expand-region
  :ensure t
  :straight t
  :bind ("C-=" . er/expand-region)
  )

;; From: https://huadeyu.tech/tools/emacs-setup-notes.html
(use-package rainbow-mode
  :ensure t
  :straight t
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow)
    ))

(use-package rainbow-delimiters
  :ensure t
  :straight t
  :config
  (progn
    (defun @-enable-rainbow-delimiters ()
      (rainbow-delimiters-mode t))
    (add-hook 'prog-mode-hook '@-enable-rainbow-delimiters)))

;;; Search
;; avy
(use-package avy
  :straight t
  :ensure t  ; 确保 avy 已安装
  :bind (("M-g c" . avy-goto-char)  ; 绑定 C-; 到 avy-goto-char
         ("M-g f" . avy-goto-line)  ; 绑定 M-g f 到 avy-goto-line
         ("M-g w" . avy-goto-word-1))  ; 绑定 M-g w 到 avy-goto-word-1
  )

;; anzu
(use-package anzu
  :ensure t
  :straight t
  :config
  (global-anzu-mode +1)
  ;; enable anzu minor mode
  ;; (anzu-mode +1)
  :bind(([remap query-replace] . anzu-query-replace)
        ([remap query-replace-regexp] . anzu-query-replace-regexp)
        ;; anzu-query-replace-at-cursor for all file
        ;;anzu-query-replace-at-cursor-thing for this function or segment region
        ("M-%" . anzu-query-replace)
        ("C-M-%" . anzu-query-replace-regexp))
  )

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; regexp char
(if (display-graphic-p)
    (progn
      (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                     (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                     (36 . ".\\(?:>\\)")
                     (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                     (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                     (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                     (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                     (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                     (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                     (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                     (48 . ".\\(?:x[a-zA-Z]\\)")
                     (58 . ".\\(?:::\\|[:=]\\)")
                     (59 . ".\\(?:;;\\|;\\)")
                     (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                     (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                     (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                     (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                     (91 . ".\\(?:]\\)")
                     (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                     (94 . ".\\(?:=\\)")
                     (119 . ".\\(?:ww\\)")
                     (123 . ".\\(?:-\\)")
                     (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                     (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                     )
                   ))
        (dolist (char-regexp alist)
          (set-char-table-range composition-function-table (car char-regexp)
                                `([,(cdr char-regexp) 0 font-shape-gstring]))))
      ))

;;; Format
(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) wen-yank-indent-threshold)
      (indent-region beg end nil)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `wen-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode wen-indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode wen-yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; 当光标放在行的始端，或者行的中间位置，即为注释该行代码
;; 当光标放在行的末端，即为给该行代码添加注释
(defun improve-comment-dwim-line (&optional arg)
  "Replacement ARG for the 'comment-dwim' command."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-;") 'improve-comment-dwim-line)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
;; Two callable functions for enabling/disabling tabs in Emacs
(defun wen-disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun wen-enable-tabs ()
  (interactive)
  ;; (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  )

;; default tab settings
(setq-default indent-tabs-mode nil
              tab-width 4
              inhibit-splash-screen t
              initial-scratch-message nil
              sentence-end-double-space nil
              ;; disable indent for previous line
              electric-indent-inhibit t
              ;; delete tab method
              backward-delete-char-untabify-method nil
              ;; backward-delete-char-untabify-method 'hungry
              )

;; Newline at end of file
;; (setq require-final-newline t)

;;; Bookmark&History
;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" config-savefile-dir)
      bookmark-save-flag 1)

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" config-savefile-dir))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package diminish
  :ensure t
  :straight t
  )
;; autosave the undo-tree history
(use-package undo-tree
  :ensure t
  :straight t
  :config
  (progn
    (global-undo-tree-mode)
    (diminish 'undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    )
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
;; (setq undo-tree-auto-save-history t)
  )

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" config-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(kill-ring
        search-ring
        regexp-search-ring))
;; save every minute
(setq savehist-autosave-interval 60)
;; keep the home clean
(setq savehist-file (expand-file-name "savehist" config-savefile-dir))
(savehist-mode +1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)

;; save recent files
(defun wen-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list config-savefile-dir package-user-dir)))))

(use-package recentf
  :ensure t
  :straight t
  :config
  (setq recentf-save-file (expand-file-name "recentf" config-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude 'wen-recentf-exclude-p)
  (recentf-mode +1)
)

;;; Dir&File
(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(use-package neotree
  :ensure t
  :straight t
  :custom
  (neo-theme 'nerd2)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
    (setq neo-window-fixed-size nil)
    ;; (setq-default neo-show-hidden-files nil)
    ;; (global-set-key [f2] 'neotree-toggle)
    ;; (global-set-key [f8] 'neotree-dir)
    ))

;; large file
(use-package vlf
  :ensure t
  :straight t)

;;; Completion
;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie-expand replace dabbbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; use settings from .editorconfig file when present
(use-package editorconfig
  :ensure t
  :straight t
  :config
  (editorconfig-mode 1))

(use-package smart-tab
  :ensure t
  :straight t
  :config
  (progn
    (defun @-enable-smart-tab ()
      (smart-tab-mode))
    (add-hook 'prog-mode-hook '@-enable-smart-tab)
    )
  ;; smart tab behavior - indent or complete
  (setq tab-always-indent 'complete)
  )

(defun wen-enable-flyspell ()
  "Enable command `flyspell-mode' if `wen-flyspell' is not nil."
  (when (and wen-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

;; flyspell-mode does spell-checking on the fly as you type
;; TODO(jouyouyun): install 'aspell-en'
(use-package flyspell
  :ensure t
  :straight t
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  ;; use American English as ispell default dictionary to solve 'zh_CN'
  (ispell-change-dictionary "american" t)
  :hook
  (text-mode-hook . wen-enable-flyspell)
  )

;; operate-on-number
;; (require 'operate-on-number)
(use-package smartrep
  :ensure t
  :straight t
  :config
  (smartrep-define-key global-map "C-c ."
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("'" . operate-on-number-at-point)))
  )

(provide 'core-editor)

;;; core-editor.el ends here
