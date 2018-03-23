(require 'functions)

(set-frame-parameter nil 'fullscreen 'fullboth)
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'monokai t)
(transient-mark-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

(setq
 confirm-nonexistent-file-or-buffer nil
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 inhibit-startup-screen t
 js-indent-level 4
 whitespace-line-column 120)

(setq-default indent-tabs-mode nil)
(add-to-list 'exec-path "/usr/local/bin")
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook
 'prog-mode-hook
 #'(lambda ()
     (flycheck-mode)
     (prelude-font-lock-comment-annotations)
     (company-mode)))

(provide 'init-emacs-settings)
