;;; init-projectile.el --- Common functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'projectile)

(use-package projectile
  :demand
  :ensure t
  :init
  (setq projectile-use-git-grep t)
  (custom-set-variables
   '(projectile-completion-system 'helm))
  :config (projectile-mode t))

(provide 'init-projectile)

;;; init-projectile.el ends here
