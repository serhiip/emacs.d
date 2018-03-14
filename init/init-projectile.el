(require 'use-package)

(use-package projectile
  :demand
  :ensure t
  :init
  (setq projectile-use-git-grep t)
  (custom-set-variables
   '(projectile-completion-system 'helm))
  :config (projectile-global-mode t))

(provide 'init-projectile)
