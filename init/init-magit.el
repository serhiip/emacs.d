(require 'use-package)

(use-package magit
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . magit-log))
  :bind
  (("C-x g" . 'magit-status)))

(provide 'init-magit)
