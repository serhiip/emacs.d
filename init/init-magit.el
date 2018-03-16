;;; init-magit.el --- Git utilities

;;; Commentary:

;;; Remove default vc-mode and set up magit

;;; Code:

(require 'use-package)

(use-package magit
  :ensure t
  :init
  (setq vc-handled-backends '())
  (add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . magit-log))
  :bind
  (("C-x g" . 'magit-status)))

(provide 'init-magit)
;;; init-magit.el ends here
