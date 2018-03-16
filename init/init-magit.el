;;; init-magit.el --- Git utilities

;;; Commentary:

;;; Remove default vc-mode and sets up magit

;;; Code:

(require 'use-package)

(setq vc-handled-backends '())

(use-package magit
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . magit-log))
  :bind
  (("C-x g" . 'magit-status)))

(provide 'init-magit)
;;; init-magit.el ends here
