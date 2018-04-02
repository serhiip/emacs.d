;;; init-magit.el --- Git utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Remove default vc-mode and set up magit

;;; Code:

(require 'use-package)

(use-package magit
  :ensure t
  :init
  (setq vc-handled-backends '())  ;; disable default vc
  (add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . magit-log))
  :bind
  (("C-x g" . 'magit-status)))

(provide 'init-magit)

;;; init-magit.el ends here
