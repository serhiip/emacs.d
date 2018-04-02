;;; init-ts.el --- Typescript -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package company :ensure t)

(use-package tide
  :ensure t
  :after (company)
  :init
  (add-hook
   'typescript-mode-hook
   #'(lambda ()
       (tide-setup)
       (flycheck-mode +1)
       (eldoc-mode +1)
       (tide-hl-identifier-mode +1)
       (company-mode +1)
       (setq tide-completion-detailed t
             tide-completion-ignore-case t)))
  :bind (:map tide-mode-map
              ("C-c C-f" . tide-format)
              ("C-c C-r" . tide-restart-server)
              ("C-c C-d" . tide-documentation-at-point)
              ("C-c C-e" . tide-references)
              ("C-C C-o" . tide-jsdoc-template)
              ("C-c C-p" . tide-organize-imports)))

(provide 'init-ts)

;;; init-ts.el ends here
