;;; init-company.el -- Autocompletion -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'company-dabbrev)
(require 'company-dabbrev-code)
(require 'use-package)

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-dabbrev-downcase nil
        company-idle-delay 0.2
        company-minimum-prefix-length 3))

(provide 'init-company)

;;; init-company.el ends here
