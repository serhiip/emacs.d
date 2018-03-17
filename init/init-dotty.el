;;; init-dotty.el --- EXPERIMANTAL Dotty Flycheck support

;;; Commentary:

;;; Code:

(require 'use-package)
(use-package lsp-mode :ensure t)
(use-package lsp-ui :after lsp-mode :ensure t)
(use-package company-lsp :after lsp-ui :ensure t)
(require 'lsp-dotty)
(require 'lsp-mode)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(add-hook 'scala-mode-hook '(lambda ()
  (setq-local flycheck-disabled-checkers '(scala))
  (flycheck-mode)
  (lsp-scala-mode-enable)
))

(provide 'init-dotty)
;;; init-dotty.el ends here
