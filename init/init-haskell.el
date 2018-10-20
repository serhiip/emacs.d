;;; init-haskell.el --- Haskell support using lsp-haskell -*- lexical-binding: t; -*-

;;; Commentary:

;;; Sets up haskell-mode and company to work well for haskell sources

;;; Code:

(require 'use-package)
(require 'company)
(require 'lsp-ui)
(require 'lsp-haskell)
(require 'flycheck)

(defun serhiip--setup-haskell-company ()
  "Make company suggest stuff from TAGS in `haskell-mode'."
  (set
   (make-local-variable 'company-backends)
   '(company-dabbrev-code company-gtags company-etags company-keywords)))

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'serhiip--setup-haskell-company)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook '(lsp-ui-sideline-enable nil))
  (add-hook 'haskell-mode-hook '(flycheck-disable-checker "haskell-ghc"))
  (setq
   haskell-tags-on-save t
   tags-revert-without-query t)
  :bind (:map haskell-mode-map
              ("C-;"     . haskell-interactive-bring)
              ("M-n"     . flycheck-next-error)
              ("M-p"     . flycheck-previous-error)
              ("C-c C-f" . haskell-mode-stylish-buffer))
  :ensure-system-package
  ((stylish-haskell . "stack install stylish-haskell")
   (hlint . "stack install hlint")))

(use-package hindent
  :ensure t
  ;; M-q   formats current block
  ;; C-M-\ formats current region
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(provide 'init-haskell)

;;; init-haskell.el ends here
