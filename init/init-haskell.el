(require 'use-package)
(require 'company)

(use-package haskell-interactive-mode)
(use-package haskell-process)

(defun serhiip--setup-haskell-company ()
  "Make company suggest stuff from TAGS in `haskell-mode'."
  (set
   (make-local-variable 'company-backends)
   '(company-dabbrev-code company-gtags company-etags company-keywords)))

(use-package haskell-mode
  :ensure t
  :init (setq haskell-process-suggest-remove-import-lines t
              haskell-process-auto-import-loaded-modules t
              haskell-process-log t
              haskell-tags-on-save t
              tags-revert-without-query t)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'serhiip--setup-haskell-company)
  :bind (:map haskell-mode-map
              ("C-`"     . haskell-interactive-bring)
              ("C-c C-l" . haskell-process-load-or-reload)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("M-."     . haskell-mode-jump-to-def-or-tag)
              ("C-c C-f" . haskell-mode-stylish-buffer))
  :ensure-system-package (stylish-haskell . "stack install stylish-haskell"))

(use-package hindent
  :ensure t
  ;; M-q   formats current block
  ;; C-M-\ formats current region
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package flycheck-haskell
  :ensure t
  :init (add-hook 'flycheck-mode-hook  #'flycheck-haskell-setup))

(provide 'init-haskell)

;;; init-haskell.el ends here
