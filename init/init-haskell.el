(require 'use-package)

(use-package hindent :ensure t)
(use-package haskell-mode :ensure t)

(setq hindent-process-path (file-truename "~/.local/bin/hindent"))

(require 'hindent)
;; M-q   formats current block
;; C-M-\ formats current region
(add-hook 'haskell-mode-hook #'hindent-mode)

;; stack install stylish-haskell
(define-key haskell-mode-map (kbd "C-c C-f") 'haskell-mode-stylish-buffer)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-tags-on-save t)
  '(tags-revert-without-query t))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)

(provide 'init-haskell)
