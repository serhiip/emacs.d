;;; init-elisp.el --- Elisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Nothing special

;;; Code:

(require 'use-package)
(require 'flycheck)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit))

(provide 'init-elisp)

;;; init-elisp.el ends here
