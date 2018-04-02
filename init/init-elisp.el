;;; init-elisp.el --- Elisp -*- lexical-binding: t; -*-

;;; Commentary:

;;; Nothing special

;;; Code:

(require 'use-package)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(provide 'init-elisp)

;;; init-elisp.el ends here
