;;; init-shell.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package exec-path-from-shell :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'init-shell)

;;; init-shell.el ends here
