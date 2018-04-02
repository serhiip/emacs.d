;;; init-python.el --- Python -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'flycheck-pycheckers)

(use-package flycheck-pycheckers
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
  :ensure-system-package (pylint . "pip install pylint"))

(provide 'init-python)

;;; init-python.el ends here
