(require 'use-package)

(use-package flycheck-pycheckers
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
  :ensure-system-package (pylint . "pip install pylint"))

(provide 'init-python)
