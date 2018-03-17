;;; init-expand-region.el --- initialization

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-'" . er/expand-region))

(provide 'init-expand-region)
;;; init-expand-region.el ends here

