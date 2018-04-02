;;; init-helm.el --- Better suggestions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Also changes soem bindings in eshell

;;; Code:

(require 'use-package)
(require 'eshell)

(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  :bind (("M-x"                    . helm-M-x)
         ("C-s"                    . helm-occur)
         ("C-x C-f"                . helm-find-files)
         :map global-map
         ([remap jump-to-register] . helm-register)
         ([remap list-buffers]     . 'helm-mini)))

(provide 'init-helm)

;;; init-helm.el ends here
