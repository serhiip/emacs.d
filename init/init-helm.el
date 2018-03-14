(require 'use-package)

(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)))
  :bind (("M-x"                    . helm-M-x)
         ("C-s"                    . helm-occur)
         ("C-x C-f"                . helm-find-files)
         :map global-map
         ([remap jump-to-register] . helm-register)
         ([remap list-buffers]     . 'helm-mini)))

(provide 'init-helm)
