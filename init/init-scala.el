(require 'use-package)

(use-package ensime :ensure t :pin melpa)
(require 'ensime)
;; disable implicit conversions higlights
(with-eval-after-load 'ensime
  (setq ensime-sem-high-faces
        (assq-delete-all 'implicitConversion ensime-sem-high-faces)))

(require 'scala-mode)
(setq prettify-symbols-alist scala-prettify-symbols-alist)
(prettify-symbols-mode)
(defun add-scala-prettify-symbols-alist ()
  (dolist (alias scala-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

(use-package scala-mode
  :ensure t
  :init
  (require 'scala-mode-prettify-symbols)
  (add-hook 'scala-mode-hook #'smartparens-strict-mode)
  (setq prettify-symbols-alist scala-prettify-symbols-alist
        flycheck-scalastylerc "~/scalastyle_config.xml")
  (prettify-symbols-mode))

(provide 'init-scala)
