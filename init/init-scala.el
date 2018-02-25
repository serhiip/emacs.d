(require 'use-package)

;; (use-package ensime :ensure t :pin melpa)
;; (require 'ensime)
;; ;; disable implicit conversions higlights
;; (with-eval-after-load 'ensime
;;   (setq ensime-sem-high-faces
;;         (assq-delete-all 'implicitConversion ensime-sem-high-faces)))

(require 'scala-mode)
(setq prettify-symbols-alist scala-prettify-symbols-alist)
(prettify-symbols-mode)

(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(provide 'init-scala)
