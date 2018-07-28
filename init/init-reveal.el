;;; init-reveal.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; Initialization for reveal.js library via ox-reveal package

;;; Code:
(require 'use-package)

(use-package ox-reveal
  :ensure t
  :init
  (setq org-reveal-root "file:////Users/serhii/workspace/reveal.js-3.6.0"))

(provide 'init-reveal)

;;; init-reveal.el ends here
