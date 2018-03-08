(require 'use-package)
(require 'org-mobile)
(require 'functions)

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file (convert-standard-filename "~/org/notes.org")
        org-mobile-directory   (convert-standard-filename "~/org/mobile"))
  :bind (("C->" . serhiip--take-notes-from-region)
         ("C-!" . serhiip--take-note-todo)))

(provide 'init-org)
