(require 'use-package)
(require 'functions)

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file
        (convert-standard-filename "~/org/notes.org"))
  :bind (("C->" . serhiip--take-notes-from-region)
         ("C-!" . serhiip--take-note-todo)))

(provide 'init-org)
