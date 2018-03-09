(require 'use-package)
(require 'org-mobile)
(require 'functions)

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file (serhiip--org-file-path "/todo.org")
        org-mobile-directory   (serhiip--org-file-path "/mobile")
        org-server-location    "org-server:/home/serhii/org-sync/org/"
        mobile-dir-truename    (file-truename (concat org-mobile-directory "/"))
        org-todo-keywords      '((sequence "TODO" "WAIT" "|" "DONE")
                                 (sequence "TOBUY"       "|" "DONE")))
  (add-hook
   'org-mobile-post-push-hook
   #'(lambda () (serhiip--rsync mobile-dir-truename org-server-location)))
  (add-hook
   'org-mobile-pre-pull-hook
   #'(lambda () (let ((from (concat org-server-location org-mobile-capture-file)))
             (serhiip--rsync from mobile-dir-truename))))
  (add-hook
   'org-mobile-post-pull-hook
   #'(lambda () (let ((from (concat mobile-dir-truename org-mobile-capture-file)))
             (serhiip--rsync from org-server-location))))
  :bind (("C->" . serhiip--take-notes-from-region)
         ("C-!" . serhiip--take-note-todo)))

(provide 'init-org)
