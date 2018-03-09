(require 'use-package)
(require 'org-mobile)
(require 'functions)

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file (serhiip--org-file-path "/gtd.org")
        org-mobile-directory   (serhiip--org-file-path "/mobile")
        org-server-location    "org-server:/home/serhii/org-sync/org/"
        mobile-dir-truename    (file-truename (concat org-mobile-directory "/"))
        org-todo-keywords      '((sequence "TODO" "WAIT"     "|" "DONE")
                                 (sequence "TOBUY"           "|" "DONE")
                                 (sequence "TOREAD"          "|" "DONE")
                                 (sequence "BUG"             "|" "FIXED")
                                 (sequence "FEATURE" "DOING" "|" "DONE"))
        org-capture-templates  '(("t" "Add to do list entry" entry (file+headline org-default-notes-file "to do")
                                  "** TODO %i %?\n  %a")
                                 ("s" "Add to groceries list" entry (file+headline org-default-notes-file "groceries")
                                  "** TOBUY %i %?\n  Added on %U\n  %a")
                                 ("r" "Add to reading list" entry (file+headline org-default-notes-file "to read")
                                  "** TOREAD %i %?\n  Added on %U\n  %a")
                                 ("f" "New feature" entry (file (serhiip--org-get-current-file))
                                  "* FEATURE %i %?\n  Added on %U\n  %a")
                                 ("b" "Bug" entry (file (serhiip--org-get-current-file))
                                  "* BUG %i %?\n  Added on %U\n  %a")))
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
  :bind (("C->"   . serhiip--take-notes-from-region)
         ("C-c c" . serhiip--take-note-todo)))

(provide 'init-org)
