(require 'simple)
(require 'files)
(require 'use-package)
(require 'org-mobile)
(require 'functions)

(defun rsync (from to)
  (let ((cmd (format
              "rsync -avz %s %s"
              (shell-quote-argument from)
              (shell-quote-argument to))))
    (shell-command cmd)))

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file (convert-standard-filename "~/org/todo.org")
        org-mobile-directory   (convert-standard-filename "~/org/mobile")
        org-server-location    "org-server:/home/serhii/org-sync/org/"
        mobile-dir-truename    (file-truename (concat org-mobile-directory "/"))
        org-todo-keywords      '((sequence "TODO" "WAIT" "|" "DONE")
                                 (sequence "TOBUY"       "|" "DONE")))
  (add-hook
   'org-mobile-post-push-hook
   #'(lambda () (rsync mobile-dir-truename org-server-location)))
  (add-hook
   'org-mobile-pre-pull-hook
   #'(lambda () (let ((from (concat org-server-location org-mobile-capture-file)))
             (rsync from mobile-dir-truename))))
  (add-hook
   'org-mobile-post-pull-hook
   #'(lambda () (let ((from (concat mobile-dir-truename org-mobile-capture-file)))
             (rsync from org-server-location))))
  :bind (("C->" . serhiip--take-notes-from-region)
         ("C-!" . serhiip--take-note-todo)))

(provide 'init-org)
