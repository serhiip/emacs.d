;;; init-org.el --- Personal Organazier -*- lexical-binding: t; -*-

;;; Commentary:

;;; Custom hotkeys, keywords, templates, file setup and syncronization

;;; Code:

(require 'use-package)
(require 'org-mobile)
(require 'functions)

(defvar org-server-location)
(defvar mobile-dir-truename)

(defun serhiip--push-notes-to-server ()
  "Perform rsync for org files to send the to server."
  (serhiip-rsync mobile-dir-truename org-server-location))

(defun serhiip--pull-mobileorg-file ()
  "Perform rsync from server to local sync folder."
  (let ((from (concat org-server-location org-mobile-capture-file)))
    (serhiip-rsync from mobile-dir-truename)))

(defun serhiip--push-mobileorg-file ()
  "Perform rsync of mobileorg file."
  (let ((from (concat mobile-dir-truename org-mobile-capture-file)))
    (serhiip-rsync from org-server-location)))

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file (serhiip-org-file-path "/gtd.org")
        org-mobile-directory   (serhiip-org-file-path "/mobile")
        org-server-location    "org-server:/home/serhii/org-sync/org/"
        mobile-dir-truename    (file-truename (concat org-mobile-directory "/"))
        org-todo-keywords      '((sequence "TODO" "WAIT"     "|" "DONE")
                                 (sequence "TOBUY"           "|" "DONE")
                                 (sequence "TOREAD"          "|" "DONE")
                                 (sequence "BUG"             "|" "FIXED")
                                 (sequence "FEATURE" "DOING" "|" "DONE"))
        org-capture-templates  '(("t" "To Do" entry (file+headline org-default-notes-file "to do")
                                  "** TODO %i %?\n  %a")
                                 ("s" "To buy" entry (file+headline org-default-notes-file "groceries")
                                  "** TOBUY %i %?\n  %U\n  %a")
                                 ("r" "To read" entry (file+headline org-default-notes-file "to read")
                                  "** TOREAD %i %?\n  %U\n  %a")
                                 ("f" "New feature" entry (file (serhiip-org-get-current-file))
                                  "* FEATURE %i %?\n  %U\n  %a")
                                 ("b" "Bug" entry (file (serhiip-org-get-current-file))
                                  "* BUG %i %?\n  %U\n  %a"))
        org-todo-keyword-faces '(("TODO" . "red")
                                 ("DOING" . "yellow")
                                 ("FEATURE" . "firebrick")
                                 ("BUG" . "magenta")
                                 ("TOREAD" . "dark cyan")
                                 ("TOBUY" . "wheat"))
        org-ellipsis           " ⤵")
  (add-hook 'org-mobile-post-push-hook #'serhiip--push-notes-to-server)
  (add-hook 'org-mobile-pre-pull-hook #'serhiip--pull-mobileorg-file)
  (add-hook 'org-mobile-post-pull-hook #'serhiip--push-mobileorg-file)
  :bind (("C->"   . serhiip-take-notes-from-region)
         ("C-c c" . serhiip-take-note-todo)))

(use-package org-bullets
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))

(provide 'init-org)

;;; init-org.el ends here
