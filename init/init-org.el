;;; init-org.el --- Personal Organazier -*- lexical-binding: t; -*-

;;; Commentary:

;;; Custom hotkeys, keywords, templates, file setup and synchronisation

;;; Code:

(require 'use-package)
(require 'functions)
(require 'org-id)

(use-package org
  :ensure t
  :init
  (setq org-default-notes-file (serhiip-org-file-path "/gtd.org")
        org-todo-keywords      '((sequence "TODO" "WAIT"     "|" "DONE")
                                 (sequence "TOBUY"           "|" "DONE")
                                 (sequence "TOREAD"          "|" "DONE")
                                 (sequence "BUG"             "|" "FIXED")
                                 (sequence "FEATURE" "DOING" "|" "DONE")
                                 (sequence "MEMO"            "|" "DONE"))
        org-capture-templates  '(("t" "To Do" entry
                                  (file+headline org-default-notes-file "to do")
                                  "** TODO %i %?\n  %a")
                                 ("s" "To Buy" entry
                                  (file+headline org-default-notes-file "to buy")
                                  "** TOBUY %i %?\n  %U\n  %a")
                                 ("m" "To Memo" entry
                                  (file+headline org-default-notes-file "to memo")
                                  "** MEMO %i %?\n  %a")
                                 ("r" "To read" entry
                                  (file+headline org-default-notes-file "to read")
                                  "** TOREAD %i %?\n  %U\n  %a")
                                 ("f" "New feature" entry
                                  (file (lambda () (serhiip-org-get-current-file)))
                                  "* FEATURE %i %?\n  %U\n  %a")
                                 ("b" "Bug" entry
                                  (file (lambda () (serhiip-org-get-current-file)))
                                  "* BUG %i %?\n  %U\n  %a"))
        org-todo-keyword-faces '(("TODO"    . "red")
                                 ("MEMO"    . "firebrick")
                                 ("DOING"   . "yellow")
                                 ("FEATURE" . "firebrick")
                                 ("BUG"     . "magenta")
                                 ("TOREAD"  . "dark cyan")
                                 ("TOBUY"   . "wheat"))
        org-ellipsis           " ⤵")
  :bind (("C->"   . serhiip-take-notes-from-region)
         ("C-c c" . serhiip-take-note-todo)))

(use-package org-bullets
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))

(provide 'init-org)

;;; init-org.el ends here
