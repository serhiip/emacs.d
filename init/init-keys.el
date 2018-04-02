;;; init-keys.el --- Hotkeys -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "<up>") nil)
(global-set-key (kbd "<right>") nil)
(global-set-key (kbd "<left>") nil)
(global-set-key (kbd "<down>") nil)
(global-set-key (kbd "C-<tab>") 'comment-or-uncomment-region)
(global-set-key (kbd "C-M-'") (lambda ()
                                (interactive)
                                (find-file (concat user-emacs-directory "init.el"))))

(provide 'init-keys)

;;; init-keys.el ends here
