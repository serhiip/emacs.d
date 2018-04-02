;;; init-autosaves.el --- Init autosaves -*- lexical-binding: t; -*-

;;; Commentary:

;;; Persist autosaves in designated dir

;;; Code:

(setq
   backup-by-copying t
   backup-directory-alist '(("." . "~/.saves"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(provide 'init-autosaves)

;;; init-autosaves.el ends here
