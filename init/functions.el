(defun serhiip--take-note ()
  (interactive)
;;  (print (concat "substracting " (number-to-string begin) " - " (number-to-string end)))
  (find-file (if (and
                  (fboundp 'projectile-project-p)
                  (projectile-project-p))
                 (projectile-expand-root "notes.org"))
             (file-truename "~/org/notes.org")))

(provide 'functions)
