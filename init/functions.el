(defun serhiip--take-note (start end)
  (interactive "r")
  (serhiip--take-note-imp start end))

(defun serhiip--take-note-imp (start end)
  (let* ((inside-project? (and
                           (fboundp 'projectile-project-p)
                           (projectile-project-p)))
         (file (if inside-project?
                   (projectile-expand-root "notes.org")
                 (file-truename "~/org/notes.org")))
         (buff (find-file-noselect file)))
    (with-current-buffer buff
      (goto-char (point-max))
      (princ "* TODO " buff))
    (append-to-buffer buff start end)
    (kill-region start end)
    (display-buffer buff)))

(provide 'functions)
