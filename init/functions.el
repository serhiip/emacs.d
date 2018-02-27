(defun serhiip--take-note (start end)
  (interactive "r")
  (serhiip--take-note-imp (buffer-substring start end))
  (kill-region start end))

(defun serhiip--take-note-imp (lines)
  (let* ((inside-project? (and
                           (fboundp 'projectile-project-p)
                           (projectile-project-p)))
         (file (if inside-project?
                   (projectile-expand-root "notes.org")
                 (file-truename "~/org/notes.org")))
         (buff (find-file-noselect file)))
    (with-current-buffer buff
      (goto-char (point-max))
      (let ((todos (mapconcat
                    (lambda (l) (concat "* TODO " l))
                    (split-string lines "\n")
                    "\n")))
        (princ todos buff)))
    (display-buffer buff)))

(provide 'functions)
