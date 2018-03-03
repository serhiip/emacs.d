(require 'subr-x)
(require 'org)

(defun serhiip--take-note (start end)
  (interactive "r")
  (serhiip--take-note-imp (buffer-substring start end))
  (kill-region start end))

(defun serhiip--fmt-note (note)
  (concat "* TODO " (string-trim note)))

(defun serhiip--take-note-imp (lines)
  (let* ((inside-project? (and
                           (fboundp 'projectile-project-p)
                           (projectile-project-p)))

         (file (if inside-project?
                   (projectile-expand-root "notes.org")
                 org-default-notes-file))

         (buff (find-file-noselect file))

         (todos (mapconcat
                 'serhiip--fmt-note
                 (seq-filter
                  (lambda (l) (not (string-blank-p l)))
                  (split-string lines "\n"))
                 "\n")))

    (with-current-buffer buff
      (goto-char (point-max))
      (delete-trailing-whitespace))
    (princ (concat todos "\n") buff)
    (display-buffer buff)))

(provide 'functions)
