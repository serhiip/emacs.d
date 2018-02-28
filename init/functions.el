(require 'subr-x)

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
                 (file-truename "~/org/notes.org")))

         (buff (find-file-noselect file))

         (todos (seq-reduce
                 (lambda (acc line)
                   (if (not (string-blank-p line))
                       (concat acc "\n"
                               (serhiip--fmt-note line))
                     acc))
                 (split-string lines "\n")
                 "")))

    (with-current-buffer buff (goto-char (point-max)))
    (princ todos buff)
    (display-buffer buff)))

(provide 'functions)
