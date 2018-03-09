(require 'subr-x)
(require 'simple)
(require 'files)
(require 'org)

(defun serhiip--rsync (from to)
  (let ((cmd (format
              "rsync -avz %s %s"
              (shell-quote-argument from)
              (shell-quote-argument to))))
    (shell-command cmd)))

(defun serhiip--org-file-path (file)
  (convert-standard-filename (concat org-directory file)))

(defun serhiip--take-notes-from-region (start end)
  (interactive "r")
  (if (not (eq start end))
      (progn
        (serhiip--take-note-impl (buffer-substring start end))
        (kill-region start end))
    (user-error "Selection must be non-empty to take a note")))

(defun serhiip--take-note-todo (note-text)
  (interactive "sTODO: ")
  (serhiip--take-note-impl note-text))

(defun serhiip--fmt-note (note)
  (concat "* TODO " (string-trim note)))

(defun serhiip--take-note-impl (lines)
  (let* ((inside-project? (and
                           (fboundp 'projectile-project-p)
                           (projectile-project-p)))

         (file (if inside-project?
                   (serhiip--org-file-path
                    (format "/%s.org" (projectile-project-name)))
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
