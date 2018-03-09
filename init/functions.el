(require 'subr-x)
(require 'simple)
(require 'files)
(require 'org)
(require 'org-capture)

(defun serhiip--rsync (from to)
  (let ((cmd (format
              "rsync -avz %s %s"
              (shell-quote-argument from)
              (shell-quote-argument to))))
    (shell-command cmd)))

(defun serhiip--org-file-path (file)
  (convert-standard-filename (concat org-directory file)))

(defvar serhiip--org-current-file nil)

(defun serhiip--org-get-current-file ()
  (or serhiip--org-current-file org-default-notes-file))

(defun serhiip--take-notes-from-region (start end)
  (interactive "r")
  (if (not (eq start end))
      (progn
        (serhiip--take-note-impl (buffer-substring start end))
        (kill-region start end))
    (user-error "Selection must be non-empty to take a note")))

(defun serhiip--take-note-todo (note-text)
  (interactive "sSummary: ")
  (serhiip--take-note-impl note-text))

(defun serhiip--fmt-note (note)
  (concat "* TODO " (string-trim note)))

(defun serhiip--take-note-impl (lines)
  (let* ((inside-project? (and
                           (fboundp 'projectile-project-p)
                           (projectile-project-p)))

         (curr-filename (file-truename buffer-file-name))

         (editing-agenda-file? (member
                                curr-filename
                                (mapcar 'file-truename org-agenda-files)))

         (file (if inside-project?
                   (serhiip--org-file-path
                    (format "/%s.org" (projectile-project-name)))
                 (if editing-agenda-file?
                     curr-filename
                   org-default-notes-file))))

    (let ((serhiip--org-current-file file)) (org-capture-string lines))))

(provide 'functions)
