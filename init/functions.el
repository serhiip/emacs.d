;;; functions.el --- Common functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; A set of various functions used across the project

;;; Code:

(require 'subr-x)
(require 'simple)
(require 'files)
(require 'org)
(require 'org-capture)
(require 'leaf)

(defvar serhiip--org-current-file nil)

(defun serhiip-take-notes-from-region (start end)
  "Capture notes from region selected.

Each line in selection marked with START and END becomes a note
in resulting org file.  Triggers `org-capture' to capture
details"
  (interactive "r")
  (let ((buff (current-buffer)))
    (when
        (unless (= start end)
          (serhiip--take-note-impl (buffer-substring start end))
          (with-current-buffer buff (kill-region start end))
          nil)
      (user-error "Selection must be non-empty to take a note"))))

(defun serhiip-take-note-todo (note-text)
  "Capture a note by asking the note text in minibuffer.

NOTE-TEXT is text captured from minibuffer"
  (interactive "sSummary: ")
  (serhiip--take-note-impl note-text))

;; from bbatsov/prelude
(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun serhiip-org-file-path (file)
  "Get full path to org file named FILE."
  (convert-standard-filename (concat org-directory file)))

(defun serhiip-org-get-current-file ()
  "Get org file for project currently opened."
  (or serhiip--org-current-file org-default-notes-file))

(defun serhiip--take-note-impl (lines)
  "Take not from rectangle or by asking.  Either is represented as LINES."
  (let* ((inside-project-p (and
                            (fboundp 'projectile-project-p)
                            (projectile-project-p)))
         (editing-agenda-p (member
                            buffer-file-truename
                            (mapcar 'file-truename org-agenda-files)))
         (file (if inside-project-p
                   (serhiip-org-file-path
                    (format "/%s.org" (projectile-project-name)))
                 (if editing-agenda-p
                     buffer-file-truename
                   org-default-notes-file))))
    (if (file-exists-p file)
        (serhiip-set-org-file-capture file lines)
      (if (yes-or-no-p (format "File %s is missing. Create?" file))
          (progn
            (write-region "" nil file)
            (serhiip-set-org-file-capture file lines))))))

(defun serhiip-set-org-file-capture (file lines)
  "Set current org FILE name and capture LINES provided."
  (let ((serhiip--org-current-file file))
    (org-capture-string lines)))

(defun serhiip-add-elisp-docs ()
  "Add docs for current elisp buffer."
  (interactive)
  (let ((filename (car (last (split-string buffer-file-name "/")))))
    (goto-char (point-max))
    (insert (format "\n;;; %s ends here" filename))
    (goto-char (point-min))
    (insert (format ";;; %s --- " filename))
    (let ((position (point)))
      (insert "-*- lexical-binding: t; -*-\n\n;;; Commentary:\n\n;;; Code:\n\n")
      (goto-char position))))

(provide 'functions)

;;; functions.el ends here
