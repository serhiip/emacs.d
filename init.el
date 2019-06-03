;;; init.el --- My emacs configuration
;;; Commentary:

;;; Has support for some langs I use, org-mode set up and
;;; customization and some custom functions

;;; Code:

(require 'files)

;;(set-frame-font "PragmataPro Mono Liga")
;;(set-face-attribute 'default nil :height 130)

(prog1 "Load leaf.el"
  (let ((default-directory (concat
                          user-emacs-directory
                          (convert-standard-filename "init"))))
    (normal-top-level-add-subdirs-to-load-path)
    (add-to-list 'load-path default-directory))
  (require 'leaf)
  (leaf leaf
    :doc "Initialize leaf dependent packages"
    :config
    (leaf package
      :custom ((package-archives . '(("org"   . "https://orgmode.org/elpa/")
                                     ("melpa" . "https://melpa.org/packages/")
                                     ("gnu"   . "https://elpa.gnu.org/packages/"))))
      :config
      (package-initialize))))

(leaf custom
  :config
  (add-to-list 'exec-path "/usr/local/bin")
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (column-number-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  (global-hl-line-mode 1)
  :setq
  `((confirm-nonexistent-file-or-buffer . nil)
    (completion-ignore-case . t)
    (read-file-name-completion-ignore-case . t)
    (inhibit-startup-screen . t)
    (whitespace-line-column . 120)
    (indent-tabs-mode . nil)
    (backup-by-copying . t)
    (backup-directory-alist . '(("." . "~/.saves")))
    (delete-old-versions . t)
    (kept-new-versions . 6)
    (kept-old-versions . 2)
    (version-control . t))
  :custom
  (custom-enabled-themes . '(tsdh-dark))
  :bind (("<up>" . nil) ("<right>" . nil) ("<left>" . nil) ("<down>" . nil)))

(leaf whitespace
  :setq
  `((whitespace-line-column . 120))
  :require t
  :bind (("C-x w" . whitespace-mode)))

(leaf monokai-theme
  :config
  :after package
  :ensure t
  :disable t
  (load-theme 'monokai t))

(leaf winner
  :config
  (winner-mode 1))

(leaf prog-mode
  :require t)

(leaf flycheck
  :after prog-mode
  :ensure t
  :setq `((flycheck-emacs-lisp-load-path . 'inherit))
  :hook (prog-mode-hook . flycheck-mode))

(leaf company
  :after prog-mode
  :ensure t
  :hook (prog-mode-hook . company-mode))

(leaf org
  :ensure t
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)))

(leaf newcomment
  :bind (("C-<tab>" . comment-or-uncomment-region)))

(leaf rainbow-delimiters
  :ensure t
  :after elisp-mode
  :hook (emacs-lisp-mode-hook . rainbow-delimiters-mode))

(leaf smartparens
  :ensure t
  :after elisp-mode
  :hook (emacs-lisp-mode-hook . smartparens-strict-mode))

(leaf magit
  :ensure t
  :setq `((vc-handled-backends . '()))
  :bind (("C-x g" . magit-status)))


(org-todo-list)
;;; init.el ends here
