;;; init.el --- My emacs initialization script
;;; Commentary:

;;; Has support for some langs I use, org-mode set up and
;;; customization and some custom functions

;;; Code:

(set-frame-font "PragmataPro")
(set-face-attribute 'default nil :height 130)
(server-start)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(defvar-local package-list '(org
                             use-package
                             flycheck
                             scala-mode
                             web-mode
                             helm
                             neotree
                             expand-region
                             tern
                             tern-auto-complete
                             projectile))

(package-initialize)
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(let ((default-directory (concat
                          user-emacs-directory
                          (convert-standard-filename "init"))))
  (normal-top-level-add-subdirs-to-load-path)
  (add-to-list 'load-path default-directory))

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)

(use-package use-package-ensure-system-package :ensure t)
(use-package whitespace :ensure t)

(require 'init-autosaves)
(require 'init-theme)
(require 'init-emacs-settings)
(require 'init-keys)
(require 'init-org)

(require 'init-magit)
(require 'init-projectile)
(require 'init-pretty-symbols)
(require 'init-helm)
(require 'init-scala)
(require 'init-smartparens)
(require 'init-shell)
(require 'init-haskell)
;;(require 'init-dotty)
(require 'init-elisp)
(require 'init-ts)
(require 'init-python)
(require 'init-company)
(require 'init-web-mode)
(require 'init-expand-region)
;;; init.el ends here
