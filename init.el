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
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ;;("melpa-stable" . "http://stable.melpa.org/packages/")
                   ))

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

(require 'flycheck)
(require 'company-dabbrev)
(require 'company-dabbrev-code)
(require 'yasnippet)

(let ((default-directory (concat
                          user-emacs-directory
                          (convert-standard-filename "init"))))
  (normal-top-level-add-subdirs-to-load-path)
  (add-to-list 'load-path default-directory)
  (add-to-list 'flycheck-emacs-lisp-load-path default-directory))

(use-package use-package-ensure-system-package :ensure t)

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

(require 'whitespace)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-'" . er/expand-region))

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case t
   company-dabbrev-code-ignore-case t
   company-dabbrev-downcase nil
   company-idle-delay 0.2
   company-minimum-prefix-length 3)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;;; init.el ends here
