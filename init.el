(set-default-font "PragmataPro")
(set-face-attribute 'default nil :height 130)
(let ((server-name "gui"))
  (server-start))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ;;("melpa-stable" . "http://stable.melpa.org/packages/")
                   ))

(setq package-list '(org
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

(use-package use-package-ensure-system-package :ensure t)

(require 'init-theme)
(require 'init-projectile)
(require 'init-pretty-symbols)
(require 'init-keys)
(require 'init-emacs-settings)
(require 'init-scala)
(require 'init-shell)
(require 'init-haskell)
(require 'init-dotty)
(require 'init-log-edit)
(require 'functions)

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
  ;;  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  ;;(define-key company-active-map [tab] nil)
  ;;(define-key company-active-map (kbd "TAB") nil)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))


(require 'org)
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;; js-mode
;; (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;; (setq js2-highlight-level 3)
;; (add-hook 'js-mode-hook 'auto-complete-mode)
;; (add-hook 'js-mode-hook (lambda () (tern-mode t))) ;; npm install -g tern
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))
;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; (add-hook 'html-mode-hook
;;           (lambda()
;;             (setq sgml-basic-offset 2)))
