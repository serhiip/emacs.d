(set-default-font "PragmataPro")
(set-face-attribute 'default nil :height 130)

(require 'package)

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ;;("melpa-stable" . "http://stable.melpa.org/packages/")
                   ))

(setq package-list '(jade-mode monokai-theme scala-mode
                               web-mode org helm neotree
                               expand-region tern tern-auto-complete
                               use-package projectile))
(package-initialize)
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq user-emacs-directory (file-truename "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(require 'init-pretty-symbols)
(require 'init-keys)
(require 'init-emacs-settings)
(require 'init-scala)
(require 'init-shell)
(require 'init-haskell)
(require 'whitespace)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-'" . er/expand-region))

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("C-<tab> p" . projectile-find-file)
           ("C-<tab> g" . projectile-grep)))

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
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(add-hook 'js-mode-hook 'auto-complete-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t))) ;; npm install -g tern
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)))
;; hideshow
(load-library "hideshow")
(add-hook 'scala-mode-hook 'hs-minor-mode)
(add-hook 'js-mode-hook 'hs-minor-mode)
(defun display-code-lines (ov) ;; display the overlay content in a tooltip
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))
(setq hs-set-up-overlay 'display-code-lines)
