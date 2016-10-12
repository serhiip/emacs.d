(require 'package)

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ;;("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq package-list '(jade-mode monokai-theme scala-mode
                               web-mode org helm neotree
			       expand-region tern tern-auto-complete
			       use-package))
(package-initialize)
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

(use-package ensime :ensure t :pin melpa-stable)

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

;; start in fullscreen
(set-frame-parameter nil 'fullscreen 'fullboth)

(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(require 'org)
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; disable implicit conversions higlights
(with-eval-after-load 'ensime
  (setq ensime-sem-high-faces
        (assq-delete-all 'implicitConversion ensime-sem-high-faces)))

(require 'scala-mode)
(setq prettify-symbols-alist scala-prettify-symbols-alist)
(prettify-symbols-mode)

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'monokai t)
(transient-mark-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)


(setq
 confirm-nonexistent-file-or-buffer nil
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 inhibit-startup-screen t
 js-indent-level 4)

(setq-default indent-tabs-mode nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-<tab> <tab>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<tab> h") 'hs-hide-all)
(global-set-key (kbd "C-<tab> s") 'hs-show-all)
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-;") 'yas-expand)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables by custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-sbt-command "/usr/local/Cellar/sbt/0.13.8/bin/sbt")
 '(org-agenda-files (quote ("~/org/notes.org")))
 '(package-selected-packages
   (quote
    (web-mode use-package tern-auto-complete scala-mode2 php-auto-yasnippets neotree monokai-theme lua-mode jade-mode helm-descbinds flymake-php expand-region ac-octave ac-js2)))
 '(sbt:program-name "/usr/local/bin/sbt")
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy 0)
 '(scala-indent:indent-value-expression t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
