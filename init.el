(require 'package)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
(setq visible-bell t)
(set-frame-font "-*-PragmataPro Mono-normal-normal-normal-*-15-*-*-*-p-0-iso10646-1")
(setq default-frame-alist '((font . "-*-PragmataPro Mono-normal-normal-normal-*-15-*-*-*-p-0-iso10646-1") (width . 300) (height . 300)))

(setenv "JAVA_HOME" "/nix/store/5f79idj0y7i9qcsp3w1w3ir7nk8280nr-zulu17.34.19-ca-jdk-17.0.3/zulu-17.jdk/Contents/Home")
(setq my-backups-dir "~/.emacs_backups/")
(setq my-autosave-dir "~/.emacs_autosave/")
(unless (file-exists-p my-backups-dir)
  (make-directory my-backups-dir t))
(unless (file-exists-p my-autosave-dir)
  (make-directory my-autosave-dir t))
(setq auto-save-default nil)
(setq auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))
(setq backup-directory-alist `((,my-backups-dir)))
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
(setq show-trailing-whitespace t)
(setq kill-whole-line t)
(setq use-package-verbose t)

(setq exec-path (append exec-path (mapcar (lambda (in) (file-name-concat (getenv "HOME") in)) '(".local/bin" ".nix-profile/bin" ".cargo/bin/" "node_modules/.bin"))))

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(let* ((my-lisp-dir "~/.emacs.d/init/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(require 'use-package)
(require 'scala3)
(require 'pragmatapro-lig)

(use-package emacs
  :init
  (setq use-package-always-defer t
	use-package-always-ensure t
	debug-on-error nil
	ns-use-native-fullscreen nil)

  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust")
	  (scala "https://github.com/tree-sitter/tree-sitter-scala")))

  (defun add-pragmatapro-prettify-symbols-alist ()
    (setq prettify-symbols-alist pragmatapro-prettify-symbols-alist))

  (defun setup-compose-predicate ()
    (setq prettify-symbols-compose-predicate
	  (defun my-prettify-symbols-default-compose-p (start end _match)
	    "Same as `prettify-symbols-default-compose-p', except compose symbols in comments as well."
	    (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
				     '(?w ?_) '(?. ?\\)))
		   (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
				     '(?w ?_) '(?. ?\\))))
	      (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
		       (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
		       (nth 3 (syntax-ppss))))))))

  (defun prettify-hook ()
    (add-pragmatapro-prettify-symbols-alist)
    (setup-compose-predicate))

  (global-prettify-symbols-mode +1)

  :hook
  ((prog-mode . pragmatapro-lig-mode))

  :bind
  (("<f8>" . pragmatapro-lig-mode)))


(use-package company
  :hook
  ((after-init-hook . global-company-mode)
   (prog-mode       . company-mode)))

;; (use-package scala-mode
;;   :interpreter ("scala" . scala-mode)
;;   :config
;;   (advice-add #'scala-mode-map:add-self-insert-hooks :around #'with-disable-for-scala3)
;;   :hook
;;   (scala-mode . disable-scala-indent)
;;   (scala-mode . eglot-ensure)
;;   (scala-mode . company-mode)
;;   (scala-mode . display-line-numbers-mode))

;; https://github.com/KaranAhlawat/scala-ts-mode

(use-package scala-mode
  :mode "\\.scala\\'"
  :hook
  (scala-mode . disable-scala-indent)
  (scala-mode . eglot-ensure)
  (scala-mode . company-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package typescript-mode)

(use-package eglot
  :pin melpa-stable
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode) . ("ts-server" "--stdio"))))
  (add-to-list 'eglot-server-programs
	       '(scala-mode . ("metals-emacs")))
  (add-to-list 'eglot-server-programs
	       `(python-ts-mode . ,(eglot-alternatives
				    '(("/Users/serhii/workspace/tensor-flow-certification/first-project/bin/pylsp")
				      ("pylsp")))))
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-ignored-server-capabilities '(:executeCommandProvider))
  :bind
  (
   (:map eglot-mode-map ("C-c l a" . eglot-code-actions))
   (:map eglot-mode-map ("C-c l =" . eglot-format-buffer))))

(use-package all-the-icons
  ;; M-x all-the-icons-install-fonts
  )

(use-package helm-icons
  :ensure t)

(use-package flymake
  :bind
  (:map flymake-mode-map ("M-n" . flymake-goto-next-error))
  (:map flymake-mode-map ("M-p" . flymake-goto-prev-error))
  (:map flymake-mode-map ("C-c l E" . flymake-show-project-diagnostics))
  (:map flymake-mode-map ("C-c l e" . flymake-show-buffer-diagnostics)))

;;(use-package all-the-icons-ivy
;;  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; (use-package ivy
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (setq search-default-mode #'char-fold-to-regexp)
;;   (ivy-mode))

(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  (setq helm-autoresize-mode t
;;	helm-display-buffer-default-height 10
;;        helm-display-buffer-default-width nil
	helm-display-header-line nil
	helm-actions-inherit-frame-settings nil
;;	helm-always-two-windows t
	helm-display-function 'helm-display-buffer-in-own-frame)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-b" . helm-recentf)
	 ("C-s" . helm-occur)
	 ("C-c C-r" . helm-resume)
	 ("C-x p g" . helm-grep-do-git-grep)))

(use-package avy
  :ensure t
  :init
  (setq avy-background t)
  :bind
  ("C-'" . avy-goto-char-timer)
  ("C-!" . avy-goto-line))

;; (use-package ace-jump-mode
;;   :ensure t
;;   :bind ("C-'" . ace-jump-mode))

;;(use-package counsel
;;  :bind (("C-s" . swiper)
;;	 ("C-c C-r" . ivy-resume)
;;	 ("M-x" . counsel-M-x)
;;	 ("C-x C-f" . counsel-find-file)
;;	 ("C-x C-b" . counsel-recentf)
;;	 ("M-g i" . counsel-imenu)
;;	 ("C-c b" . counsel-bookmark)
;;	 (:map minibuffer-local-map
;;	       ("C-r" . counsel-minibuffer-history))))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package whitespace
  :bind
  (("C-x w"   . whitespace-mode)
   ("C-x C-w" . whitespace-cleanup))
  :config
  (setq whitespace-line-column 140))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  (("C-M-s" . yas-insert-snippet)))

(use-package yasnippet-snippets)

(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#44475a")
  (set-face-background 'highlight-indentation-current-column-face "#44475a")
  :hook
  (scala-mode . highlight-indentation-current-column-mode)
  (yaml-mode . highlight-indentation-current-column-mode))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))

(use-package mode-line-bell
  :init
  (mode-line-bell-mode))

(use-package treemacs
  :bind
  (("<f9>" . treemacs)))

;; rustup component add rust-analyzer
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . company-mode))

(use-package python-ts-mode
  :mode "\\.py\\'"
  :ensure nil
  :hook
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . company-mode)
  :config
  (setq major-mode-remap-alist
	'((python-mode . python-ts-mode))))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  (setq pyvenv-post-activate-hooks
	(list (lambda ()
		(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		(setq python-shell-interpreter "python3")))))

;; tree-sitter
;; (use-package rust-ts-mode
;;   :mode "\\.rs\\'"
;;   :hook
;;   (rust-ts-mode . eglot-ensure))

;; npm i -g typescript-language-server; npm i -g typescript
(use-package typescript-ts-mode
  :mode "\\.tsx?\\'"
  :custom
  (js-indent-level 2)
  (js-jsx-indent-level 2)
  :hook
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . company-mode))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  ;; fixes an issue with sideline https://github.com/emacs-lsp/lsp-ui/issues/285#issuecomment-493092398
  (custom-set-faces
   '(markdown-code-face ((t (:inherit consolas))))))

(use-package denote
  :config
  (setq denote-infer-keywords t)
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("sql" "meeting" "todo" "daily"))
  :hook
  (dired-mode . denote-dired-mode-in-directories))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package ellama
  :bind (("C-x j" . ellama-transient-main-menu))
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5-coder:1.5b" :embedding-model "qwen2.5-coder:1.5b")))

;; (eval-and-compile
;;   (defun ess-mojo-hl-load-path ()
;;     (file-name-concat (getenv "HOME") "emacs.d" "init" "mojo-hl")))

;; (require 'mojo-mode)

;; (use-package mojo-mode
;;   :load-path (lambda () (ess-mojo-hl-load-path))
;;   :hook
;;   (mojo-mode . eglot-ensure))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package wildcharm-theme
  :init
  (load-theme 'wildcharm t)
  :custom-face
  (org-level-1 ((t (:height 1.6))))
  (org-level-2 ((t (:height 1.4))))
  (org-level-3 ((t (:height 1.2))))
  (font-lock-comment-face ((t (:slant italic)))))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom") 
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package hide-mode-line
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-Iosvkem))
 '(custom-safe-themes
   '("b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" default))
 '(package-selected-packages
   '(helm-icons ace-jump-mode helm hide-mode-line moody code-cells nix-mode eglot apheleia mojo pyvenv python-ts-mode typescript-mode ellama denote yaml-mode markdown-mode haskell-mode scala-ts-mode treemacs prog-mode mode-line-bell highlight-indentation yasnippet-snippets yasnippet newcomment company-box all-the-icons company darcula-theme magit scala-mode sbt-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit consolas)))))
(put 'downcase-region 'disabled nil)
