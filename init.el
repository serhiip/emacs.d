(server-start)
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

;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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


(defun center-frame (frame)
  (let*
    ((monitor (frame-monitor-attributes frame))
      (half-monitor-width (/ (nth 3 (assoc 'workarea monitor)) 2))
      (half-frame-width (/ (frame-pixel-width frame) 2))
      (half-monitor-height (/ (nth 4 (assoc 'workarea monitor)) 2))
      (half-frame-height (/ (frame-pixel-height frame) 2)))

    (set-frame-position frame
      (- half-monitor-width half-frame-width)
      (- half-monitor-height half-frame-height))))

(defun helm-display-buffer-in-own-frame-new (buffer &optional resume)
  "Display Helm buffer BUFFER in a separate frame.

Function suitable for `helm-display-function',
`helm-completion-in-region-display-function' and/or
`helm-show-completion-default-display-function'.

See `helm-display-buffer-height' and `helm-display-buffer-width'
to configure frame size.

Note that this feature is available only with emacs-25+.
Note also it is not working properly in helm nested session with emacs
version < emacs-28."
  (cl-assert (and (fboundp 'window-absolute-pixel-edges)
		  (fboundp 'frame-geometry))
	     nil "Helm buffer in own frame is only available starting at emacs-25+")
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((pos (window-absolute-pixel-position))
	   (half-screen-size (/ (display-pixel-height x-display-name) 2))
	   (frame-info (frame-geometry))
	   (prmt-size (length helm--prompt))
	   (line-height (frame-char-height))
	   tab-bar-mode
	   (new-frame-alist
	     (if resume
		 (buffer-local-value 'helm--last-frame-parameters
				     (get-buffer buffer))
	       `((width . ,helm-display-buffer-width)
		 (height . ,helm-display-buffer-height)
		 (tool-bar-lines . 0)
		 (left . ,(- (car pos)
			     (* (frame-char-width)
				(if (< (- (point) (pos-bol)) prmt-size)
				    (- (point) (pos-bol))
				  prmt-size))))
		 ;; Try to put frame at the best possible place.
		 ;; Frame should be below point if enough
		 ;; place, otherwise above point and
		 ;; current line should not be hidden
		 ;; by helm frame.
		 (top . ,(if (> (cdr pos) half-screen-size)
			     ;; Above point
			     (- (cdr pos)
				;; add 2 lines to make sure there is always a gap
				(* (+ helm-display-buffer-height 2) line-height)
				;; account for title bar height too
				(cddr (assq 'title-bar-size frame-info)))
			   ;; Below point
			   (+ (cdr pos) line-height)))
		 (title . "Helm")
		 (undecorated . ,helm-use-undecorated-frame-option)
		 (background-color . ,(or helm-frame-background-color
					  (face-attribute 'default :background)))
		 (foreground-color . ,(or helm-frame-foreground-color
					  (face-attribute 'default :foreground)))
		 (alpha . ,(or helm-frame-alpha 100))
		 (font . ,(assoc-default 'font (frame-parameters)))
		 (vertical-scroll-bars . nil)
		 (menu-bar-lines . 0)
		 (fullscreen . nil)
		 (visibility . ,(null helm-display-buffer-reuse-frame))
		 (minibuffer . t))))
	   display-buffer-alist)
      ;; Display minibuffer above or below only in initial session,
      ;; not on a session triggered by action, this way if user have
      ;; toggled minibuffer and header-line manually she keeps this
      ;; setting in next action.
      (unless (or helm--executing-helm-action resume)
	;; Add the hook inconditionally, if
	;; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
	;; will have anyway no effect so no need to remove the hook.
	(add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
	(with-helm-buffer
	  (setq-local helm-echo-input-in-header-line
		      (not (> (cdr pos) half-screen-size)))))
      (helm-display-buffer-popup-frame buffer new-frame-alist)
      (center-frame helm-popup-frame)
      ;; When frame size have been modified manually by user restore
      ;; it to default value unless resuming or not using
      ;; `helm-display-buffer-reuse-frame'.
      ;; This have to be done AFTER raising the frame otherwise
      ;; minibuffer visibility is lost until next session.
      (unless (or resume (not helm-display-buffer-reuse-frame))
	(set-frame-size helm-popup-frame
			helm-display-buffer-width
			helm-display-buffer-height)
	(center-frame frame)))
    (helm-log-run-hook "helm-display-buffer-in-own-frame" 'helm-window-configuration-hook)))

(use-package helm
  :ensure t
  :init
  (require 'esh-mode)
  (helm-mode 1)
  (setq helm-autoresize-mode t
	helm-display-buffer-default-height 20
	helm-display-buffer-default-width nil
	helm-display-header-line nil
	helm-actions-inherit-frame-settings nil
	helm-display-buffer-width 80
	helm-display-buffer-height 50
	helm-display-function 'helm-display-buffer-in-own-frame-new)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-b" . helm-recentf)
	 ("C-c C-r" . helm-resume)
	 ("C-x p g" . helm-grep-do-git-grep)
	 :map eshell-mode-map
	 ("C-r" . helm-eshell-history)))

(use-package helm-swoop
  :ensure t
  :bind
  ("C-s" . helm-swoop)
  :init
  (setq helm-swoop-split-direction 'split-window-horizontally))

(use-package helm-flymake
  :ensure t)

(use-package helm-project
  :ensure t)

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
  (load-theme 'gruvbox-dark-hard nil))

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
  :ensure t
  :config
  ;; fixes an issue with sideline https://github.com/emacs-lsp/lsp-ui/issues/285#issuecomment-493092398
  (custom-set-faces
   '(markdown-code-face ((t (:inherit consolas))))))

(use-package denote
  :config
  (setq denote-infer-keywords t
        denote-directory (expand-file-name "~/Documents/notes/")
        denote-known-keywords '("sql" "meeting" "todo" "daily"))
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

(use-package gptel
  :ensure t
  :bind
  (("C-c !" . gptel-menu)
   ("C-c @" . gptel))
  :init
  (setq
    gptel-model 'deepseek-coder-v2:16b
    gptel-backend (gptel-make-ollama "Ollama"
		    :host "localhost:11434"
		    :stream nil
		    :models '(deepseek-coder-v2:16b codegeex4:9b codellama:70b))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-Iosvkem))
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     default))
 '(package-selected-packages
   '(ace-jump-mode all-the-icons apheleia code-cells company company-box
		   darcula-theme denote eglot ellama gptel
		   haskell-mode helm helm-flymake helm-icons
		   helm-project helm-swoop hide-mode-line
		   highlight-indentation magit markdown-mode
		   mode-line-bell mojo moody newcomment nix-mode
		   prog-mode python-ts-mode pyvenv sbt-mode scala-mode
		   scala-ts-mode treemacs typescript-mode yaml-mode
		   yasnippet yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit consolas)))))
(put 'downcase-region 'disabled nil)
