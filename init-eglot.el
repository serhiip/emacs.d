(require 'package)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(set-frame-font "-*-PragmataPro Mono-normal-normal-normal-*-13-*-*-*-p-0-iso10646-1")
(setq default-frame-alist '((font . "-*-PragmataPro Mono-normal-normal-normal-*-13-*-*-*-p-0-iso10646-1") (width . 300) (height . 300)))

(setq my-backups-dir "~/.emacs_backups/")
(setq my-autosave-dir "~/.emacs_autosave/")
(unless (file-exists-p my-backups-dir)
  (make-directory my-backups-dir t))
(unless (file-exists-p my-autosave-dir)
  (make-directory my-autosave-dir t))
(setq auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))
(setq backup-directory-alist `(("." . ,my-backups-dir)))
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
(setq show-trailing-whitespace t)
(setq kill-whole-line t)

(setq exec-path (append exec-path (mapcar (lambda (in) (file-name-concat (getenv "HOME") in)) '(".local/bin" ".nix-profile/bin" ".cargo/bin/" "node_modules/.bin"))))
;;(setq treesit-extra-load-path (append treesit-extra-load-path '("/Users/.emacs.d/tree-sitter")))

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install use-package if not already installed
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
      debug-on-error nil)

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

(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :config
  (advice-add #'scala-mode-map:add-self-insert-hooks :around #'with-disable-for-scala3)
  :hook
  (scala-mode . disable-scala-indent)
  (scala-mode . eglot-ensure)
  (scala-mode . company-mode)
  (scala-mode . display-line-numbers-mode))

;; https://github.com/KaranAhlawat/scala-ts-mode
;; (use-package scala-ts-mode
;;   :mode "\\.scala\\'"
;;   :hook
;;   (scala-ts-mode . disable-scala-indent)
;;   (scala-ts-mode . eglot-ensure))

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

(use-package eglot
  :pin melpa-stable
  :config
  (add-to-list 'eglot-server-programs
	       '(scala-mode . ("metals-emacs")))
  (setq eglot-confirm-server-initiated-edits nil)
  :bind
  (
   (:map eglot-mode-map ("C-c l a" . eglot-code-actions))
   (:map eglot-mode-map ("C-c l =" . eglot-format-buffer))))

(use-package all-the-icons
  ;; M-x all-the-icons-install-fonts
  )

(use-package flymake
  :bind
  (:map flymake-mode-map ("M-n" . flymake-goto-next-error))
  (:map flymake-mode-map ("M-p" . flymake-goto-prev-error))
  (:map flymake-mode-map ("C-c l E" . flymake-show-project-diagnostics))
  (:map flymake-mode-map ("C-c l e" . flymake-show-buffer-diagnostics)))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (ivy-mode))

(use-package counsel
  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-b" . counsel-recentf)
	 ("M-g i" . counsel-imenu)
	 ("C-c b" . counsel-bookmark)
	 (:map minibuffer-local-map
	       ("C-r" . counsel-minibuffer-history))))

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

;; tree-sitter
;; (use-package rust-ts-mode
;;   :mode "\\.rs\\'"
;;   (rust-ts-mode . eglot-ensure))

;; npm i -g typescript-language-server; npm i -g typescript
(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . company-mode))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  ;; fixes an issue with sideline https://github.com/emacs-lsp/lsp-ui/issues/285#issuecomment-493092398
  (custom-set-faces
   '(markdown-code-face ((t (:inherit consolas))))))

(use-package yaml-mode)

(use-package denote
  :config
  (setq denote-infer-keywords t)
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("sql" "meeting" "todo" "daily"))
  :hook
  (dired-mode . denote-dired-mode-in-directories))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Serhiis-MacBook-Pro-2.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" "f76b5717f04b34542972fb4d320df806d9a465f16c07b31b4bd6e79e4feb1794" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d" default))
 '(package-selected-packages
   '(treemacs prog-mode mode-line-bell highlight-indentation yasnippet-snippets yasnippet ivy-xref all-the-icons-ivy newcomment company-box all-the-icons company darcula-theme magit scala-mode sbt-mode))
 '(warning-suppress-log-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
