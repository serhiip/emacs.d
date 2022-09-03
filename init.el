;;; init.el --- My emacs configuration -*- coding: utf-8 -*-
;;; Commentary:

;;; Has support for some langs I use, org-mode set up and
;;; customization and some custom functions

;;; Code:


(let* ((my-lisp-dir "~/.emacs.d/init/")
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(require 'files)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'leaf)

(prog1 "Load leaf.el"
  (leaf leaf
    :doc "Initialize leaf dependent packages"
    :config
    (leaf package
      :custom ((package-archives . '(("org"   . "https://orgmode.org/elpa/")
                                     ("melpa" . "https://melpa.org/packages/")
                                     ("gnu"   . "https://elpa.gnu.org/packages/"))))
      :config
      (package-initialize))))

(require 'functions)
(require 'pragmatapro-lig)

(leaf exec-path-from-shell :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(leaf custom
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (column-number-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-hl-line-mode 1)
  ;; (load custom-file)
  :setq
  `((confirm-nonexistent-file-or-buffer . nil)
    (completion-ignore-case . t)
    (read-file-name-completion-ignore-case . t)
    (inhibit-startup-screen . t)
    (indent-tabs-mode . nil)
    (backup-by-copying . t)
    (delete-old-versions . t)
    (kept-new-versions . 6)
    (kept-old-versions . 2)
    (version-control . t)
    ;; (custom-file . (concat user-emacs-directory "custom.el"))
    (debug-on-error . t)
    ;;(erc-autojoin-channels-alist . '(("freenode.net" "#haskell" "#haskell-ide-engine")))
    (exec-path-from-shell-variables . '("PATH" "MANPATH" "NIX_PATH" "NIX_SSL_CERT_FILE"))
    (lsp-keymap-prefix . "C-c l")
    (lsp-haskell-formatting-provider . "brittany")
    (lsp-haskell-brittany-on . t))
  :bind (
         ("<up>" . nil)
         ("<right>" . nil)
         ("<left>" . nil)
         ("<down>" . nil)
         ("<f9>" . treemacs)
         ("<f8>" . pragmatapro-lig-mode))
  )

(leaf dash
  :ensure t)

(leaf whitespace
  :require t
  :setq
  `((whitespace-line-column . 140))
  :bind (("C-x w" . whitespace-mode)))

(leaf monokai-theme
  :config
  (load-theme 'monokai t)
  :after package
  :ensure t
  :disabled nil)

(leaf winner
  :config
  (winner-mode 1))

(leaf flycheck
  :after prog-mode
  :ensure t
  :require t
  :config (global-flycheck-mode t)
  :disabled t
  :setq `((flycheck-emacs-lisp-load-path . 'inherit)))

(leaf company
  :after prog-mode
  :ensure t
  :hook (prog-mode-hook . company-mode))

(leaf projectile
  :ensure t
  :require t)

(leaf org
  :ensure t
  :require t
  :setq
  ((org-directory . "~/workspace/org")
    (org-default-notes-file . (serhiip-org-file-path "/gtd.org"))
    (org-todo-keywords . '((sequence "TODO" "WAIT"     "|" "DONE")
                           (sequence "TOBUY"           "|" "DONE")
                           (sequence "TOREAD"          "|" "DONE")
                           (sequence "BUG"             "|" "FIXED")
                           (sequence "FEATURE" "DOING" "|" "DONE")
                           (sequence "MEMO"            "|" "DONE")))
    (org-capture-templates . '(("t" "To Do" entry
				(file+headline org-default-notes-file "to do")
				"** TODO %i %?\n  %a")
                               ("s" "To Buy" entry
				(file+headline org-default-notes-file "to buy")
				"** TOBUY %i %?\n  %U\n  %a")
                               ("m" "To Memo" entry
				(file+headline org-default-notes-file "to memo")
				"** MEMO %i %?\n  %a")
                               ("r" "To read" entry
				(file+headline org-default-notes-file "to read")
				"** TOREAD %i %?\n  %U\n  %a")
                               ("f" "New feature" entry
				(file (lambda () (serhiip-org-get-current-file)))
				"* FEATURE %i %?\n  %U\n  %a")
                               ("b" "Bug" entry
				(file (lambda () (serhiip-org-get-current-file)))
				"* BUG %i %?\n  %U\n  %a")))
    (org-todo-keyword-faces . '(("TODO"    . "red")
				("MEMO"    . "firebrick")
				("DOING"   . "yellow")
				("FEATURE" . "firebrick")
				("BUG"     . "magenta")
				("TOREAD"  . "dark cyan")
				("TOBUY"   . "wheat")))
    (org-ellipsis . " ⤵"))
  :bind (("C-c a" . org-agenda)
	 ("C->"   . serhiip-take-notes-from-region)
         ("C-c c" . serhiip-take-note-todo)))

(leaf newcomment
  :bind (("C-<tab>" . comment-or-uncomment-region)))

(leaf rainbow-delimiters
  :ensure t
  :after elisp-mode
  :hook (emacs-lisp-mode-hook . rainbow-delimiters-mode))

(leaf smartparens
  :after elisp-mode)

(leaf magit
  :ensure t
  :setq `((vc-handled-backends . '()))
  :bind (("C-x g" . magit-status)))

(leaf helm
  :ensure t
  :after package
  :config
  (helm-mode t)
  (global-set-key (kbd "M-x") #'helm-M-x)
  :bind
  (("C-s"     . helm-occur)
   ("C-x C-f" . helm-find-files)
   (:global-map
    ([remap jump-to-register] . helm-register)
    ([remap list-buffers]     . helm-mini)))
  :custom
  (helm-display-source-at-screen-top . nil)
  :custom-face
  (helm-source-header . '((t (:background "#22083397778B" :foreground "white" :weight bold :height 0.8)))))

(leaf lsp-treemacs
  :ensure t
  :require t)

(leaf lsp-mode
  :ensure t
  :require t
  :setq `((lsp-enable-snippet . nil)
          (lsp-keymap-prefix . "C-c l"))
  :bind (("C-c l a"   . lsp-execute-code-action)
         ("C-c l e"   . lsp-treemacs-errors-list))
  :custom
  (lsp-prefer-flymake . nil)
  (lsp-keymap-prefix . "C-c l")
  :hook (haskell-mode-hook . lsp))

(leaf eltags
  :setq
  `((tags-revert-without-query . t)))

(leaf markdown-mode
  :ensure t
  :require t
  ;; fixes an issue with sideline https://github.com/emacs-lsp/lsp-ui/issues/285#issuecomment-493092398
  :custom-face (markdown-code-face . '((t (:inherit consolas)))))

(leaf lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :require t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(leaf haskell-mode
  :ensure t
  :require t
  :init
  :after lsp
  :hook lsp
  (defun runhaskell ()
    "Run current file via runhaskell command."
    (interactive)
    (async-shell-command
     (format
      "runhaskell %s"
      (buffer-file-name (current-buffer))))))

(leaf lsp-haskell
  :ensure t
  :after haskell-mode lsp
  :require t
  :config
  (eval-after-load 'lsp-ui-mode (add-hook 'haskell-mode-hook 'lsp-ui-mode))
  (eval-after-load 'lsp-mode (add-hook 'haskell-mode-hook 'lsp))
  :setq
  `((haskell-tags-on-save . nil)
    (lsp-log-io . nil)
    ))

(leaf company
  :ensure t
  :after lsp-mode
  :require t
  :hook (after-init-hook . global-company-mode)
  :setq
  `((company-idle-delay . 0.2)
    (company-minimum-prefix-length . 2)))

;; (leaf company-lsp
;;   :ensure t
;;   :after lsp-mode company
;;   :config
;;   (push 'company-lsp company-backends))

(leaf all-the-icons
  ;; M-x all-the-icons-install-fonts
  :ensure t
  :require t
  :custom
  (company-box-icons-alist . (quote company-box-icons-all-the-icons)))

(leaf company-box
  :ensure t
  :after company all-the-icons
  :require t
  :config
  (eval-after-load 'company-box (add-hook 'company-mode-hook 'company-box-mode)))

(leaf yasnippet
  :ensure t
  :require t
  :after company-box
  :config
  (yas-global-mode))

;; (leaf lsp-python-ms
;;   :init
;;   (setq lsp-python-ms-auto-install-server t)
;;   :reqire t
;;   :hook (python-mode-hook . lsp))

(leaf nix-mode :ensure t)

(leaf terraform-mode :ensure t)

(leaf prog-mode
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (defun add-pragmatapro-prettify-symbols-alist ()
    (setq prettify-symbols-alist pragmatapro-prettify-symbols-alist))

  ;; enable prettified symbols on comments
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

  ;; main hook fn, just add to text-mode/prog-mode
  (defun prettify-hook ()
    ;;(add-pragmatapro-prettify-symbols-alist)
    (setup-compose-predicate))

  ;; (add-hook 'prog-mode-hook 'setup-compose-predicate)
  ;; (add-hook 'text-mode-hook 'setup-compose-predicate)
  (add-hook 'text-mode-hook 'pragmatapro-lig-mode)
  (add-hook 'prog-mode-hook 'pragmatapro-lig-mode)
  ;;(global-prettify-symbols-mode +1)
  )

(leaf yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(leaf erc
  :init
  (defun erc-join ()
    (interactive)
    (let ((pwd (read-passwd "freenode passwd ")))
      (and pwd (erc :server "irc.freenode.net" :port 6667 :nick "abracadabrahocus" :password pwd)))))

(leaf scala-mode
  :ensure t
  :after lsp-mode lsp-ui
  :config
  (add-to-list 'auto-mode-alist '("\\.s\\(cala\\|bt\\)$" . scala-mode))
  (eval-after-load 'lsp-ui-mode (add-hook 'scala-mode-hook 'lsp-ui-mode))
  (eval-after-load 'lsp-mode (add-hook 'scala-mode-hook 'lsp)))

(leaf lsp-metals :ensure t :after scala-mode)

(leaf js2-mode
  :ensure t
  :reqire t
  :after tide
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  )

(leaf tide
  :ensure t
  :require t
  :init

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq standard-indent 2)
  (setq js-indent-level 2)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'js-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(leaf web-mode
  :after js2-mode
  :ensure t
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when (or
                     (string-equal "tsx" (file-name-extension buffer-file-name))
                     (string-equal "jsx" (file-name-extension buffer-file-name)))
                (setup-tide-mode))))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

;;; ocaml mode
;;; https://github.com/ocaml/ocaml-lsp#installation
(leaf tuareg
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'tuareg-mode-hook (lambda () (lsp))))

(leaf dockerfile-mode
  :ensure t
  :require t)

(leaf tmr
  :ensure t
  :require t)

(leaf denote
  :ensure t
  :require t
  :init
  (setq denote-infer-keywords t)
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("sql" "meeting" "todo" "daily"))
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

(leaf protobuf-mode
  :ensure t)

(org-todo-list)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(helm-display-source-at-screen-top nil)
 '(lsp-keymap-prefix "C-c l")
 '(lsp-ocaml-lang-server-command '("ocamllsp"))
 '(lsp-prefer-flymake nil t)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(tuareg yasnippet yaml-mode web-mode tide terraform-mode scala-mode rainbow-delimiters projectile ob-deno nix-mode monokai-theme magit lsp-ui lsp-metals lsp-haskell js2-mode helm exec-path-from-shell company-box all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :height 0.8))))
 '(markdown-code-face ((t (:inherit consolas)))))
