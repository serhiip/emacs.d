;;; init.el --- My emacs configuration -*- coding: utf-8 -*-
;;; Commentary:

;;; Has support for some langs I use, org-mode set up and
;;; customization and some custom functions

;;; Code:

(let ((default-directory (concat
                          user-emacs-directory
                          (convert-standard-filename "init"))))
    (normal-top-level-add-subdirs-to-load-path)
    (add-to-list 'load-path default-directory))

(require 'files)
(require 'leaf)

(set-frame-font "PragmataPro Mono Liga")
(set-face-attribute 'default nil :height 130)

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
  (load custom-file)
  :setq
  `((confirm-nonexistent-file-or-buffer . nil)
    (completion-ignore-case . t)
    (read-file-name-completion-ignore-case . t)
    (inhibit-startup-screen . t)
    (indent-tabs-mode . nil)
    (backup-by-copying . t)
    (backup-directory-alist . '(("." . "~/.saves")))
    (delete-old-versions . t)
    (kept-new-versions . 6)
    (kept-old-versions . 2)
    (version-control . t)
    (custom-file . (concat user-emacs-directory "custom.el")))
  :custom
  ;; (custom-enabled-themes . '(tsdh-dark))
  :bind (("<up>" . nil) ("<right>" . nil) ("<left>" . nil) ("<down>" . nil)))

(leaf whitespace
  :require t
  :setq
  `((whitespace-line-column . 120))
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

(leaf prog-mode
  :require t)

(leaf flycheck
  :after prog-mode
  :ensure t
  :config
  (global-flycheck-mode t)
  :disabled nil
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

(leaf lsp-mode
  :ensure t
  :after prog-mode
  :require t
  :setq `((lsp-enable-snippet . nil))
  :custom
  (lsp-prefer-flymake . nil))

(leaf eltags
  :setq
  `((tags-revert-without-query . t)))

(leaf haskell-mode
  :ensure t
  :require t
  :init
  (defun fmt-haskell-file ()
    "Format current file using brittany."
    (interactive)
    (async-shell-command
     (format
      "brittany --write-mode=inplace %s"
      (buffer-file-name (current-buffer))))))

(leaf lsp-haskell
  :ensure t
  :after lsp-mode haskell-mode
  :require t
  :config
  (eval-after-load 'lsp (add-hook 'haskell-mode-hook 'lsp))
  :setq
  `((lsp-haskell-process-path-hie . "hie-wrapper")
    (haskell-tags-on-save . nil)))

(leaf company
  :ensure t
  :after lsp-mode
  :require t
  :hook (after-init-hook . global-company-mode)
  :setq
  `((company-idle-delay . 0.2)
    (company-minimum-prefix-length . 2)))

(leaf company-lsp
  :ensure t
  :after lsp-mode company
  :config
  (push 'company-lsp company-backends))

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

(leaf python-mode
  :require t
  :ensure t
  :hook (python-mode-hook . lsp))

(leaf flycheck-pycheckers
  :ensure t
  :require t
  :after python-mode flycheck
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))


(org-todo-list)

;;; init.el ends here
