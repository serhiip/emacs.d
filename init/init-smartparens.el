(require 'use-package)

(use-package smartparens
  :ensure t
  :after scala-mode
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "s-[")
  (sp-pair "{" "}" :wrap "C-{")

  (bind-key "C-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "C-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

(provide 'init-smartparens)
