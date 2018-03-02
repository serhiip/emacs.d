(require 'use-package)
(require 'vc)
(require 'server)

(defun serhiip--staged-diff ()
  ;; might be a way to make vc lib to show diff for staged stuff only
  (setq vc-diff-added-files t)
  ;; will show whole diff but should show only staged stuff
  (vc-root-diff nil))

(defun serhiip--log-edit-callback () (interactive))

(use-package log-edit
  :ensure t
  :init
  (add-to-list
   'auto-mode-alist
   '(".*_EDITMSG\\'" . (lambda ()
                         (log-edit
                          'server-edit
                          nil
                          '((log-edit-diff-function . serhiip--staged-diff)))))))

(provide 'init-log-edit)
