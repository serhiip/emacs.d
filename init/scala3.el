(defun is-scala3-project ()
  "Check if the current project is using scala3.

Loads the build.sbt file for the project and serach for the scalaVersion."
  t)

(defun with-disable-for-scala3 (orig-scala-mode-map:add-self-insert-hooks &rest arguments)
    "When using scala3 skip adding indention hooks."
    (unless (is-scala3-project)
      (apply orig-scala-mode-map:add-self-insert-hooks arguments)))

(defun disable-scala-indent ()
  "In scala 3 indent line does not work as expected due to whitespace grammar."
  (when (is-scala3-project)
    (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)))

(provide 'scala3)
