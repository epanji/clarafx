(cl:in-package #:clarafx-test)

(defun suite-tests ()
  (run! 'intern-suite)
  (run! 'extern-suite)
  (run! 'dialogue-suite))

(defmacro ps-string (form)
  `(with-output-to-string (stream)
     (print-script ,form stream)))

(def-suite intern-suite :description "Clarafx internal test suite.")
(def-suite extern-suite :description "Clarafx external test suite.")
(def-suite dialogue-suite :description "Clarafx dialogue test suite.")

