(cl:in-package #:clarafx-test)

(defun suite-tests ()
  (run! 'intern-suite)
  (run! 'extern-suite))

(def-suite intern-suite :description "Clarafx internal test suite.")
(def-suite extern-suite :description "Clarafx external test suite.")

