(cl:in-package #:asdf-user)

(defsystem "clarafx-test"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description ""
  :depends-on ("clarafx" "fiveam")
  :serial t
  :components ((:file "package")
               (:file "suites")
               (:file "intern-tests")
               (:file "extern-tests")))

