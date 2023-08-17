(cl:in-package #:asdf-user)

(defsystem "clarafx"
  :version ""
  :author "Panji Kusuma <epanji@gmail.com>"
  :description ""
  :license  "BSD 2-Clause License"
  :depends-on ("claraoke" "font-discovery" "cl-freetype2")
  :serial t
  :components ((:module "clarafx.draw"
                :pathname "Draw"
                :serial t
                :components ((:file "package")
                             (:file "point")
                             (:file "drawing-command")
                             (:file "drawing-command-parser")
                             (:file "drawing-command-operator")))
               (:module "clarafx.core"
                :pathname "Core"
                :serial t
                :components ((:file "package")
                             (:file "font-ffi")
                             (:file "canvas")
                             (:file "alignment")
                             (:file "fx-implementation")
                             (:file "drawing-command-ffi")))
               (:module "clarafx.user"
                :pathname "User"
                :serial t
                :components ((:file "package")
                             (:file "load")
                             (:file "shapes")
                             (:file "effects")
                             (:file "file-io")))
               (:static-file "README.md"))
  :in-order-to ((test-op (load-op "clarafx-test")))
  :perform (test-op (o c) (symbol-call :clarafx-test :suite-tests)))

