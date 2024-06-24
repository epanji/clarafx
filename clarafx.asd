(cl:in-package #:asdf-user)

(defsystem "clarafx"
  :version (:read-file-form "version.lisp-expr")
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "Read and write customize karaoke effects targeting ASS format. (Advanced Substation Alpha)"
  :license  "BSD 2-Clause License"
  :depends-on ("asdf" "claraoke" "font-discovery" "cl-freetype2" "unix-opts")
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
                             (:file "partial")
                             (:file "alignment")
                             (:file "alignment-code")
                             (:file "alignment-calculation")
                             (:file "fx-implementation")
                             (:file "drawing-command-ffi")))
               (:module "clarafx.user"
                :pathname "User"
                :serial t
                :components ((:file "package")
                             (:file "version")
                             (:file "load")
                             (:file "shapes")
                             (:file "effects")
                             (:file "dialogue")
                             (:file "file-io")))
               (:module "clarafx.cli"
                :pathname "CLI"
                :serial t
                :components ((:file "package")
                             (:file "main")
                             (:static-file "Makefile")
                             (:static-file "slack-desc")
                             (:static-file "slackbuild.sh")))
               (:static-file "LICENSE")
               (:static-file "README.md")
               (:static-file "version.lisp-expr"))
  :in-order-to ((test-op (load-op "clarafx-test")))
  :perform (test-op (o c) (symbol-call :clarafx-test :suite-tests)))

