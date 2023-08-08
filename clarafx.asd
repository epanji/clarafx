(cl:in-package #:asdf-user)

(defsystem "clarafx"
  :version ""
  :author "Panji Kusuma <epanji@gmail.com>"
  :description ""
  :license  "BSD 2-Clause License"
  :depends-on ("claraoke" "font-discovery" "cl-freetype2")
  :serial t
  :components ((:module "clarafx.core"
                :pathname "Core"
                :serial t
                :components ((:file "package")
                             (:file "font-ffi")
                             (:file "canvas")
                             (:file "alignment")
                             (:file "fx-implementation")))
               (:module "clarafx.user"
                :pathname "User"
                :serial t
                :components ((:file "package")
                             (:file "effects")))
               (:static-file "README.md")))

