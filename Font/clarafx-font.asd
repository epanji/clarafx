(cl:in-package #:asdf-user)

(defsystem "clarafx-font"
  :depends-on ("claraoke" "font-discovery" "cl-freetype2")
  :serial t
  :components
  ((:file "package")
   (:file "font-ffi")
   (:file "canvas")
   (:file "alignment")
   (:file "fx-implementation")))

