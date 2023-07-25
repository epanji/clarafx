(cl:in-package #:asdf-user)

(defsystem "claraoke-font"
  :depends-on ("claraoke-base" "zpb-ttf")
  :serial t
  :components
  ((:file "package")
   (:file "ttf")
   (:file "canvas")
   (:file "alignment")
   (:file "font-implementation")
   (:file "test")))

