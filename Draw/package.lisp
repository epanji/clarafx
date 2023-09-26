(cl:in-package #:cl-user)

(defpackage #:clarafx.draw
  (:use #:common-lisp)
  (:intern #:%minmax
           #:%moving
           #:%negate
           #:%resize
           #:%round
           #:bezier
           #:close-bsp
           #:cubic-bsp
           #:drawing-command
           #:drawing-command-name-validp
           #:drawing-command-string
           #:extend-bsp
           #:line
           #:make-parser
           #:make-prev-comparison
           #:move
           #:name
           #:no-close-move
           #:parse-drawing-command
           #:point
           #:point-string
           #:point-x
           #:point-y
           #:points
           #:split-element-drawing-commands)
  (:export #:add-drawing-command
           #:drawing-commands
           #:drawing-commands-expr
           #:drawing-commands-string
           #:make-dc-bezier
           #:make-dc-beziers
           #:make-dc-close-bsp
           #:make-dc-cubic-bsp
           #:make-dc-extend-bsp
           #:make-dc-line
           #:make-dc-lines
           #:make-dc-move
           #:make-dc-no-close-move
           #:max-x
           #:max-xy
           #:max-y
           #:min-x
           #:min-xy
           #:min-y
           #:minmax-x
           #:minmax-xy
           #:minmax-y
           #:moving-x
           #:moving-xy
           #:moving-y
           #:negate-x
           #:negate-xy
           #:negate-y
           #:parse-drawing-commands
           #:resize-x
           #:resize-xy
           #:resize-y
           #:round-x
           #:round-xy
           #:round-y
           #:svg-data-to-element-drawing-commands
           #:svg-path-to-drawing-commands))

