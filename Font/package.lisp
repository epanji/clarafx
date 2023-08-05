(cl:in-package #:cl-user)

(defpackage #:clarafx-font
  (:use #:common-lisp)
  (:export #:rectangle
           #:point-x1
           #:point-y1
           #:point-x2
           #:point-y2
           ;;
           #:rectangle-area
           #:width
           #:height
           ;;
           #:canvas
           #:fontname
           #:fontsize
           #:fontspace
           #:bold
           #:italic
           #:dpi
           #:make-canvas*
           #:make-canvas
           #:make-canvas-face
           ;;
           #:syllable
           #:plain-text
           #:start
           #:duration
           ;;
           #:split-dialogue
           #:with-every-syllable-from-karaoke
           #:alignment
           #:alignment-code
           #:syllables
           #:make-alignment*
           #:make-alignment
           #:make-syllable))

