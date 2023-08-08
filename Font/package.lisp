(cl:in-package #:cl-user)

(defpackage #:clarafx-font
  (:use #:common-lisp #:claraoke)
  (:intern #:%string-pixel-width
           #:calculate-layout-width
           #:calculate-virtual-alignment-1
           #:ensure-font
           #:ensure-font-pathname
           #:line
           #:make-face
           #:make-face*
           #:margin-bottom
           #:margin-left
           #:margin-right
           #:margin-top
           #:set-char-size
           #:split-dialogue-multiple-line
           #:split-dialogue-single-line
           #:string-pixel-height
           #:string-pixel-width)
  (:export #:alignment
           #:alignment-code
           #:bold
           #:canvas
           #:define-effect
           #:dpi
           #:duration
           #:fontname
           #:fontsize
           #:fontspace
           #:height
           #:italic
           #:line-syllables
           #:make-alignment
           #:make-alignment*
           #:make-canvas
           #:make-canvas*
           #:make-canvas-face
           #:make-syllable
           #:plain-text
           #:point-x1
           #:point-x2
           #:point-y1
           #:point-y2
           #:rectangle
           #:rectangle-area
           #:resize-big-y-syllables
           #:resize-small-y-syllables
           #:rotate-ccw-syllables
           #:rotate-cw-syllables
           #:split-dialogue
           #:start
           #:syllable
           #:width
           #:with-every-syllable-from-karaoke
           #:with-syllable-modifiers))

