(cl:in-package #:cl-user)

(defpackage #:clarafx.core
  (:use #:common-lisp #:claraoke)
  (:intern #:%string-pixel-width
           #:calculate-layout-width
           #:calculate-virtual-alignment-1
           #:ensure-font
           #:ensure-font-pathname
           #:find-effect
           #:make-face
           #:make-face*
           #:margin-bottom
           #:margin-left
           #:margin-right
           #:margin-top
           #:newline-and-space-splitter
           #:parse-effect-info
           #:set-char-size
           #:split-dialogue-multiple-line
           #:split-dialogue-single-line
           #:string-pixel-height
           #:string-pixel-width)
  (:export #:alignment
           #:alignment-code
           #:base-x1
           #:base-x2
           #:base-y1
           #:base-y2
           #:bold
           #:canvas
           #:define-effect
           #:dpi
           #:duration
           #:extra-dialogues
           #:fontname
           #:fontsize
           #:fontspace
           #:funcall-effect
           #:height
           #:italic
           #:line-syllables
           #:make-alignment
           #:make-alignment*
           #:make-canvas
           #:make-canvas*
           #:make-canvas-face
           #:make-syllable
           #:origin-end
           #:origin-start
           #:parse-effect
           #:plain-text
           #:point-x1
           #:point-x2
           #:point-y1
           #:point-y2
           #:rectangle
           #:rectangle-area
           #:register-effect
           #:split-dialogue
           #:start
           #:syllable
           #:width
           #:with-every-syllable-from-karaoke
           #:with-syllable-modifiers))

