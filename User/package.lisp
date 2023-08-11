(cl:in-package #:cl-user)

(defpackage #:clarafx.user
  (:nicknames #:clarafx)
  (:use #:common-lisp #:claraoke #:clarafx.core)
  (:export #:.shadow
           #:.style
           #:.text
           #:alignment
           #:alignment-code
           #:alpha
           #:alphap
           #:alphastring
           #:alphastringp
           #:angle
           #:arg1
           #:arg2
           #:arg3
           #:arg4
           #:arg5
           #:arg6
           #:arg7
           #:back-colour
           #:base-x1
           #:base-x2
           #:base-y1
           #:base-y2
           #:blue
           #:bold
           #:border-style
           #:canvas
           #:centiseconds
           #:color
           #:colorp
           #:colorstring
           #:colorstringp
           #:command
           #:comment
           #:decrease-duration
           #:decrease-karaoke
           #:decrease-modifier
           #:decrease-override
           #:define-effect
           #:delete-event
           #:delete-font
           #:delete-graphic
           #:delete-info
           #:delete-line
           #:delete-modifier
           #:delete-note
           #:delete-override
           #:delete-style
           #:descriptor
           #:dialogue
           #:dpi
           #:duration
           #:duration-difference
           #:duration-greaterp
           #:duration-length
           #:duration-lessp
           #:durationinteger
           #:durationintegerp
           #:durationp
           #:durationstring
           #:durationstringp
           #:effect
           #:encoding
           #:end
           #:events
           #:failed-to-create-color
           #:failed-to-create-duration
           #:failed-to-create-events
           #:failed-to-create-fonts
           #:failed-to-create-graphics
           #:failed-to-create-info
           #:failed-to-create-integer
           #:failed-to-create-note
           #:failed-to-create-script-info
           #:failed-to-create-style
           #:failed-to-create-styles
           #:failed-to-create-subtitle
           #:failed-to-create-text
           #:find-event
           #:find-font
           #:find-graphic
           #:find-info
           #:find-line
           #:find-modifier
           #:find-note
           #:find-override
           #:find-style
           #:font
           #:fontname
           #:fonts
           #:fontsize
           #:fontspace
           #:graphic
           #:graphics
           #:green
           #:header
           #:height
           #:hours
           #:increase-duration
           #:increase-karaoke
           #:increase-modifier
           #:increase-override
           #:index
           #:info
           #:insert-event
           #:insert-font
           #:insert-graphic
           #:insert-info
           #:insert-karaoke
           #:insert-karaoke-fill
           #:insert-karaoke-outline
           #:insert-line
           #:insert-modifier
           #:insert-note
           #:insert-override
           #:insert-style
           #:interval
           #:interval-counter
           #:interval-frequency
           #:italic
           #:last-event
           #:last-line
           #:layer
           #:line
           #:line-syllables
           #:lines
           #:make-alignment
           #:make-alignment*
           #:make-canvas
           #:make-canvas*
           #:make-canvas-face
           #:make-syllable
           #:margin-l
           #:margin-r
           #:margin-v
           #:minutes
           #:modifier
           #:modifiers
           #:movie
           #:name
           #:note
           #:null-object-warning
           #:object-must-be-batch
           #:object-must-be-color
           #:object-must-be-duration
           #:object-must-be-event
           #:object-must-be-events
           #:object-must-be-info
           #:object-must-be-integer
           #:object-must-be-line
           #:object-must-be-modifier
           #:object-must-be-note
           #:object-must-be-override
           #:object-must-be-script
           #:object-must-be-script-info
           #:object-must-be-style
           #:object-must-be-styles
           #:object-must-be-subtitle
           #:object-must-be-text
           #:origin-end
           #:origin-start
           #:original-text
           #:outline
           #:outline-colour
           #:override
           #:overrides
           #:parse-script
           #:picture
           #:plain-text
           #:point-x1
           #:point-x2
           #:point-y1
           #:point-y2
           #:primary-colour
           #:print-script
           #:random-color
           #:rectangle
           #:rectangle-area
           #:red
           #:rgb
           #:scale-x
           #:scale-y
           #:script
           #:script-info
           #:secondary-colour
           #:seconds
           #:section-line
           #:separator
           #:sort-events
           #:sort-overrides
           #:sound
           #:spacing
           #:split-dialogue
           #:start
           #:strike-out
           #:style
           #:styles
           #:subtitle
           #:syllable
           #:synch-duration
           #:table-line
           #:text
           #:underline
           #:update-karaoke
           #:update-modifier
           #:value
           #:value-mixin
           #:width
           #:with-every-syllable-from-karaoke
           #:with-syllable-modifiers))

