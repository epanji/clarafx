(cl:in-package #:cl-user)

(defpackage #:clarafx.user
  (:nicknames #:clarafx)
  (:use #:common-lisp #:claraoke #:clarafx.draw #:clarafx.core)
  (:export #:.shadow
           #:.style
           #:.text
           #:add-drawing-command
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
           #:char-to-drawing-commands
           #:clarafx-version
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
           #:drawing-commands
           #:drawing-commands-expr
           #:drawing-commands-string
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
           #:extra-dialogues
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
           #:find-partial
           #:find-style
           #:font
           #:fontname
           #:fonts
           #:fontsize
           #:fontspace
           #:funcall-effect
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
           #:insert-partial
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
           #:list-effects
           #:load-external-effects
           #:load-revision-file
           #:make-alignment
           #:make-alignment*
           #:make-canvas
           #:make-canvas*
           #:make-canvas-face
           #:make-dc-bezier
           #:make-dc-beziers
           #:make-dc-close-bsp
           #:make-dc-cubic-bsp
           #:make-dc-extend-bsp
           #:make-dc-line
           #:make-dc-lines
           #:make-dc-move
           #:make-dc-no-close-move
           #:make-face
           #:make-face*
           #:make-shape-five-star
           #:make-shape-heart
           #:make-shape-star
           #:make-syllable
           #:margin-l
           #:margin-r
           #:margin-v
           #:max-x
           #:max-xy
           #:max-y
           #:min-x
           #:min-xy
           #:min-y
           #:minmax-x
           #:minmax-xy
           #:minmax-y
           #:minutes
           #:modifier
           #:modifiers
           #:movie
           #:moving-x
           #:moving-xy
           #:moving-y
           #:name
           #:negate-x
           #:negate-xy
           #:negate-y
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
           #:parse-drawing-commands
           #:parse-effect
           #:parse-script
           #:partialp
           #:picture
           #:plain-text
           #:point-x1
           #:point-x2
           #:point-y1
           #:point-y2
           #:populate-delay-effect
           #:populate-odd-even-effect
           #:populate-zero-karaoke
           #:primary-colour
           #:print-script
           #:random-color
           #:read-subtitle-effect
           #:rectangle
           #:rectangle-area
           #:red
           #:register-effect
           #:reset-internal-effects
           #:resize-x
           #:resize-xy
           #:resize-y
           #:rgb
           #:rotate-center
           #:rotate-origin
           #:round-x
           #:round-xy
           #:round-y
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
           #:svg-data-to-element-drawing-commands
           #:svg-path-to-drawing-commands
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
           #:with-auto-increment-events
           #:with-every-syllable-from-karaoke
           #:with-syllable-modifiers
           #:write-subtitle-effect
           #:write-subtitle-file))

