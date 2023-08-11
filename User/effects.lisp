(cl:in-package #:clarafx.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Effects
;;;
(define-effect (standard-effect-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'color1 :arg1 (primary-colour (style var))))

(define-effect (complement-effect-syllables var)
  (progn (setf (origin-start var) (start (dialogue var))
               (origin-end var) (start var))
         (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var)))
  (modifier 'color1 :arg1 (secondary-colour (style var))))

(define-effect (shaking-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'origin :arg1 (- (base-x2 var) (floor (width var) 2))
                    :arg2 (- (base-y1 var) (floor (height var) 2)))
  (modifier 'fad :arg1 250 :arg2 0)
  (modifier 'fontrotate-z :arg1 -3)
  (modifier 'transformation4 :arg1 200 :arg2 400 :arg3 0.9
                             :arg4 (list (modifier 'fontrotate-z :arg1 30)
                                         (modifier 'fontrotate-z :arg1 0))))

(define-effect (dropping-each-syllables var)
  (modifier 'fad :arg1 250 :arg2 0)
  (modifier 'move :arg1 (base-x1 var)
                  :arg2 (- (base-y1 var) (* 2/3 (height var)))
                  :arg3 (base-x1 var)
                  :arg4 (base-y1 var)
                  :arg5 0
                  :arg6 500))

(define-effect (rotate-ccw-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'origin :arg1 (- (base-x2 var) (truncate (width var) 2))
                    :arg2 (- (base-y1 var) (truncate (height var) 2)))
  (modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (modifier 'fontrotate-z :arg1 720)))

(define-effect (rotate-cw-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'origin :arg1 (- (base-x2 var) (truncate (width var) 2))
                    :arg2 (- (base-y1 var) (truncate (height var) 2)))
  (modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (modifier 'fontrotate-z :arg1 -720)))

(define-effect (enlarge-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'fontscale-y :arg1 0)
  (modifier 'transformation3 :arg1 0 :arg2 500 :arg3 (modifier 'fontscale-y :arg1 100)))

(define-effect (shrink-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'fontscale-y :arg1 200)
  (modifier 'transformation3 :arg1 0 :arg2 500 :arg3 (modifier 'fontscale-y :arg1 100)))

(define-effect (enlarge-with-star-each-syllables var)
  (progn (setf (extra-dialogues var)
               (list
                (dialogue "m 0 0 l 100 30 130 130 160 30 260 0 160 -30 130 -130 100 -30"
                          :start (start var)
                          :duration 50
                          :layer 1000
                          :overrides
                          (list
                           (override
                            'batch 0
                            :modifiers
                            (list
                             (modifier 'drawing-mode :arg1 4)
                             (modifier 'pos :arg1 (+ (base-x1 var)
                                                     (truncate (width var) 2))
                                            :arg2 (base-y1 var))
                             (modifier 'alpha3 :arg1 (alphastring "FF"))
                             (modifier 'alpha4 :arg1 (alphastring "FF"))
                             (modifier 'color1
                                       :arg1 (colorstring
                                              (secondary-colour (style var))))
                             (modifier 'fad :arg1 220 :arg2 220)))))))
         (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var)))
  (modifier 'fontscale-y :arg1 0)
  (modifier 'transformation3 :arg1 0 :arg2 500 :arg3 (modifier 'fontscale-y :arg1 100)))

