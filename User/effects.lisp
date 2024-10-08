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
               (origin-end var) (1+ (start var)))
         (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var)))
  (modifier 'color1 :arg1 (secondary-colour (style var))))

(define-effect (complement-partial-effect-syllables var)
  (progn (setf (origin-start var) (start (dialogue var))
               (origin-end var) (if (partialp var)
                                    (1+ (start var))
                                    (end (dialogue var))))
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

(define-effect (clip-expand-vertical-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'clip-rectangle :arg1 (point-x1 var)
                            :arg2 (+ (point-y1 var) (* 3/4 (height var)))
                            :arg3 (point-x2 var)
                            :arg4 (- (point-y2 var) (* 3/4 (height var))))
  (modifier 'transformation4
            :arg1 0 :arg2 500 :arg3 0.9
            :arg4 (modifier 'clip-rectangle :arg1 (point-x1 var)
                                            :arg2 (point-y1 var)
                                            :arg3 (+ (point-x2 var)
                                                     (outline (style var))
                                                     (.shadow (style var)))
                                            :arg4 (+ (point-y2 var)
                                                     (outline (style var))
                                                     (.shadow (style var))))))

(define-effect (clip-expand-horizontal-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'clip-rectangle :arg1 (+ (point-x1 var) (* 1/2 (width var)))
                            :arg2 (point-y1 var)
                            :arg3 (- (point-x2 var) (* 1/2 (width var)))
                            :arg4 (point-y2 var))
  (modifier 'transformation4
            :arg1 0 :arg2 250 :arg3 0.9
            :arg4 (modifier 'clip-rectangle :arg1 (point-x1 var)
                                            :arg2 (point-y1 var)
                                            :arg3 (+ (point-x2 var)
                                                     (outline (style var))
                                                     (.shadow (style var)))
                                            :arg4 (+ (point-y2 var)
                                                     (outline (style var))
                                                     (.shadow (style var))))))

(define-effect (vacuum-top-center-each-syllables var)
  (modifier 'transformation4
            :arg1 (* 10 (duration var))
            :arg2 3000
            :arg3 0.9
            :arg4 (list (modifier 'fontscale-x :arg1 60)
                        (modifier 'fontscale-x :arg1 60)))
  (modifier 'move :arg1 (base-x1 var)
                  :arg2 (base-y1 var)
                  :arg3 (truncate (width (canvas var)) 2)
                  :arg4 (- (height var))
                  :arg5 (* 10 (duration var))
                  :arg6 3000))

(define-effect (shear-x-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'transformation4
            :arg1 0
            :arg2 250
            :arg3 0.9
            :arg4 (list (modifier 'fontshear-x :arg1 -1)
                        (modifier 'fontshear-x :arg1 0))))

(define-effect (shear-y-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'transformation4
            :arg1 0
            :arg2 250
            :arg3 0.9
            :arg4 (list (modifier 'fontshear-y :arg1 -1)
                        (modifier 'fontshear-y :arg1 0))))

(define-effect (gradation-each-syllables var)
  (let ((string (.text (.text (dialogue var))))
        (override (first (overrides (dialogue var)))))
    (setf (origin-start var)
          (increase-duration
           (duration
            (if (char= #\INVISIBLE_SEPARATOR (char string 0))
                (arg1 (claraoke:increase-karaoke override 0))
                0))
           (start (dialogue var))))
    (setf (origin-end var) (end (dialogue var)))
    (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var)))
  (modifier 'color1 :arg1 (nth (index-in-line var)
                               (gradation-colors
                                (secondary-colour (style var))
                                (primary-colour (style var))
                                (count-in-line var)))))

(define-effect (rgb-range-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'border :arg1 1)
  (modifier 'blur :arg1 2)
  (modifier 'color1 :arg1 (color "white"))
  (modifier 'color3 :arg1 (nth (index-in-line var)
                               (rgb-range-colors
                                (primary-colour (style var))
                                (secondary-colour (style var))
                                (count-in-line var)))))

(define-effect (rbg-range-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'border :arg1 1)
  (modifier 'blur :arg1 2)
  (modifier 'color1 :arg1 (color "white"))
  (modifier 'color3 :arg1 (nth (index-in-line var)
                               (rbg-range-colors
                                (primary-colour (style var))
                                (secondary-colour (style var))
                                (count-in-line var)))))

(define-effect (grb-range-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'border :arg1 1)
  (modifier 'blur :arg1 2)
  (modifier 'color1 :arg1 (color "white"))
  (modifier 'color3 :arg1 (nth (index-in-line var)
                               (grb-range-colors
                                (primary-colour (style var))
                                (secondary-colour (style var))
                                (count-in-line var)))))

(define-effect (gbr-range-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'border :arg1 1)
  (modifier 'blur :arg1 2)
  (modifier 'color1 :arg1 (color "white"))
  (modifier 'color3 :arg1 (nth (index-in-line var)
                               (gbr-range-colors
                                (primary-colour (style var))
                                (secondary-colour (style var))
                                (count-in-line var)))))

(define-effect (brg-range-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'border :arg1 1)
  (modifier 'blur :arg1 2)
  (modifier 'color1 :arg1 (color "white"))
  (modifier 'color3 :arg1 (nth (index-in-line var)
                               (brg-range-colors
                                (primary-colour (style var))
                                (secondary-colour (style var))
                                (count-in-line var)))))

(define-effect (bgr-range-each-syllables var)
  (modifier 'pos :arg1 (base-x1 var) :arg2 (base-y1 var))
  (modifier 'border :arg1 1)
  (modifier 'blur :arg1 2)
  (modifier 'color1 :arg1 (color "white"))
  (modifier 'color3 :arg1 (nth (index-in-line var)
                               (bgr-range-colors
                                (primary-colour (style var))
                                (secondary-colour (style var))
                                (count-in-line var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register Effects
;;;
(defun reset-internal-effects ()
  "Re-register all internal effects."
  (register-effect "standard" 'standard-effect-syllables)
  (register-effect "complement" 'complement-effect-syllables)
  (register-effect "complement-partial" 'complement-partial-effect-syllables)
  (register-effect "shaking" 'shaking-each-syllables)
  (register-effect "dropping" 'dropping-each-syllables)
  (register-effect "rotate-ccw" 'rotate-ccw-each-syllables)
  (register-effect "rotate-cw" 'rotate-cw-each-syllables)
  (register-effect "enlarge" 'enlarge-each-syllables)
  (register-effect "shrink" 'shrink-each-syllables)
  (register-effect "enlarge-with-star" 'enlarge-with-star-each-syllables)
  (register-effect "clip-expand-vertical" 'clip-expand-vertical-each-syllables)
  (register-effect "clip-expand-horizontal" 'clip-expand-horizontal-each-syllables)
  (register-effect "vacuum-top-center" 'vacuum-top-center-each-syllables)
  (register-effect "shear-x" 'shear-x-each-syllables)
  (register-effect "shear-y" 'shear-y-each-syllables)
  (register-effect "gradation" 'gradation-each-syllables)
  (register-effect "rgb-range" 'rgb-range-each-syllables)
  (register-effect "rbg-range" 'rbg-range-each-syllables)
  (register-effect "grb-range" 'grb-range-each-syllables)
  (register-effect "gbr-range" 'gbr-range-each-syllables)
  (register-effect "brg-range" 'brg-range-each-syllables)
  (register-effect "bgr-range" 'bgr-range-each-syllables)
  (list-effects))

(reset-internal-effects)

