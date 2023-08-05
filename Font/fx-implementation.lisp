(cl:in-package :clarafx-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define Effects
;;;
(defmacro with-syllable-modifiers ((var-name dialogue subtitle alignment-code style-name dpi) &body body)
  (let ((modifiers (gensym "WSM"))
        (overrides (gensym "WSM")))
    `(with-every-syllable-from-karaoke (,var-name ,dialogue ,subtitle ,alignment-code ,style-name ,dpi)
       (let* ((,modifiers (list ,@body))
              (,overrides (list (claraoke:override 'alignment-numpad 0
                                                   :arg1 1
                                                   :modifiers ,modifiers))))
         (claraoke:dialogue (plain-text ,var-name) :start (start ,var-name)
                                                   :end (claraoke:end ,dialogue)
                                                   :overrides ,overrides)))))

(defmacro define-effect ((name var) &body body)
  `(defun ,name (dialogue &key subtitle (alignment-code 2) (style-name "Default") (dpi 64))
     (with-syllable-modifiers (,var dialogue subtitle alignment-code style-name dpi)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Effects
;;;
(define-effect (rotate-ccw-syllables var)
  (claraoke:modifier 'pos :arg1 (point-x1 var) :arg2 (point-y2 var))
  (claraoke:modifier 'origin :arg1 (- (point-x2 var) (floor (width var) 2)) :arg2 (- (point-y2 var) (floor (height var) 2)))
  (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'fontrotate-z :arg1 720)))

(define-effect (rotate-cw-syllables var)
  (claraoke:modifier 'pos :arg1 (point-x1 var) :arg2 (point-y2 var))
  (claraoke:modifier 'origin :arg1 (- (point-x2 var) (floor (width var) 2)) :arg2 (- (point-y2 var) (floor (height var) 2)))
  (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'fontrotate-z :arg1 -720)))

(define-effect (resize-small-y-syllables var)
  (claraoke:modifier 'pos :arg1 (point-x1 var) :arg2 (point-y2 var))
  (claraoke:modifier 'fontscale-y :arg1 0)
  (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'fontscale-y :arg1 100)))

(define-effect (resize-big-y-syllables var)
  (claraoke:modifier 'pos :arg1 (point-x1 var) :arg2 (point-y2 var))
  (claraoke:modifier 'fontscale-y :arg1 300)
  (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'fontscale-y :arg1 100)))

