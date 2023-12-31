(cl:in-package :clarafx.draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Resize: resize-x, resize-y, resize-xy
;;;
(defgeneric %resize (object value target))

(defmethod %resize ((object point) (value rational) target)
  (let ((num (numerator value))
        (den (denominator value)))
    (unless (zerop num)
      (let ((x (point-x object))
            (y (point-y object)))
        (case target
          (:x (setf (point-x object) (float (/ (* x num) den))))
          (:y (setf (point-y object) (float (/ (* y num) den))))
          (:xy (setf (point-x object) (float (/ (* x num) den))
                     (point-y object) (float (/ (* y num) den))))
          (otherwise
           (when (rationalp target)
             (let ((num2 (numerator target))
                   (den2 (denominator target)))
               (setf (point-x object) (float (/ (* x num) den))
                     (point-y object) (float (/ (* y num2) den2)))))))
        (values)))))

(defmethod %resize ((object close-bsp) (value rational) target)
  (values))

(defmethod %resize ((object drawing-command) (value rational) target)
  (%resize (points object) value target))

(defmethod %resize ((object drawing-commands) (value rational) target)
  (%resize (drawing-commands object) value target))

(defmethod %resize ((object cons) (value rational) target)
  (let ((container (etypecase object
                     ((cons drawing-command) object)
                     ((cons point) object))))
    (loop for item in container
          do (%resize item value target))))

(defun resize-x (object value)
  (%resize object (rationalize value) :x)
  (values object))

(defun resize-y (object value)
  (%resize object (rationalize value) :y)
  (values object))

(defun resize-xy (object value1 &optional value2)
  (if (null value2)
      (%resize object (rationalize value1) :xy)
      (%resize object (rationalize value1) (rationalize value2)))
  (values object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving: moving-x, moving-y, moving-xy
;;;
(defgeneric %moving (object value target))

(defmethod %moving ((object point) value target)
  (let ((x (point-x object))
        (y (point-y object)))
    (case target
      (:x (setf (point-x object) (+ x value)))
      (:y (setf (point-y object) (+ y value)))
      (:xy (setf (point-x object) (+ x value)
                 (point-y object) (+ y value)))
      (otherwise
       (when (realp target)
         (setf (point-x object) (+ x value)
               (point-y object) (+ y target)))))
    (values)))

(defmethod %moving ((object close-bsp) value target)
  (values))

(defmethod %moving ((object drawing-command) value target)
  (%moving (points object) value target))

(defmethod %moving ((object drawing-commands) value target)
  (%moving (drawing-commands object) value target))

(defmethod %moving ((object cons) value target)
  (let ((container (etypecase object
                     ((cons drawing-command) object)
                     ((cons point) object))))
    (loop for item in container
          do (%moving item value target))))

(defun moving-x (object value)
  (%moving object value :x)
  (values object))

(defun moving-y (object value)
  (%moving object value :y)
  (values object))

(defun moving-xy (object value1 &optional value2)
  (if (null value2)
      (%moving object value1 :xy)
      (%moving object value1 value2))
  (values object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Negate: negate-x, negate-y, negate-xy
;;;
(defgeneric %negate (object target))

(defmethod %negate ((object point) target)
  (let ((x (point-x object))
        (y (point-y object)))
    (ecase target
      (:x (setf (point-x object) (- x)))
      (:y (setf (point-y object) (- y)))
      (:xy (setf (point-x object) (- x)
                 (point-y object) (- y))))))

(defmethod %negate ((object close-bsp) target)
  (values))

(defmethod %negate ((object drawing-command) target)
  (%negate (points object) target))

(defmethod %negate ((object drawing-commands) target)
  (%negate (drawing-commands object) target))

(defmethod %negate ((object cons) target)
  (let ((container (etypecase object
                     ((cons drawing-command) object)
                     ((cons point) object))))
    (loop for item in container
          do (%negate item target))))

(defun negate-x (object)
  (%negate object :x)
  (values object))

(defun negate-y (object)
  (%negate object :y)
  (values object))

(defun negate-xy (object)
  (%negate object :xy)
  (values object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Round: round-x, round-y, round-xy
;;;
(defgeneric %round (object target))

(defmethod %round ((object point) target)
  (let ((x (point-x object))
        (y (point-y object)))
    (ecase target
      (:x (setf (point-x object) (round x)))
      (:y (setf (point-y object) (round y)))
      (:xy (setf (point-x object) (round x)
                 (point-y object) (round y))))))

(defmethod %round ((object close-bsp) target)
  (values))

(defmethod %round ((object drawing-command) target)
  (%round (points object) target))

(defmethod %round ((object drawing-commands) target)
  (%round (drawing-commands object) target))

(defmethod %round ((object cons) target)
  (let ((container (etypecase object
                     ((cons drawing-command) object)
                     ((cons point) object))))
    (loop for item in container
          do (%round item target))))

(defun round-x (object)
  (%round object :x)
  (values object))

(defun round-y (object)
  (%round object :y)
  (values object))

(defun round-xy (object)
  (%round object :xy)
  (values object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MinMax: min-x, min-y, min-xy,
;;;         max-x, max-y, max-xy,
;;;         minmax-x, minmax-y, minmax-xy
;;;
(defun make-prev-comparison (min-or-max)
  (declare (type (member :min :max) min-or-max))
  (let ((prev nil)
        (pred (case min-or-max
                (:min #'<)
                (:max #'>))))
    (lambda (curr)
      (cond ((null curr)
             (values prev))
            ((null prev)
             (setf prev curr)
             (values))
            ((funcall pred curr prev)
             (setf prev curr)
             (values))
            (t (values))))))

(defvar *compare-min-x* nil) ; (make-prev-comparison :min)
(defvar *compare-min-y* nil) ; (make-prev-comparison :min)
(defvar *compare-max-x* nil) ; (make-prev-comparison :max)
(defvar *compare-max-y* nil) ; (make-prev-comparison :max)

(defgeneric %minmax (object target))

(defmethod %minmax ((object point) target)
  (let ((x (point-x object))
        (y (point-y object)))
    (ecase target
      (:min-x (funcall *compare-min-x* x))
      (:min-y (funcall *compare-min-y* y))
      (:min-xy
       (funcall *compare-min-x* x)
       (funcall *compare-min-y* y))
      (:max-x (funcall *compare-max-x* x))
      (:max-y (funcall *compare-max-y* y))
      (:max-xy
       (funcall *compare-max-x* x)
       (funcall *compare-max-y* y))
      (:minmax-x
       (funcall *compare-min-x* x)
       (funcall *compare-max-x* x))
      (:minmax-y
       (funcall *compare-min-y* y)
       (funcall *compare-max-y* y))
      (:minmax-xy
       (funcall *compare-min-x* x)
       (funcall *compare-min-y* y)
       (funcall *compare-max-x* x)
       (funcall *compare-max-y* y)))))

(defmethod %minmax ((object close-bsp) target)
  (values))

(defmethod %minmax ((object drawing-command) target)
  (%minmax (points object) target))

(defmethod %minmax ((object drawing-commands) target)
  (%minmax (drawing-commands object) target))

(defmethod %minmax ((object cons) target)
  (let ((container (etypecase object
                     ((cons drawing-command) object)
                     ((cons point) object))))
    (loop for item in container
          do (%minmax item target))))

(defun min-x (object)
  (let ((*compare-min-x* (make-prev-comparison :min)))
    (%minmax object :min-x)
    (funcall *compare-min-x* nil)))

(defun min-y (object)
  (let ((*compare-min-y* (make-prev-comparison :min)))
    (%minmax object :min-y)
    (funcall *compare-min-y* nil)))

(defun min-xy (object)
  (let ((*compare-min-x* (make-prev-comparison :min))
        (*compare-min-y* (make-prev-comparison :min)))
    (%minmax object :min-xy)
    (list (funcall *compare-min-x* nil)
          (funcall *compare-min-y* nil))))

(defun max-x (object)
  (let ((*compare-max-x* (make-prev-comparison :max)))
    (%minmax object :max-x)
    (funcall *compare-max-x* nil)))

(defun max-y (object)
  (let ((*compare-max-y* (make-prev-comparison :max)))
    (%minmax object :max-y)
    (funcall *compare-max-y* nil)))

(defun max-xy (object)
  (let ((*compare-max-x* (make-prev-comparison :max))
        (*compare-max-y* (make-prev-comparison :max)))
    (%minmax object :max-xy)
    (list (funcall *compare-max-x* nil)
          (funcall *compare-max-y* nil))))

(defun minmax-x (object)
  (let ((*compare-min-x* (make-prev-comparison :min))
        (*compare-max-x* (make-prev-comparison :max)))
    (%minmax object :minmax-x)
    (list (funcall *compare-min-x* nil)
          (funcall *compare-max-x* nil))))

(defun minmax-y (object)
  (let ((*compare-min-y* (make-prev-comparison :min))
        (*compare-max-y* (make-prev-comparison :max)))
    (%minmax object :minmax-y)
    (list (funcall *compare-min-y* nil)
          (funcall *compare-max-y* nil))))

(defun minmax-xy (object)
  (let ((*compare-min-x* (make-prev-comparison :min))
        (*compare-max-x* (make-prev-comparison :max))
        (*compare-min-y* (make-prev-comparison :min))
        (*compare-max-y* (make-prev-comparison :max)))
    (%minmax object :minmax-xy)
    (list (funcall *compare-min-x* nil)
          (funcall *compare-max-x* nil)
          (funcall *compare-min-y* nil)
          (funcall *compare-max-y* nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rotate: rotate-origin, rotate-center
;;;
;;; x' = x cos θ + y sin θ
;;; y' = - x sin θ + y cos θ
;;;
(defun %deg->rad (angle)
  (let ((deg (mod angle 360)))
    (* deg (/ pi 180))))

(defun %cos (angle)
  (cos (%deg->rad angle)))

(defun %sin (angle)
  (sin (%deg->rad angle)))

(defun %new-x (angle x y)
  (round (+ (* x (%cos angle))
            (* y (%sin angle)))))

(defun %new-y (angle x y)
  (round (+ (- (* x (%sin angle)))
            (* y (%cos angle)))))

(defgeneric %rotate (object origin angle))

(defmethod %rotate ((object point) (origin point) (angle real))
  (let ((object-x (point-x object))
        (object-y (point-y object))
        (origin-x (point-x origin))
        (origin-y (point-y origin)))
    (let ((x (- object-x origin-x))
          (y (- object-y origin-y)))
      (setf (point-x object) (+ (%new-x angle x y) origin-x)
            (point-y object) (+ (%new-y angle x y) origin-y))))
  (values))

(defmethod %rotate ((object close-bsp) origin angle)
  (values))

(defmethod %rotate ((object drawing-command) (origin point) (angle real))
  (%rotate (points object) origin angle))

(defmethod %rotate ((object drawing-commands) (origin point) (angle real))
  (%rotate (drawing-commands object) origin angle))

(defmethod %rotate ((object cons) (origin point) (angle real))
  (let ((container (etypecase object
                     ((cons drawing-command) object)
                     ((cons point) object))))
    (loop for item in container
          do (%rotate item origin angle))))

(defun rotate-origin (object angle &optional (x 0) (y 0))
  (let ((origin (make-instance 'point :x x :y y)))
    (%rotate object origin angle)
    (values object)))

(defun rotate-center (object angle)
  (destructuring-bind (min-x max-x min-y max-y) (minmax-xy object)
    (let ((x (- max-x (/ (- max-x min-x) 2.0)))
          (y (- max-y (/ (- max-y min-y) 2.0))))
      (rotate-origin object angle x y))))

