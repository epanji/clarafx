(cl:in-package :clarafx.draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Point x, y
;;;
(defgeneric point-x (object &optional n)
  (:method ((object null) &optional n)
    (declare (ignore n))
    nil))

(defgeneric point-y (object &optional n)
  (:method ((object null) &optional n)
    (declare (ignore n))
    nil))

;; class

(defclass point ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)))

(defun point-string (point)
  (declare (type point point))
  (with-output-to-string (stream)
    (princ (slot-value point 'x) stream)
    (princ #\Space stream)
    (princ (slot-value point 'y) stream)))

(defmethod print-object ((object point) stream)
  (princ "#<" stream)
  (princ (point-string object) stream)
  (princ ">" stream))

;; getters

(defmethod point-x ((object point) &optional (n 0))
  (when (zerop n)
    (slot-value object 'x)))

(defmethod point-y ((object point) &optional (n 0))
  (when (zerop n)
    (slot-value object 'y)))

;; setters

(defmethod (setf point-x)
    (new-value (object point) &optional (n 0))
  (when (zerop n)
    (setf (slot-value object 'x) new-value)))

(defmethod (setf point-y)
    (new-value (object point) &optional (n 0))
  (when (zerop n)
    (setf (slot-value object 'y) new-value)))

