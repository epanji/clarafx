(cl:in-package :clarafx.draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing Command
;;;
(defclass drawing-command ()
  ((name
    :initarg :name
    :initform (error "Required NAME.")
    :accessor name)
   (points
    :initform '()
    :accessor points)))

(defun drawing-command-string (object)
  (declare (type drawing-command object))
  (with-output-to-string (stream)
    (princ (name object) stream)
    (princ #\Space stream)
    (loop for (point next) on (points object)
          do (princ (point-string point) stream)
             (unless (null next)
               (princ #\Space stream)))))

(defmethod print-object ((object drawing-command) stream)
  (princ "#<" stream)
  (princ (drawing-command-string object) stream)
  (princ ">" stream))

;; getter

(defmethod point-x ((object drawing-command) &optional (n 0))
  (let ((point (nth n (points object))))
    (point-x point)))

(defmethod point-y ((object drawing-command) &optional (n 0))
  (let ((point (nth n (points object))))
    (point-y point)))

;; setter

(defmethod (setf point-x)
    (new-value (object drawing-command) &optional (n 0))
  (let ((point (nth n (points object))))
    (setf (point-x point) new-value)))

(defmethod (setf point-y)
    (new-value (object drawing-command) &optional (n 0))
  (let ((point (nth n (points object))))
    (setf (point-y point) new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Move (m)
;;;
(defclass move (drawing-command)
  ()
  (:default-initargs :name "m"))

(defun make-dc-move (x1 y1)
  (declare (type number x1 y1))
  (let ((p1 (make-instance 'point :x x1 :y y1))
        (move (make-instance 'move)))
    (setf (points move) (list p1))
    move))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line (l)
;;;
(defclass line (drawing-command)
  ()
  (:default-initargs :name "l"))

(defun make-dc-line (x1 y1)
  (declare (type number x1 y1))
  (let ((p1 (make-instance 'point :x x1 :y y1))
        (line (make-instance 'line)))
    (setf (points line) (list p1))
    line))

(defun make-dc-lines (x1 y1 &rest xnyn)
  (let ((line (make-dc-line x1 y1))
        (lines (loop for (x y) on xnyn by 'cddr
                     collect (make-dc-line x y))))
    (list* line lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bezier (b)
;;;
(defclass bezier (drawing-command)
  ()
  (:default-initargs :name "b"))

(defun make-dc-bezier (x1 y1 x2 y2 x3 y3)
  (declare (type number x1 y1 x2 y2 x3 y3))
  (let ((p1 (make-instance 'point :x x1 :y y1))
        (p2 (make-instance 'point :x x2 :y y2))
        (p3 (make-instance 'point :x x3 :y y3))
        (bezier (make-instance 'bezier)))
    (setf (points bezier) (list p1 p2 p3))
    bezier))

(defun make-dc-beziers (x1 y1 x2 y2 x3 y3 &rest xnyn)
  (flet ((cddddddr (list)
           (cddr (cddddr list))))
    (let ((bezier (make-dc-bezier x1 y1 x2 y2 x3 y3))
          (beziers (loop for (bx1 by1 bx2 by2 bx3 by3) on xnyn by #'cddddddr
                         collect (make-dc-bezier bx1 by1 bx2 by2 bx3 by3))))
      (list* bezier beziers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; No Close Move (n)
;;;
(defclass no-close-move (drawing-command)
  ()
  (:default-initargs :name "n"))

(defun make-dc-no-close-move (x1 y1)
  (declare (type number x1 y1))
  (let ((p1 (make-instance 'point :x x1 :y y1))
        (no-close-move (make-instance 'no-close-move)))
    (setf (points no-close-move) (list p1))
    no-close-move))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cubic Bezier Spline (s)
;;;
(defclass cubic-bsp (drawing-command)
  ()
  (:default-initargs :name "s"))

(defun make-dc-cubic-bsp (x1 y1 x2 y2 x3 y3 &rest xnyn)
  (declare (type number x1 y1 x2 y2 x3 y3))
  (let ((p1 (make-instance 'point :x x1 :y y1))
        (p2 (make-instance 'point :x x2 :y y2))
        (p3 (make-instance 'point :x x3 :y y3))
        (pn (loop for (xn yn) on xnyn by 'cddr
                  unless (or (null xn) (null yn))
                    collect (make-instance 'point :x xn :y yn)))
        (cubic-bsp (make-instance 'cubic-bsp)))
    (setf (points cubic-bsp) (list* p1 p2 p3 pn))
    cubic-bsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extend Bezier Spline (p)
;;;
(defclass extend-bsp (drawing-command)
  ()
  (:default-initargs :name "p"))

(defun make-dc-extend-bsp (x1 y1)
  (declare (type number x1 y1))
  (let ((p1 (make-instance 'point :x x1 :y y1))
        (extend-bsp (make-instance 'extend-bsp)))
    (setf (points extend-bsp) (list p1))
    extend-bsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Close Bezier Spline (c)
;;;
(defclass close-bsp (drawing-command)
  ((points :initform nil))
  (:default-initargs :name "c"))

(defun make-dc-close-bsp ()
  (make-instance 'close-bsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing Commands
;;;
(defclass drawing-commands ()
  ((container :initform '() :accessor drawing-commands)))

(defun drawing-commands-string (object)
  (declare (type drawing-commands object))
  (with-output-to-string (stream)
    (loop with prev-name = nil
          for drawing-command in (drawing-commands object)
          do (let ((name (name drawing-command))
                   (points (points drawing-command)))
               (unless (string-equal prev-name name)
                 (unless (null prev-name)
                   (princ #\Space stream))
                 (princ name stream))
               (unless (null points)
                 (princ #\Space stream)
                 (loop for (point next) on points
                       do (princ (point-string point) stream)
                          (unless (null next)
                            (princ #\Space stream))))
               (setf prev-name name)))))

(defmethod print-object ((object drawing-commands) stream)
  (princ "#<DCS " stream)
  (princ (drawing-commands-string object) stream)
  (princ ">" stream))

(defmethod add-drawing-command :before
    ((object drawing-commands) (item drawing-command))
  (let ((container (drawing-commands object)))
    (when (null container)
      (unless (eql 'move (class-name (class-of item)))
        (error "First command should be MOVE.")))))

(defmethod add-drawing-command
    ((object drawing-commands) (item drawing-command))
  (let ((container (reverse (drawing-commands object))))
    (setf (drawing-commands object)
          (reverse (cons item container))))
  object)

