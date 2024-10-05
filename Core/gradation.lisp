(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gradation colors
;;;
(defun gradation-colors (from to &optional (amount 3))
  (let ((c0 (claraoke:color from))
        (c1 (claraoke:color to))
        (sf (/ 1 (max 1 (1- amount)))))
    (flet ((icolor (isf)
             (claraoke:rgb (round (+ (claraoke:red c0)
                                     (* isf (- (claraoke:red c1)
                                               (claraoke:red c0)))))
                           (round (+ (claraoke:green c0)
                                     (* isf (- (claraoke:green c1)
                                               (claraoke:green c0)))))
                           (round (+ (claraoke:blue c0)
                                     (* isf (- (claraoke:blue c1)
                                               (claraoke:blue c0))))))))
      (loop for i from 0 below (max 2 amount)
            collect (icolor (* i sf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RGB & BGR range colors
;;;
(defun %sum-elements (element0 element1 element2)
  (declare (type (integer 0 255) element0 element1 element2))
  (loop with elements = (list element2 element1 element0)
        for i from 0
        for element in elements
        sum (* element (expt 256 i))))

(defun %the-elements (sum-elements)
  (declare (type unsigned-byte sum-elements))
  (let (result)
    (reduce (lambda (number divisor)
              (multiple-value-bind (quotient remainder)
                  (floor number divisor)
                (push quotient result) remainder))
            (list (expt 256 2) (expt 256 1) (expt 256 0))
            :initial-value (mod sum-elements (expt 256 3)))
    (reverse result)))

(defun rgb-range-colors (from to &optional (amount 3))
  (let* ((c0 (claraoke:color from))
         (c1 (claraoke:color to))
         (sum0 (%sum-elements (claraoke:red c0)
                              (claraoke:green c0)
                              (claraoke:blue c0)))
         (sum1 (%sum-elements (claraoke:red c1)
                              (claraoke:green c1)
                              (claraoke:blue c1)))
         (range-by (let ((by (/ (- sum1 sum0) (max 1 (1- amount)))))
                     (if (minusp by)
                         (floor by)
                         (ceiling by))))
         (pred (if (minusp range-by) #'max #'min)))
    (loop for i from 0 below (max 2 amount)
          collect (let ((sum (funcall pred sum1 (+ sum0 (* i range-by)))))
                    (destructuring-bind (r g b)
                        (%the-elements sum)
                      (claraoke:rgb r g b))))))

(defun bgr-range-colors (from to &optional (amount 3))
  (let* ((c0 (claraoke:color from))
         (c1 (claraoke:color to))
         (sum0 (%sum-elements (claraoke:blue c0)
                              (claraoke:green c0)
                              (claraoke:red c0)))
         (sum1 (%sum-elements (claraoke:blue c1)
                              (claraoke:green c1)
                              (claraoke:red c1)))
         (range-by (let ((by (/ (- sum1 sum0) (max 1 (1- amount)))))
                     (if (minusp by)
                         (floor by)
                         (ceiling by))))
         (pred (if (minusp range-by) #'max #'min)))
    (loop for i from 0 below (max 2 amount)
          collect (let ((sum (funcall pred sum1 (+ sum0 (* i range-by)))))
                    (destructuring-bind (b g r)
                        (%the-elements sum)
                      (claraoke:rgb r g b))))))

