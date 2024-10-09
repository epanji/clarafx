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
;;; R.G.B variant range colors
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

(macrolet ((define-color-range (name (p0 p1 p2))
             (declare (type (integer 0 2) p0 p1 p2))
             (let ((fn '(claraoke:red claraoke:green claraoke:blue))
                   (sn '(r g b)))
               `(defun ,name (from to &optional (amount 3))
                  (let* ((c0 (claraoke:color from))
                         (c1 (claraoke:color to))
                         (sum0 (%sum-elements (,(nth p0 fn) c0)
                                              (,(nth p1 fn) c0)
                                              (,(nth p2 fn) c0)))
                         (sum1 (%sum-elements (,(nth p0 fn) c1)
                                              (,(nth p1 fn) c1)
                                              (,(nth p2 fn) c1)))
                         (range-by (let ((by (/ (- sum1 sum0) (max 1 (1- amount)))))
                                     (if (minusp by)
                                         (floor by)
                                         (ceiling by))))
                         (pred (if (minusp range-by) #'max #'min)))
                    (loop for i from 0 below (max 2 amount)
                          collect (let ((sum (funcall pred sum1 (+ sum0 (* i range-by)))))
                                    (destructuring-bind (,(nth p0 sn)
                                                         ,(nth p1 sn)
                                                         ,(nth p2 sn))
                                        (%the-elements sum)
                                      (claraoke:rgb ,(nth 0 sn)
                                                    ,(nth 1 sn)
                                                    ,(nth 2 sn))))))))))
  (define-color-range rgb-range-colors (0 1 2))
  (define-color-range rbg-range-colors (0 2 1))
  (define-color-range grb-range-colors (1 0 2))
  (define-color-range gbr-range-colors (1 2 0))
  (define-color-range brg-range-colors (2 0 1))
  (define-color-range bgr-range-colors (2 1 0)))

