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

