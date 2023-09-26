(cl:in-package #:clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FFI - CHAR to DRAWING COMMANDS
;;;
(defun char-to-drawing-commands
    (char &key (rawp t) (face *face*))
  (let ((string
          (with-output-to-string (stream)
            (let ((lastx nil)
                  (lasty nil))
              (freetype2:do-char-decompose
                  (face char)
                  (op &rest points)
                (case op
                  (:moveto (princ "m " stream))
                  (:lineto (princ "l " stream))
                  (:conicto
                   (princ "b " stream)
                   (princ lastx stream)
                   (princ #\Space stream)
                   (princ lasty stream)
                   (princ #\Space stream))
                  (t (princ op stream)))
                (loop for v in (reverse points)
                      unless (null v)
                        do (princ (freetype2::ft-vector-x v) stream)
                           (princ #\Space stream)
                           (princ (freetype2::ft-vector-y v) stream)
                           (princ #\Space stream)
                           (setf lastx (freetype2::ft-vector-x v))
                           (setf lasty (freetype2::ft-vector-y v))))))))
    (let ((dcs (parse-drawing-commands string)))
      (negate-y dcs)
      (destructuring-bind (mx my) (min-xy dcs)
        (moving-xy dcs (- mx) (- my)))
      (resize-xy dcs 1/4)
      (if (null rawp)
          (drawing-commands-string dcs)
          (values dcs)))))

