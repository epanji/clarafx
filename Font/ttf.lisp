(cl:in-package :claraoke-font)

;;; For current dev purpose only
;;;
;;; /usr/share/fonts/TTF/LiberationSans-Bold.ttf
;;;
(defparameter *font-loader*
  (zpb-ttf:open-font-loader
   #p"/usr/share/fonts/TTF/LiberationSans-Bold.ttf"))

(defparameter *scale-tolerance* -167)

(defun show-font (&optional (font *font-loader*))
  (let* ((bbox (zpb-ttf:bounding-box font))
         (xmin (zpb-ttf:xmin bbox))
         (xmax (zpb-ttf:xmax bbox))
         (ymin (zpb-ttf:ymin bbox))
         (ymax (zpb-ttf:ymax bbox))
         (desc  (zpb-ttf:descender font))
         (asce  (zpb-ttf:ascender font)))
    (values xmin xmax
            ymin ymax
            desc asce
            (/ asce (abs desc)))))

(defun pixel-scale (size &key (font *font-loader*))
  (let ((asc (abs (zpb-ttf:ascender font)))
        (des (abs (zpb-ttf:descender font)))
        (gap (abs (zpb-ttf:line-gap font))))
    (/ (+ asc des gap *scale-tolerance*) size)
    ;; (/ asc size)
    ))

(defun string-pixel-width (string &key (size 16) (space 0) (font *font-loader*))
  (let ((scale (pixel-scale size :font font))
        (max (1- (length string)))
        (char nil)
        (char-width 0)
        (glyph nil)
        (prev-glyph nil)
        (index 0)
        (result 0))
    (loop (when (< max index) (return result))
          (setf char (aref string index))
          (setf glyph (zpb-ttf:find-glyph char font))
          (setf char-width (+ ;; (zpb-ttf:xmin glyph)
                              (zpb-ttf:advance-width glyph)
                              ;; Assume kerning always true
                              (if (null prev-glyph)
                                  0
                                  (zpb-ttf:kerning-offset prev-glyph glyph font))
                              ;; Font spacing
                              (* space scale)))
          (setf result (+ result char-width))
          ;; (format t "~&~A ~A ~C~%" result char-width char)
          (setf prev-glyph glyph)
          (incf index))
    (values (ceiling result scale) result)))

;; (defclass font-holder ()
;;   ((font-designator
;;     :initarg :font
;;     :initform "/usr/share/fonts/TTF/LiberationSans-Bold.ttf"
;;     :accessor font-designator)
;;    (zpb-ttf-font
;;     :reader font-loader)
;;    (scale-tolerance
;;     :initarg :tolerance
;;     :initform -8
;;     :accessor scale-tolerance)))

;; (zpb-ttf:with-font-loader )

;;; loader from name
;;; TODO (software-type)
(defun font-from-name (string)
  (let* ((ttf (uiop:run-program (format nil "fc-list | grep ~S.ttf" string)
                                :ignore-error-status t
                                :output '(:string :stripped t)))
         (pathspec (subseq ttf 0 (position #\: ttf))))
    (if (zerop (length pathspec))
        (values *font-loader* :fallback)
        (values (zpb-ttf:open-font-loader (pathname pathspec)) :success))))

