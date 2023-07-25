(cl:in-package :claraoke-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Splitter
;;;
(defun split-dialogue (object &optional (size 16) (space 0) (font *font-loader*))
  (declare (type claraoke-subtitle:dialogue object))
  (let ((start (claraoke:durationinteger (claraoke:start object)))
        (string (claraoke:.text (claraoke:.text object)))
        (counter 0))
    (multiple-value-bind (idx tms)
        (loop for batch in (claraoke:overrides object)
              for kara = (claraoke:increase-karaoke batch 0)
              unless (null kara)
                collect (claraoke:index batch) into indexes
                and collect (claraoke:arg1 kara) into times
              finally (return (values indexes times)))
      (loop for tm in tms
            for (p1 p2) on idx
            collect (prog1 (make-syllable (subseq string p1 p2)
                                          :size size
                                          :space space
                                          :font font
                                          :start (+ counter start)
                                          :duration tm)
                      (incf counter tm))))))

(defmacro with-every-syllable-from-karaoke
    ((var-name dialogue subtitle alignment-code style-name) &body body)
  (let ((alignment (gensym "ALG"))
        (result (gensym "RES")))
    `(let* ((,alignment (make-alignment ,alignment-code
                                        (make-canvas ,subtitle ,style-name)
                                        ,dialogue))
            (,result (loop for ,var-name in (syllables ,alignment)
                           collect (progn ,@body))))
       (values ,result ,alignment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syllable
;;;
(defclass syllable (rectangle-area)
  ((plain-text :initarg :plain-text :reader plain-text :initform "-")
   (start :initarg :start :reader start :initform nil)
   (duration :initarg :duration :reader duration :initform nil))
  (:documentation "Drawable area with alignment inside canvas."))

(defun make-syllable
    (string &key (start 0) (duration 15) (size 16) (space 0) (font *font-loader*))
  (make-instance 'syllable
                 :h size
                 :w (string-pixel-width string :size size :space space :font font)
                 :plain-text string
                 :start start
                 :duration duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alignment
;;;
(defclass alignment ()
  ((code :initarg :an :reader alignment-code :initform (error "Required :an"))
   (canvas :initarg :canvas :reader canvas :initform (error "Required :canvas"))
   (syllables :initarg :syllables :reader syllables)))

(defmethod initialize-instance :before ((instance alignment) &key canvas)
  (check-type canvas canvas))

(defmethod initialize-instance :after ((instance alignment) &key)
  (calculate-virtual-alignment-1 instance))

(defmethod (setf alignment-code) (new-value (instance alignment))
  (setf (slot-value instance 'code) new-value)
  (calculate-virtual-alignment-1 instance))

;;; TODO
(defun make-alignment* (code
                        width height
                        margin-top margin-left
                        margin-bottom margin-right
                        dialogue
                        &optional
                          fontname
                          fontsize
                          fontspace)
  (let ((canvas (make-canvas* width height
                              margin-top margin-left
                              margin-bottom margin-right
                              fontname fontsize fontspace)))
    (make-alignment code canvas dialogue)))

;;; TODO font-loader from name
(defun make-alignment (code canvas dialogue)
  (make-instance 'alignment
                 :an code
                 :canvas canvas
                 :syllables (split-dialogue
                             dialogue
                             (fontsize canvas)
                             (fontspace canvas)
                             (font-from-name (fontname canvas)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual alignment
;;;
(defun calculate-virtual-alignment-1 (alignment)
  "Calculate coordinates every syllables with origin equal to {\\an1}"
  (with-accessors ((an alignment-code)
                   (canvas canvas)
                   (syllables syllables))
      alignment
    (let ((total-width (loop for syllable in syllables
                             sum (width syllable))))
      (ecase an
        ((1 :left-bottom :bottom-left)
         (let ((xmin (margin-left canvas))
               (ymax (- (height canvas) (margin-bottom canvas)))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) (- ymax (height syllable)))
             (setf (point-y2 syllable) ymax)
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((2 :center-bottom :bottom-center)
         (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
               (ymax (- (height canvas) (margin-bottom canvas)))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) (- ymax (height syllable)))
             (setf (point-y2 syllable) ymax)
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((3 :right-bottom :bottom-right)
         (let ((xmin (- (width canvas) (margin-right canvas) total-width))
               (ymax (- (height canvas) (margin-bottom canvas)))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) (- ymax (height syllable)))
             (setf (point-y2 syllable) ymax)
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((4 :left-middle :middle-left)
         (let ((xmin (margin-left canvas))
               (ymax (truncate (height canvas) 2))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) ymax)
             (setf (point-y2 syllable) (+ ymax (truncate (height syllable) 2)))
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((5 :center-middle :middle-center)
         (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
               (ymax (truncate (height canvas) 2))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) ymax)
             (setf (point-y2 syllable) (+ ymax (truncate (height syllable) 2)))
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((6 :right-middle :middle-right)
         (let ((xmin (- (width canvas) (margin-right canvas) total-width))
               (ymax (truncate (height canvas) 2))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) ymax)
             (setf (point-y2 syllable) (+ ymax (truncate (height syllable) 2)))
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((7 :left-top :top-left)
         (let ((xmin (margin-left canvas))
               (ymax (margin-top canvas))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) ymax)
             (setf (point-y2 syllable) (+ ymax (height syllable)))
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((8 :center-top :top-center)
         (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
               (ymax (margin-top canvas))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) ymax)
             (setf (point-y2 syllable) (+ ymax (height syllable)))
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))
        ((9 :right-top :top-right)
         (let ((xmin (- (width canvas) (margin-right canvas) total-width))
               (ymax (margin-top canvas))
               (xcnt 0))
           (dolist (syllable syllables)
             (setf (point-x1 syllable) (+ xmin xcnt))
             (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
             (setf (point-y1 syllable) ymax)
             (setf (point-y2 syllable) (+ ymax (height syllable)))
             ;; xcnt
             (setf xcnt (+ xcnt (width syllable))))))))))

