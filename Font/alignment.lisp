(cl:in-package :clarafx-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Splitter
;;;
(defun split-dialogue-single-line (object &key (line 0) (fontspace 0) (face *face*))
  (declare (type claraoke-subtitle:dialogue object))
  (let ((start (durationinteger (start object)))
        (string (.text (.text object)))
        (counter 0))
    (multiple-value-bind (indexes1 times1)
        (loop for batch in (overrides object)
              for kara = (increase-karaoke batch 0)
              unless (null kara)
                collect (index batch) into indexes2
                and collect (arg1 kara) into times2
              finally (return (values indexes2 times2)))
      (loop for time in times1
            for (index1 index2) on indexes1
            collect (prog1 (make-syllable (subseq string index1 index2)
                                          :fontspace fontspace
                                          :face face
                                          :start (+ counter start)
                                          :duration time
                                          :line line)
                      (incf counter time))))))

(defun calculate-layout-width (canvas)
  (declare (type canvas canvas))
  (- (width canvas) (+ (margin-right canvas) (margin-left canvas))))

(defun split-dialogue-multiple-line (object &key layout-width (fontspace 0) (face *face*))
  (declare (type claraoke-subtitle:dialogue object)
           (type integer layout-width))
  ;; Duplicate first to avoid mutations
  (setf object (claraoke-subtitle:dialogue-from-string
                (with-output-to-string (stream)
                  (print-script object stream))))
  (let ((string1 (.text (.text object)))
        (overrides1 (overrides object))
        (start1 (durationinteger (start object)))
        (end1 (durationinteger (end object)))
        (counter1 0))
    (loop with counter2 = 0
          for (start2 end2) in (claraoke-internal:split-by-char #\Space string1 0 nil)
          for string-width = (string-pixel-width (subseq string1 counter2 end2)
                                                 :fontspace fontspace
                                                 :face face)
          when (or (> string-width layout-width) (null end2))
            collect (progn
                      ;; String length for start with null end
                      (when (null end2)
                        (setf start2 (length string1)))
                      ;; Filter overrides
                      (let ((string2 (subseq string1 counter2 (1- start2)))
                            (overrides2 (remove-if
                                         (lambda (index)
                                           (not (< (1- counter2) index start2)))
                                         overrides1 :key 'index)))
                        ;; Mutate overrides
                        (mapc (lambda (override)
                                (decf (index override) counter2))
                              overrides2)
                        (setf counter2 start2)
                        ;; Result follow by duration counter
                        (prog1 (dialogue
                                string2
                                :overrides overrides2
                                :start (+ start1 counter1)
                                :end end1)
                          (loop for batch in overrides2
                                for kara = (increase-karaoke batch 0)
                                unless (null kara)
                                  do (incf counter1 (arg1 kara)))))))))

(defun split-dialogue (object &key layout-width (fontspace 0) (face *face*))
  (declare (type claraoke-subtitle:dialogue object))
  (let* ((string (.text (.text object)))
         (string-width (string-pixel-width string :fontspace fontspace :face face))
         (dialogues (list object)))
    (when (and (integerp layout-width) (> string-width layout-width))
      (setf dialogues (split-dialogue-multiple-line object :layout-width layout-width
                                                           :fontspace fontspace
                                                           :face face)))
    (loop for dialogue in dialogues
          for line from 0
          collect (split-dialogue-single-line dialogue :line line
                                                       :fontspace fontspace
                                                       :face face))))

(defmacro with-every-syllable-from-karaoke
    ((var-name dialogue subtitle alignment-code style-name dpi) &body body)
  (let ((alignment (gensym "WESFK"))
        (result (gensym "WESFK")))
    `(let* ((,alignment (make-alignment ,alignment-code
                                        (make-canvas ,subtitle ,style-name ,dpi)
                                        ,dialogue))
            (,result (loop for syllables in (line-syllables ,alignment)
                           append (loop for ,var-name in syllables
                                        collect (progn ,@body)))))
       (values ,result ,alignment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syllable
;;;
(defclass syllable (rectangle-area)
  ((plain-text :initarg :plain-text :reader plain-text :initform "-")
   (start :initarg :start :reader start :initform nil)
   (duration :initarg :duration :reader duration :initform nil)
   (line :initarg :line :reader line :initform 0))
  (:documentation "Drawable area with alignment inside canvas."))

(defun make-syllable (string &key (start 0) (duration 15) (fontspace 0) (face *face*) (line 0))
  (make-instance 'syllable
                 :h (string-pixel-height string :face face)
                 :w (string-pixel-width string :fontspace fontspace :face face)
                 :plain-text string
                 :start start
                 :duration duration
                 :line line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alignment
;;;
(defclass alignment ()
  ((code :initarg :an :reader alignment-code :initform (error "Required :an"))
   (canvas :initarg :canvas :reader canvas :initform (error "Required :canvas"))
   (line-syllables :initarg :line-syllables :reader line-syllables)))

(defmethod initialize-instance :before ((instance alignment) &key canvas)
  (check-type canvas canvas))

(defmethod initialize-instance :after ((instance alignment) &key)
  (calculate-virtual-alignment-1 instance))

(defmethod (setf alignment-code) (new-value (instance alignment))
  (setf (slot-value instance 'code) new-value)
  (calculate-virtual-alignment-1 instance))

(defun make-alignment* (code
                        width height
                        margin-top margin-left
                        margin-bottom margin-right
                        dialogue
                        &optional
                          fontname fontsize fontspace
                          bold italic dpi)
  (let ((canvas (make-canvas* width height
                              margin-top margin-left
                              margin-bottom margin-right
                              fontname fontsize fontspace
                              bold italic dpi)))
    (make-alignment code canvas dialogue)))

(defun make-alignment (code canvas dialogue)
  (let ((layout-width (calculate-layout-width canvas))
        (fontspace (fontspace canvas))
        (face (make-canvas-face canvas)))
    (make-instance 'alignment
                   :an code
                   :canvas canvas
                   :line-syllables
                   (split-dialogue dialogue :layout-width layout-width
                                            :fontspace fontspace
                                            :face face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual alignment
;;;
(defun calculate-virtual-alignment-1 (alignment)
  "Calculate coordinates every syllables with origin equal to {\\an1}"
  (with-accessors ((an alignment-code)
                   (canvas canvas)
                   (lines line-syllables))
      alignment
    (loop with below-line = (1- (length lines))
          for syllables in lines
          for total-width = (loop for syllable in syllables
                                  sum (width syllable))
          do (ecase an
               ((1 :left-bottom :bottom-left)
                (let ((xmin (margin-left canvas))
                      (ymax (- (height canvas) (margin-bottom canvas)))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (- ymax (* (- (1+ below-line) (line syllable)) (height syllable))))
                    (setf (point-y2 syllable) (- ymax (* (- below-line (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((2 :center-bottom :bottom-center)
                (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
                      (ymax (- (height canvas) (margin-bottom canvas)))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (- ymax (* (- (1+ below-line) (line syllable)) (height syllable))))
                    (setf (point-y2 syllable) (- ymax (* (- below-line (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((3 :right-bottom :bottom-right)
                (let ((xmin (- (width canvas) (margin-right canvas) total-width))
                      (ymax (- (height canvas) (margin-bottom canvas)))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (- ymax (* (- (1+ below-line) (line syllable)) (height syllable))))
                    (setf (point-y2 syllable) (- ymax (* (- below-line (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((4 :left-middle :middle-left)
                (let ((xmin (margin-left canvas))
                      (ymax (truncate (height canvas) 2))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                    (setf (point-y2 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((5 :center-middle :middle-center)
                (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
                      (ymax (truncate (height canvas) 2))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                    (setf (point-y2 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((6 :right-middle :middle-right)
                (let ((xmin (- (width canvas) (margin-right canvas) total-width))
                      (ymax (truncate (height canvas) 2))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                    (setf (point-y2 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((7 :left-top :top-left)
                (let ((xmin (margin-left canvas))
                      (ymax (margin-top canvas))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((8 :center-top :top-center)
                (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
                      (ymax (margin-top canvas))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))
               ((9 :right-top :top-right)
                (let ((xmin (- (width canvas) (margin-right canvas) total-width))
                      (ymax (margin-top canvas))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (setf (point-x1 syllable) (+ xmin xcnt))
                    (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                    (setf (point-y1 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))))))

