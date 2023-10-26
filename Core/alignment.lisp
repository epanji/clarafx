(cl:in-package :clarafx.core)

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
              for kara = (and (typep batch 'claraoke-text:batch)
                              (increase-karaoke batch 0))
              unless (null kara)
                collect (index batch) into indexes2
                and collect (arg1 kara) into times2
              finally (return (values indexes2 times2)))
      ;; Prevent null KARAOKE
      (when (and (null indexes1)
                 (null times1))
        (push 0 indexes1)
        (push 15 times1))
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

(defun force-split-indexes (object)
  (declare (type (or list claraoke-subtitle:dialogue) object))
  (let ((overrides (if (listp object) object (overrides object))))
    (loop for override in overrides
          for index = (typecase override
                        (claraoke-text:batch
                         (let ((newline (find-modifier override 'newline)))
                           (unless (null newline)
                             (index newline))))
                        (claraoke-text:newline
                         (index override)))
          unless (null index)
            collect index)))

(defun newline-and-space-splitter (dialogue)
  (declare (type claraoke-subtitle:dialogue dialogue))
  (let ((string (.text (.text dialogue)))
        (indexes (force-split-indexes dialogue)))
    (let ((char-bag '(#\Space #\Newline))
          (length (length string))
          (result '())
          (start 0)
          (end 0))
      (loop (when (= length end)
              (push (list start end :normal) result)
              (return))
            (when (member end indexes)
              (push (list start end :force) result)
              (setf start end))
            (when (and (/= start end)
                       (member (char string end) char-bag))
              (push (list start end :normal) result)
              (setf start (1+ end)))
            (incf end))
      (reverse result))))

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
          for (start2 end2 mark) in (newline-and-space-splitter object)
          for string-width = (string-pixel-width (subseq string1 counter2 end2)
                                                 :fontspace fontspace
                                                 :face face)
          when (or (> string-width layout-width)
                   (eql mark :force)
                   (= end2 (length string1)))
            collect (progn
                      ;; Mutate start when force split or last in loop
                      (when (or (eql mark :force)
                                (= end2 (length string1)))
                        (setf start2 end2))
                      ;; Filter overrides
                      (let ((string2 (subseq string1 counter2 (if (eql mark :force)
                                                                  start2
                                                                  (1- start2))))
                            (overrides2 (remove-if
                                         (lambda (index)
                                           (not (< (1- counter2) index start2)))
                                         overrides1 :key 'index))
                            (ktime 15))
                        ;; Mutate overrides
                        (mapc (lambda (override)
                                (decf (index override) counter2))
                              overrides2)
                        (setf counter2 start2)
                        ;; Prevent null KARAOKE
                        (when (or (null overrides2)
                                  (null (and (typep (first overrides2) 'claraoke-text:batch)
                                             (increase-karaoke (first overrides2) 0))))
                          (setf overrides2 (list (override 'karaoke 0 :arg1 ktime))))
                        ;; Result follow by duration counter
                        (prog1 (dialogue
                                string2
                                :overrides overrides2
                                :start (+ start1 counter1)
                                :end end1)
                          (loop for batch in overrides2
                                for kara = (and (typep batch 'claraoke-text:batch)
                                                (increase-karaoke batch 0))
                                unless (null kara)
                                  do (incf counter1 (arg1 kara))
                                     (setf ktime (arg1 kara)))))))))

(defun split-dialogue (object &key layout-width (fontspace 0) (face *face*))
  (declare (type claraoke-subtitle:dialogue object))
  (let* ((string (.text (.text object)))
         (string-width (string-pixel-width string :fontspace fontspace :face face))
         (dialogues (list object)))
    (when (or (and (integerp layout-width)
                   (> string-width layout-width))
              (not (null (force-split-indexes object))))
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
  (let ((canvas (gensym "WESFK"))
        (alignment (gensym "WESFK"))
        (result (gensym "WESFK")))
    `(let* ((,canvas (make-canvas ,subtitle ,style-name ,dpi))
            (,alignment (make-alignment ,alignment-code
                                        ,canvas
                                        ,dialogue))
            (,result (loop for syllables in (line-syllables ,alignment)
                           append (loop for ,var-name in syllables
                                        append (progn (setf (style ,var-name)
                                                            (find-style ,subtitle ,style-name))
                                                      (setf (dialogue ,var-name) ,dialogue)
                                                      (setf (canvas ,var-name) ,canvas)
                                                      ,@body)))))
       (values (reverse ,result) ,alignment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syllable
;;;
(defclass syllable (rectangle-area)
  ((plain-text :initarg :plain-text :reader plain-text :initform "-")
   (start :initarg :start :reader start :initform nil)
   (duration :initarg :duration :reader duration :initform nil)
   (line :initarg :line :reader line :initform 0)
   (style :writer (setf style) :initform nil)
   (dialogue :writer (setf dialogue) :initform nil)
   (canvas :accessor canvas :initform nil)
   (origin-start :accessor origin-start :initform nil)
   (origin-end :accessor origin-end :initform nil)
   (extra-dialogues :accessor extra-dialogues :initform '()))
  (:documentation "Drawable area with alignment inside canvas."))

(defmethod style ((object syllable) &key)
  (slot-value object 'style))

(defmethod dialogue ((object syllable) &key)
  (slot-value object 'dialogue))

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
                    (setf (point-y1 syllable) (+ ymax (* (line syllable) (height syllable))))
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
                    (setf (point-y1 syllable) (+ ymax (* (line syllable) (height syllable))))
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
                    (setf (point-y1 syllable) (+ ymax (* (line syllable) (height syllable))))
                    (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                    ;; xcnt
                    (setf xcnt (+ xcnt (width syllable))))))))))

