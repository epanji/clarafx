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
    (multiple-value-bind (indexes1 times1 partials1)
        (loop for batch in (stable-sort (copy-seq (overrides object)) '< :key 'index)
              for (kara pred) = (list (and (typep batch 'claraoke-text:batch)
                                           (increase-karaoke batch 0))
                                      (not (null (find-partial batch))))
              unless (null kara)
                collect (index batch) into indexes2
                and collect (arg1 kara) into times2
                and collect pred into partials2
              finally (return (values indexes2 times2 partials2)))
      ;; Prevent null KARAOKE
      (when (and (null indexes1)
                 (null times1))
        (push 0 indexes1)
        (push 15 times1)
        (push nil partials1))
      (loop for time in times1
            for partialp in partials1
            for (index1 index2) on indexes1
            collect (prog1 (make-syllable (subseq string index1 index2)
                                          :fontspace fontspace
                                          :face face
                                          :partialp partialp
                                          :start (+ counter start)
                                          :duration time
                                          :line line)
                      (incf counter time))))))

(defun calculate-layout-width (canvas)
  (declare (type canvas canvas))
  (- (width canvas) (+ (margin-right canvas) (margin-left canvas))))

(defun calculate-layout-height (canvas)
  (declare (type canvas canvas))
  (- (height canvas) (+ (margin-top canvas) (margin-bottom canvas))))

(defun force-split-indexes (object)
  (declare (type (or list claraoke-subtitle:dialogue) object))
  (let ((overrides (if (listp object) object (overrides object))))
    (loop for override in overrides
          for index = (typecase override
                        (claraoke-text:batch
                         (let ((newline (find-modifier override 'newline)))
                           (unless (null newline)
                             (index override))))
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

(defun split-dialogue-multiple-line
    (object &key layout-width verticalp (fontspace 0) (face *face*))
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
          for string-width = (let ((new-string (subseq string1 counter2 end2)))
                               (if verticalp
                                   (+ (* (string-pixel-height new-string :face face)
                                         (length new-string))
                                      (* fontspace (length new-string)))
                                   (string-pixel-width new-string
                                                       :fontspace fontspace
                                                       :face face)))
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
                                                                  (max 0 (1- start2)))))
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

(defun split-dialogue (object &key layout-width verticalp (fontspace 0) (face *face*))
  (declare (type claraoke-subtitle:dialogue object))
  (let* ((string (.text (.text object)))
         (string-width (if verticalp
                           (+ (* (string-pixel-height string :face face)
                                 (length string))
                              (* fontspace (length string)))
                           (string-pixel-width string :fontspace fontspace :face face)))
         (dialogues (list object)))
    (when (or (and (integerp layout-width)
                   (> string-width layout-width))
              (not (null (force-split-indexes object))))
      (setf dialogues (split-dialogue-multiple-line object :layout-width layout-width
                                                           :fontspace fontspace
                                                           :face face
                                                           :verticalp verticalp)))
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
                           append (loop with i = 0 and n = (length syllables)
                                        for ,var-name in syllables
                                        append (progn (setf (style ,var-name)
                                                            (find-style ,subtitle ,style-name))
                                                      (setf (dialogue ,var-name) ,dialogue)
                                                      (setf (canvas ,var-name) ,canvas)
                                                      (setf (index-in-line ,var-name) i)
                                                      (setf (count-in-line ,var-name) n)
                                                      (incf i)
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
   (invisiblep :initarg :invisiblep :reader invisiblep :initform nil)
   (partialp :initarg :partialp :reader partialp :initform nil)
   (index-in-line :initform nil :accessor index-in-line)
   (count-in-line :initform nil :accessor count-in-line)
   (extra-dialogues :accessor extra-dialogues :initform '()))
  (:documentation "Drawable area with alignment inside canvas."))

(defmethod style ((object syllable) &key)
  (slot-value object 'style))

(defmethod dialogue ((object syllable) &key)
  (slot-value object 'dialogue))

(defun string-invisiblep (string)
  (or (zerop (length string))
      (and (= 1 (length string))
           (char= #\INVISIBLE_SEPARATOR (char string 0)))))

(defun make-syllable (string &key (start 0) (duration 15) (fontspace 0) (face *face*) (line 0) (partialp nil))
  (make-instance 'syllable
                 :h (string-pixel-height string :face face)
                 :w (string-pixel-width string :fontspace fontspace :face face)
                 :plain-text string
                 :invisiblep (string-invisiblep string)
                 :partialp partialp
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
  (let ((layout-width (if (digit-char-p (alignment-code code) 10)
                          (calculate-layout-width canvas)
                          (calculate-layout-height canvas)))
        (fontspace (fontspace canvas))
        (face (make-canvas-face canvas)))
    (make-instance
     'alignment
     :an code
     :canvas canvas
     :line-syllables
     (split-dialogue
      dialogue
      :layout-width layout-width
      :fontspace fontspace
      :face face
      :verticalp
      (case (alignment-code code)
        ((#\A
          #\B #\C
          #\D #\E #\F
          #\G #\H #\I
          #\J #\K
          #\L)
         t)
        (otherwise nil))))))

