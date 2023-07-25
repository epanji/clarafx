(cl:in-package :claraoke)

(cl:defgeneric splitable-dialogue-p (dialogue))
(cl:defgeneric split-dialogue (subtitle dialogue))
(cl:defgeneric syllable-positions (subtitle dialogue))

(cl:export '(splitable-dialogue-p
             split-dialogue
             syllable-positions)
           :claraoke)

(cl:in-package :claraoke-font)

;;; TODO object-must-be-dialogue
;; (defmethod claraoke:splitable-dialogue-p (object)
;;   (error 'claraoke:object-must-be-event :object object))

;; (defmethod claraoke:splitable-dialogue-p ((object claraoke-subtitle:dialogue))
;;   (< 1 (length (claraoke:overrides object))))

;; ;;;
;; (defmethod claraoke:split-dialogue (subtitle dialogue))
;; (defmethod claraoke:split-dialogue ((subtitle claraoke-subtitle:subtitle) dialogue))
;; (defmethod claraoke:split-dialogue ((subtitle claraoke-subtitle:subtitle) (dialogue claraoke-subtitle:dialogue)))

;; Calculate text length.

;; (defun split-dialogue (alignment width height margin-top margin-bottom margin-left margin-right fontsize line-height line-width dialogue))

;; Sync Time and Pos

;; alignment 1, syllable 0
;; x=? y=?
;; x = margin-left + stack-render-length*
;; y = width - margin-bottom + stack-render-length*

;; (defclass layout ()
;;   (width height))

;; (defclass video-size (layout)
;;   ())

;; (defclass line-size (layout)
;;   (count))

;; (defclass rectangle-area (video-size)
;;   (low-x low-y high-x high-y))

;; (defgeneric compute-box-size (fontname fontsize)
;;   )


;;; Base classes

;; (defclass rectangle ()
;;   ((x1 :initform 0 :accessor point-x1)
;;    (y1 :initform 0 :accessor point-y1)
;;    (x2 :initform 0 :accessor point-x2)
;;    (y2 :initform 0 :accessor point-y2)))

;; (defclass rectangle-area (rectangle)
;;   ((w :initarg :w :reader width :initform (error "Required :w for width"))
;;    (h :initarg :h :reader height :initform (error "Required :h for height"))))

;; (defclass canvas (rectangle-area)
;;   ()
;;   (:documentation "Drawable area with rectangle shape."))

;; ;;; Constructor

;; (defmethod initialize-instance :after ((instance canvas)
;;                                        &rest initargs
;;                                        &key &allow-other-keys)
;;   (let ((top (getf initargs :margin-top 0))
;;         (left (getf initargs :margin-left 0))
;;         (bottom (getf initargs :margin-bottom 0))
;;         (right (getf initargs :margin-right 0)))
;;     ;; Dont change order!
;;     (setf (margin-top instance) top)
;;     (setf (margin-left instance) left)
;;     (setf (margin-bottom instance) bottom)
;;     (setf (margin-right instance) right)))

;; ;;; Getters

;; (defun margin-top (canvas)
;;   (point-y1 canvas))

;; (defun margin-left (canvas)
;;   (point-x1 canvas))

;; (defun margin-bottom (canvas)
;;   (- (height canvas)
;;      (+ (point-y1 canvas)
;;         (point-y2 canvas))))

;; (defun margin-right (canvas)
;;   (- (width canvas)
;;      (+ (point-x1 canvas)
;;         (point-x2 canvas))))

;; ;;; Setters

;; (defun (setf margin-top) (new-value canvas)
;;   (setf (point-y1 canvas) new-value))

;; (defun (setf margin-left) (new-value canvas)
;;   (setf (point-x1 canvas) new-value))

;; (defun (setf margin-bottom) (new-value canvas)
;;   (let ((height (- (height canvas) (point-y1 canvas))))
;;     (setf (point-y2 canvas) (- height new-value))))

;; (defun (setf margin-right) (new-value canvas)
;;   (let ((width (- (width canvas) (point-x1 canvas))))
;;     (setf (point-x2 canvas) (- width new-value))))

#+(or)
(make-instance 'canvas :w 1280 :h 720 :margin-top 25 :margin-bottom 25 :margin-left 20 :margin-right 20)

;; (defclass syllable (rectangle-area)
;;   ((plain-text :initarg :plain-text :reader plain-text :initform "-"))
;;   (:documentation "Drawable area with alignment inside canvas."))

;; (defclass alignment ()
;;   ((code :initarg :an :reader alignment-code :initform (error "Required :an"))
;;    (canvas :initarg :canvas :reader canvas :initform (error "Required :canvas"))
;;    (syllables :initarg :syllables :reader syllables)))

;; (defmethod initialize-instance :before ((instance alignment) &key canvas)
;;   (check-type canvas canvas))

;; (defmethod initialize-instance :after ((instance alignment) &key)
;;   (compute-rectangle-for-syllables instance))

;; (defmethod (setf alignment-code) (new-value (instance alignment))
;;   (setf (slot-value instance 'code) new-value)
;;   ;; (compute-rectangle-for-syllables instance)
;;   (calculate-virtual-alignment-1 instance))

;; (defun compute-rectangle-for-syllables (alignment)
;;   (with-accessors ((an alignment-code)
;;                    (canvas canvas)
;;                    (syllables syllables))
;;       alignment
;;     (let ((total-width (loop for syllable in syllables
;;                              sum (width syllable))))
;;       (ecase an
;;         ((1 :left-bottom :bottom-left)
;;          (let ((xmin (margin-left canvas))
;;                (ymax (- (height canvas) (margin-bottom canvas)))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) (- ymax (height syllable)))
;;              (setf (point-y2 syllable) ymax)
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((2 :center-bottom :bottom-center)
;;          (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
;;                (ymax (- (height canvas) (margin-bottom canvas)))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) (- ymax (height syllable)))
;;              (setf (point-y2 syllable) ymax)
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((3 :right-bottom :bottom-right)
;;          (let ((xmin (- (width canvas) (margin-right canvas) total-width))
;;                (ymax (- (height canvas) (margin-bottom canvas)))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) (- ymax (height syllable)))
;;              (setf (point-y2 syllable) ymax)
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((4 :left-middle :middle-left)
;;          (let ((xmin (margin-left canvas))
;;                (ymax (truncate (height canvas) 2))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) ymax)
;;              (setf (point-y2 syllable) (+ ymax (truncate (height syllable) 2)))
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((5 :center-middle :middle-center)
;;          (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
;;                (ymax (truncate (height canvas) 2))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) ymax)
;;              (setf (point-y2 syllable) (+ ymax (truncate (height syllable) 2)))
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((6 :right-middle :middle-right)
;;          (let ((xmin (- (width canvas) (margin-right canvas) total-width))
;;                (ymax (truncate (height canvas) 2))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) ymax)
;;              (setf (point-y2 syllable) (+ ymax (truncate (height syllable) 2)))
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((7 :left-top :top-left)
;;          (let ((xmin (margin-left canvas))
;;                (ymax (margin-top canvas))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) ymax)
;;              (setf (point-y2 syllable) (+ ymax (height syllable)))
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((8 :center-top :top-center)
;;          (let ((xmin (- (truncate (width canvas) 2) (truncate total-width 2)))
;;                (ymax (margin-top canvas))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) ymax)
;;              (setf (point-y2 syllable) (+ ymax (height syllable)))
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))
;;         ((9 :right-top :top-right)
;;          (let ((xmin (- (width canvas) (margin-right canvas) total-width))
;;                (ymax (margin-top canvas))
;;                (xcnt 0))
;;            (dolist (syllable syllables)
;;              (setf (point-x1 syllable) (+ xmin xcnt))
;;              (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
;;              (setf (point-y1 syllable) ymax)
;;              (setf (point-y2 syllable) (+ ymax (height syllable)))
;;              ;; xcnt
;;              (setf xcnt (+ xcnt (width syllable))))))))))

