(cl:in-package :clarafx-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base classes
;;;
(defclass rectangle ()
  ((x1 :initform 0 :accessor point-x1)
   (y1 :initform 0 :accessor point-y1)
   (x2 :initform 0 :accessor point-x2)
   (y2 :initform 0 :accessor point-y2)))

(defclass rectangle-area (rectangle)
  ((w :initarg :w :reader width :initform (error "Required :w for width"))
   (h :initarg :h :reader height :initform (error "Required :h for height"))))

(defclass canvas (rectangle-area)
  ((fontname :initarg :fontname :accessor fontname :initform "Arial")
   (fontsize :initarg :fontsize :accessor fontsize :initform 12)
   (fontspace :initarg :fontspace :accessor fontspace :initform 0)
   (bold :initarg :bold :accessor bold :initform 0)
   (italic :initarg :italic :accessor italic :initform 0)
   (dpi :initarg :dpi :accessor dpi :initform 64))
  (:documentation "Drawable area with rectangle shape."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructor
;;;
(defmethod initialize-instance :after ((instance canvas)
                                       &rest initargs
                                       &key &allow-other-keys)
  (let ((top (getf initargs :margin-top 0))
        (left (getf initargs :margin-left 0))
        (bottom (getf initargs :margin-bottom 0))
        (right (getf initargs :margin-right 0)))
    ;; Dont change order!
    (setf (margin-top instance) top)
    (setf (margin-left instance) left)
    (setf (margin-bottom instance) bottom)
    (setf (margin-right instance) right)))

(defun make-canvas* (width height
                     margin-top margin-left
                     margin-bottom margin-right
                     &optional
                       fontname fontsize fontspace
                       bold italic dpi)
  (let ((canvas (make-instance 'canvas
                               :w width
                               :h height
                               :margin-top margin-top
                               :margin-left margin-left
                               :margin-bottom margin-bottom
                               :margin-right margin-right)))
    (unless (null fontname)
      (setf (fontname canvas) fontname))
    (unless (null fontsize)
      (setf (fontsize canvas) fontsize))
    (unless (null fontspace)
      (setf (fontspace canvas) fontspace))
    (unless (null bold)
      (setf (bold canvas) bold))
    (unless (null italic)
      (setf (italic canvas) italic))
    (unless (null dpi)
      (setf (dpi canvas) dpi))
    canvas))

(defun make-canvas (object &optional (style-name "Default") (dpi 64))
  (declare (type claraoke-subtitle:subtitle object))
  (let ((width (claraoke:value (claraoke:find-info object "PlayResX")))
        (height (claraoke:value (claraoke:find-info object "PlayResY")))
        (style (claraoke:find-style object style-name)))
    (let ((margin-top (claraoke:margin-v style))
          (margin-left (claraoke:margin-l style))
          (margin-bottom (claraoke:margin-v style))
          (margin-right (claraoke:margin-r style))
          (fontname (claraoke:fontname style))
          (fontsize (claraoke:fontsize style))
          (fontspace (claraoke:spacing style))
          (bold (claraoke:bold style))
          (italic (claraoke:italic style)))
      (make-canvas* width height
                    margin-top margin-left margin-bottom margin-right
                    fontname fontsize fontspace bold italic dpi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Getters
;;;
(defun margin-top (canvas)
  (point-y1 canvas))

(defun margin-left (canvas)
  (point-x1 canvas))

(defun margin-bottom (canvas)
  (- (height canvas)
     (+ (point-y1 canvas)
        (point-y2 canvas))))

(defun margin-right (canvas)
  (- (width canvas)
     (+ (point-x1 canvas)
        (point-x2 canvas))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setters
;;;
(defun (setf margin-top) (new-value canvas)
  (setf (point-y1 canvas) new-value))

(defun (setf margin-left) (new-value canvas)
  (setf (point-x1 canvas) new-value))

(defun (setf margin-bottom) (new-value canvas)
  (let ((height (- (height canvas) (point-y1 canvas))))
    (setf (point-y2 canvas) (- height new-value))))

(defun (setf margin-right) (new-value canvas)
  (let ((width (- (width canvas) (point-x1 canvas))))
    (setf (point-x2 canvas) (- width new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Others
;;;
(defun make-canvas-face (canvas)
  (declare (type canvas canvas))
  (with-accessors ((fn fontname)
                   (fs fontsize)
                   (b bold)
                   (i italic)
                   (d dpi))
      canvas
    (make-face* fn :bold b :italic i :fontsize fs :dpi d)))

