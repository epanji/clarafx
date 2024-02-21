(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; font-discovery
;;;
;;; (cffi:defcenum weight
;;;   (:thin 0)
;;;   (:extra-light 40)
;;;   (:light 50)
;;;   (:semi-light 55)
;;;   (:book 75)
;;;   (:regular 80)
;;;   (:medium 100)
;;;   (:semi-bold 180)
;;;   (:bold 200)
;;;   (:extra-bold 205)
;;;   (:black 210)
;;;   (:extra-black 215))
;;;
;;; (cffi:defcenum slant
;;;   (:roman 0)
;;;   (:italic 100)
;;;   (:oblique 110))
;;;
(defun ensure-font (family-name &key bold italic)
  (let ((w (case bold
             ((nil) nil)
             ((t -1 1) :bold)
             (0 :regular)
             (otherwise (typecase bold
                          ((or keyword integer) bold)
                          (otherwise nil)))))
        (s (case italic
             ((nil) nil)
             ((t -1 1) :italic)
             (0 :roman)
             (otherwise (typecase italic
                          ((or keyword integer) italic)
                          (otherwise nil))))))
    (org.shirakumo.font-discovery:find-font
     :family family-name :weight w :slant s)))

(defun ensure-font-pathname (family-name &key bold italic)
  (org.shirakumo.font-discovery:file
   (ensure-font family-name :bold bold :italic italic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cl-freetype2
;;;
(defun make-face (family-name &key bold italic)
  (freetype2:new-face (ensure-font-pathname family-name :bold bold :italic italic)))

(defun set-char-size (face &key (fontsize 36) (dpi 64))
  (freetype2:set-char-size face (* fontsize 64) 0 dpi dpi)
  (values face))

(defun make-face* (family-name &key bold italic (fontsize 36) (dpi 64))
  (let ((face (make-face family-name :bold bold :italic italic)))
    (set-char-size face :fontsize fontsize :dpi dpi)))

(defvar *face* (make-face* "Not exists"))

(defun %string-pixel-width (string &key (face *face*))
  (freetype2:string-pixel-width face string))

(defun string-pixel-width (string &key (fontspace 0) (face *face*))
  (+ (%string-pixel-width string :face face)
     (* fontspace (length string))))

(defun string-pixel-height (string &key (face *face*))
  (freetype2:string-pixel-height face string))

