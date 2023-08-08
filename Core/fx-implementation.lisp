(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define Effects
;;;
(defmacro with-syllable-modifiers ((var-name dialogue subtitle alignment-code style-name dpi) &body body)
  (let ((modifiers (gensym "WSM"))
        (overrides (gensym "WSM")))
    `(with-every-syllable-from-karaoke (,var-name ,dialogue ,subtitle ,alignment-code ,style-name ,dpi)
       (setf (script-info ,var-name) (script-info ,subtitle)
             (style ,var-name) (find-style ,subtitle ,style-name)
             (dialogue ,var-name) ,dialogue)
       (let* ((,modifiers (list ,@body))
              (,overrides (list (override 'alignment-numpad 0
                                          :arg1 1
                                          :modifiers ,modifiers))))
         (dialogue (plain-text ,var-name) :start (or (origin-start ,var-name) (start ,var-name))
                                          :end (or (origin-end ,var-name) (end ,dialogue))
                                          :layer (layer ,dialogue)
                                          :style (.style ,dialogue)
                                          :event-name (name ,dialogue)
                                          :event-margin-l (margin-l ,dialogue)
                                          :event-margin-r (margin-r ,dialogue)
                                          :event-margin-v (margin-v ,dialogue)
                                          :overrides ,overrides)))))

(defmacro define-effect ((name var) &body body)
  `(defun ,name (dialogue &key subtitle (alignment-code 2) (style-name "Default") (dpi 64))
     (with-syllable-modifiers (,var dialogue subtitle alignment-code style-name dpi)
       ,@body)))

