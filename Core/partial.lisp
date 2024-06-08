(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Partial effect
;;;
(defvar *partial-name* "part")

(defgeneric partialp (object)
  (:method (object)
    (and (typep object 'claraoke-text:modifier)
         (string-equal "unknown" (string (type-of object)))
         (stringp (arg1 object))
         (string-equal *partial-name* (arg1 object)))))

(defun %find-partial (batch)
  (declare (type claraoke-text:batch batch))
  (find-if 'partialp (modifiers batch)))

(defgeneric find-partial (object)
  (:method (object) nil)
  (:method ((object claraoke-text:batch))
    (%find-partial object))
  (:method ((object claraoke-text:text))
    (loop for override in (overrides object)
          for modifier = (find-partial override)
          unless (null modifier)
            return modifier))
  (:method ((object claraoke-subtitle:dialogue))
    (find-partial (.text object))))

(defun %insert-partial (text index)
  (declare (type claraoke-text:text text)
           (type unsigned-byte index))
  (let* ((string (.text text))
         (index (min index (1- (length string))))
         (override (find-override text index)))
    (etypecase override
      (null
       (insert-override text (override 'unknown index :arg1 *partial-name*)))
      (claraoke-text:newline
       (let ((arg1 (arg1 override)))
         (change-class override 'batch :modifiers
                       (list (claraoke:modifier 'newline :arg1 arg1)
                             (claraoke:modifier 'unknown :arg1 *partial-name*)))))
      (claraoke-text:batch
       (let ((modifier (%find-partial override)))
         (when (null modifier)
           (insert-modifier override (modifier 'unknown :arg1 *partial-name*))))))
    (values text)))

(defgeneric insert-partial (object index)
  (:method ((object claraoke-text:text) (index integer))
    (%insert-partial object index))
  (:method ((object claraoke-text:text) (index string))
    (let ((index (search index (.text object))))
      (insert-partial object index)))
  (:method ((object claraoke-subtitle:dialogue) index)
    (insert-partial (.text object) index)
    (values object)))

