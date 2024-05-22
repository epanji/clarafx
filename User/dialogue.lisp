(cl:in-package :clarafx.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Populate zero karaoke
;;;
(defun %populate-zero-karaoke (text)
  (declare (type claraoke-text:text text))
  (let ((plain (.text text))
        (char-bag '(#\Space #\Tab #\Newline)))
    ;; Change karaoke value to 0 and insert karaoke with previous
    ;; value at the end of non white-space character for every
    ;; syllables. Unchanged values could be happened due to same
    ;; indexes.
    (loop with prev = (length plain)
          for override in (stable-sort (copy-seq (overrides text)) '> :key 'index)
          for curr = (index override)
          and kara = (and (typep override 'claraoke-text:batch)
                          (increase-karaoke override 0))
          unless (null kara)
            do (let ((ktime (arg1 kara))
                     (char (char plain (max 0 (1- prev)))))
                 (insert-karaoke text curr 0)
                 (if (member char char-bag)
                     (insert-karaoke text (max 0 (- prev 2)) ktime)
                     (insert-karaoke text (max 0 (- prev 1)) ktime))
                 (setf prev curr)))
    ;; Populate zero karaoke for every characters without karaoke
    ;; except white-space.
    (loop with overrides = (overrides text)
          for index from 0 upto (1- (length plain))
          for kara = (let (override)
                       (and (setf override (find-override overrides index))
                            (typep override 'claraoke-text:batch)
                            (increase-karaoke override 0)))
          when (null kara)
            do (unless (member (char plain index) char-bag)
                 (insert-karaoke text index 0)))
    (sort-overrides text)
    (values)))

(defgeneric populate-zero-karaoke (object)
  (:documentation "Make every characters have zero karaoke except
white-space and last non white-space character in syllables."))

(defmethod populate-zero-karaoke ((object claraoke-text:text))
  (%populate-zero-karaoke object)
  (values))

(defmethod populate-zero-karaoke ((object claraoke-subtitle:dialogue))
  (populate-zero-karaoke (.text object))
  (values))

(defmethod populate-zero-karaoke ((object cons))
  (loop for dialogue in object
        when (typep dialogue 'claraoke-subtitle:dialogue)
          do (populate-zero-karaoke dialogue))
  (values))

(defmethod populate-zero-karaoke ((object claraoke-subtitle:events))
  (populate-zero-karaoke (lines object))
  (values))

(defmethod populate-zero-karaoke ((object claraoke-subtitle:subtitle))
  (populate-zero-karaoke (events object))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Populate delay effect
;;;
(defun %populate-delay-effect (text &optional delay)
  (declare (type claraoke-text:text text))
  (let ((delay (or delay 15))
        (plain (.text text))
        (overrides (overrides text))
        (char #\INVISIBLE_SEPARATOR))
    ;; Check first character
    (unless (char= char (char plain 0))
      ;; Add invisible separator for index 0
      (setf (.text text) (concatenate 'string (string char) plain))
      ;; Increase indexes for each overrides
      (loop for override in overrides
            do (incf (index override) 1))
      (insert-override text (override 'karaoke 0 :arg1 delay)))
    (values)))

(defgeneric populate-delay-effect (object &optional delay)
  (:documentation "Add invisible separator and karaoke modifier for
  index 0 after increasing other indexes."))

(defmethod populate-delay-effect
    ((object claraoke-text:text) &optional delay)
  (%populate-delay-effect object delay)
  (values))

(defmethod populate-delay-effect
    ((object claraoke-subtitle:dialogue) &optional delay)
  (populate-delay-effect (.text object) delay)
  (values))

(defmethod populate-delay-effect
    ((object cons) &optional delay)
  (loop for dialogue in object
        when (typep dialogue 'claraoke-subtitle:dialogue)
          do (populate-delay-effect dialogue delay))
  (values))

(defmethod populate-delay-effect
    ((object claraoke-subtitle:events) &optional delay)
  (populate-delay-effect (lines object) delay)
  (values))

(defmethod populate-delay-effect
    ((object claraoke-subtitle:subtitle) &optional delay)
  (populate-delay-effect (events object) delay)
  (values))

