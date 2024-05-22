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

