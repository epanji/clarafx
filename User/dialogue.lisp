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
      (insert-override text (override 'karaoke 0 :arg1 (abs delay))))
    (values)))

(defgeneric populate-delay-effect (object &optional delay)
  (:documentation "Add invisible separator for first character in plain text.
Increase current indexes and add karaoke modifier for override with index 0.
- DELAY could be positive or negative integer.
- Negative DELAY will decrease start slot if exists.
- KARAOKE always have positive value."))

(defmethod populate-delay-effect
    ((object claraoke-text:text) &optional delay)
  (%populate-delay-effect object delay)
  (values))

(defmethod populate-delay-effect
    ((object claraoke-subtitle:dialogue) &optional delay)
  (populate-delay-effect (.text object) delay)
  (when (and (integerp delay) (minusp delay))
    (decrease-duration (start object) (abs delay)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Populate odd-even effect
;;;
(defun %make-counter (&optional (init 0) &aux (counter init))
  (lambda (&optional option)
    (case option
      (:update (incf counter))
      (:reset (setf counter init))
      (otherwise (values counter)))))

(defvar *counter* nil)

(defun %populate-odd-even-effect (dialogue odd even &optional (force t))
  (declare (type claraoke-subtitle:dialogue dialogue)
           (type (or string null) odd even))
  (when (null *counter*)
    (setf *counter* (%make-counter 1)))
  (let ((string-effect (effect dialogue)))
    (cond ((oddp (funcall *counter*))
           (when (and (stringp odd)
                      (or (string= "" string-effect)
                          force))
             (setf (effect dialogue) odd)))
          ((evenp (funcall *counter*))
           (when (and (stringp even)
                      (or (string= "" string-effect)
                          force))
             (setf (effect dialogue) even)))))
  (funcall *counter* :update)
  (values))

(defgeneric populate-odd-even-effect (object odd even &optional force)
  (:documentation "Populate dialogue effect for odd lines and even lines.
If dialogue effect already exists, it is unchanged except when FORCE is true.
ODD and EVEN argument could be same string or one of them NIL."))

(defmethod populate-odd-even-effect
    ((object claraoke-subtitle:dialogue) odd even &optional force)
  (%populate-odd-even-effect object odd even force)
  (values))

(defmethod populate-odd-even-effect ((object cons) odd even &optional force)
  (if (null *counter*)
      (setf *counter* (%make-counter 1))
      (funcall *counter* :reset))
  (loop for dialogue in object
        when (typep dialogue 'claraoke-subtitle:dialogue)
          do (populate-odd-even-effect dialogue odd even force))
  (values))

(defmethod populate-odd-even-effect
    ((object claraoke-subtitle:events) odd even &optional force)
  (sort-events object)
  (populate-odd-even-effect (reverse (lines object)) odd even force)
  (values))

(defmethod populate-odd-even-effect
    ((object claraoke-subtitle:subtitle) odd even &optional force)
  (populate-odd-even-effect (events object) odd even force)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; With auto increment events
;;;
(defun with-auto-increment-events
    (subtitle interval frequency counter ktime &rest elements)
  "Simplify insert events with auto increment for dialogue times.
Below are description for arguments:

SUBTITLE    is subtitle object targeted for elements.
INTERVAL    is delay time before next line.
FREQUENCY   is interval frequency for one line duration.
COUNTER     is initial time (integer or duration format) for counter.
KTIME       is initial karaoke time for spell duration.
ELEMENT     is item with various value to be executed for subtitle object.
            It could be integer, string, or cons: '(string &optional ±event 0+delay +spell).
            The integer means to be interval frequency for blank line duration.
            The string means to be dialogue text.
            The cons will be parse to dialogue with specific options."
  (declare (type claraoke-subtitle:subtitle subtitle)
           (type unsigned-byte interval frequency)
           (type (or unsigned-byte null) ktime))
  (let ((cleanup-list (list (interval subtitle)
                            (interval-frequency subtitle)
                            (interval-counter subtitle))))
    (setf (interval subtitle) interval)
    (setf (interval-frequency subtitle) frequency)
    (setf (interval-counter subtitle) (durationinteger counter))
    (unwind-protect
         (%call-with-auto-increment-events subtitle ktime elements)
      (setf (interval subtitle) (first cleanup-list))
      (setf (interval-frequency subtitle) (second cleanup-list))
      (setf (interval-counter subtitle) (third cleanup-list))))
  (values))

(defun %call-with-auto-increment-events
    (subtitle ktime elements)
  (declare (type claraoke-subtitle:subtitle subtitle)
           (type (or unsigned-byte null) ktime))
  (let ((gop (not (null ktime)))
        (idelay 0)
        (interval (interval subtitle))
        (frequency (interval-frequency subtitle)))
    (flet ((translate-element (element)
             (etypecase element
               (cons
                ;; '(string &optional ±event 0+delay +spell)
                (multiple-value-bind (str iev ide spe)
                    (apply 'values element)
                  (let ((lgop (or gop (not (null spe)))))
                    (insert-event subtitle
                                  (dialogue str
                                            :generate-overrides-p lgop
                                            :spell-duration (or spe ktime))
                                  :interval-delay ide
                                  :interval-event (+ frequency iev))))
                (setf idelay 1))
               (string
                (insert-event subtitle
                              (dialogue element
                                        :generate-overrides-p gop
                                        :spell-duration ktime)
                              :interval-delay idelay)
                (setf idelay 1))
               (unsigned-byte
                (incf (interval-counter subtitle) (* element interval))
                (setf idelay 0)))))
      (mapc #'translate-element elements)))
  (values))

