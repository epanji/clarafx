(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define Effect
;;;
(declaim (type boolean *ignore-partial*))

(defvar *ignore-partial* nil "Boolean to ignore partial effect.")

(defmacro with-syllable-modifiers ((var-name dialogue subtitle alignment-code style-name dpi) &body body)
  (let ((modifiers (gensym "WSM"))
        (overrides (gensym "WSM")))
    `(with-every-syllable-from-karaoke (,var-name ,dialogue ,subtitle ,alignment-code ,style-name ,dpi)
       (let* ((,modifiers (list ,@body))
              (,overrides (list (override 'alignment-numpad 0
                                          :arg1 1
                                          :modifiers ,modifiers))))
         ;; When read subtitle effects from already parsed file, it
         ;; will have different text due to CLARAOKE always trim every
         ;; dialogues. To avoid differences, it is necessary to trim
         ;; plain text when converting syllables to dialogues. It is
         ;; safe because new dialogue does not need to be calculated
         ;; like syllable.
         (unless (invisiblep ,var-name)
           (when (or *ignore-partial* (partialp ,var-name))
             (list* (dialogue (string-trim '(#\Space #\Tab) (plain-text ,var-name))
                              :start (or (origin-start ,var-name) (start ,var-name))
                              :end (or (origin-end ,var-name) (end ,dialogue))
                              :layer (layer ,dialogue)
                              :style (if (find-style ,subtitle ,style-name)
                                         (name (find-style ,subtitle ,style-name))
                                         (.style ,dialogue))
                              :event-name (name ,dialogue)
                              :event-margin-l (margin-l ,dialogue)
                              :event-margin-r (margin-r ,dialogue)
                              :event-margin-v (margin-v ,dialogue)
                              :overrides ,overrides)
                    (extra-dialogues ,var-name))))))))

(defmacro define-effect ((name var) &body body)
  `(defun ,name (dialogue &key subtitle
                            (alignment-code 2)
                            (style-name "Default")
                            (dpi 64)
                            (ignore-partial t))
     (let ((*ignore-partial* (or ignore-partial (not (find-partial dialogue)))))
       (with-syllable-modifiers (,var dialogue subtitle alignment-code style-name dpi)
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register and Calling Effect
;;;
(defvar *effects* (make-hash-table :test 'equalp))

(defun list-effects ()
  (loop for effect-name being the hash-key in *effects*
        collect effect-name))

(defun register-effect (name symbol)
  (declare (type string name)
           (type symbol symbol))
  (setf (gethash name *effects*) (fdefinition symbol)))

(defun find-effect (name)
  (declare (type string name))
  (gethash name *effects* nil))

(defun parse-effect-info (info)
  (declare (type (or claraoke-subtitle:info null) info))
  (unless (null info)
    (claraoke-internal:distinct-number-and-string
     (claraoke-internal:split-by-char #\, (value info) 5))))

(defun funcall-effect (subtitle dialogue)
  "Call matching effect between `dialogue effect' and `script info'.
It will return DIALOGUEs when successfully calling the effect, otherwise NIL.
Failure could be caused by no match in `script info' or unregistered effect name.

This is sample for script info and effect name:

[Script Info]
...
clarafx-<number>: <effect>[,[style][,[dpi][,[alignment][,[no complement predicate]]]]]
...

[Events]
...
Dialogue: 1st,2nd,3rd,4th,5th,6th,7th,8th,clarafx-<number>,10th
...
"
  (declare (type claraoke-subtitle:subtitle subtitle)
           (type claraoke-subtitle:dialogue dialogue))
  (let ((effect-info (find-info subtitle (effect dialogue))))
    (unless (null effect-info)
      (let* ((effect-info-list (parse-effect-info effect-info))
             (effect-name (first effect-info-list))
             (style-name (or (second effect-info-list)
                             (.style dialogue)
                             "Default"))
             (dpi (or (third effect-info-list) 64))
             (alignment-code (or (fourth effect-info-list) 2))
             (effect (find-effect effect-name))
             (complement-effect (when (null (fifth effect-info-list))
                                  (if (find-partial dialogue)
                                      (find-effect "complement-partial")
                                      (find-effect "complement")))))
        (unless (null effect)
          ;; Return splitted dialogues
          (prog1 (append (funcall effect dialogue
                                  :subtitle subtitle
                                  :ignore-partial nil
                                  :style-name style-name
                                  :dpi dpi
                                  :alignment-code alignment-code)
                         (unless (null complement-effect)
                           (funcall complement-effect dialogue
                                    :subtitle subtitle
                                    :style-name style-name
                                    :dpi dpi
                                    :alignment-code alignment-code)))
            ;; Mutate origin dialogue
            (change-class dialogue 'claraoke-subtitle:comment
                          :descriptor "Comment")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse Effect
;;;
(defgeneric parse-effect (object)
  (:documentation "Return SUBTITLE object after executing effects from OBJECT argument.
If OBJECT argument is SUBTITLE object, it will return mutated OBJECT."))

(defmethod parse-effect ((object string))
  (let ((path (pathname object)))
    (parse-effect path)))

(defmethod parse-effect ((object pathname))
  (let ((subtitle (parse-script object)))
    (parse-effect subtitle)))

(defmethod parse-effect ((object claraoke-subtitle:subtitle))
  (let* ((filtered-dialogues (loop for dialogue1 in (lines (events object))
                                   for effect-name = (and (< 8 (length (effect dialogue1)))
                                                          (subseq (effect dialogue1) 0 8))
                                   when (and (typep dialogue1 'claraoke-subtitle:dialogue)
                                             (string-equal "clarafx-" effect-name))
                                     collect dialogue1))
         (generated-effects (loop for dialogue2 in filtered-dialogues
                                  for effect = (funcall-effect object dialogue2)
                                  unless (null effect)
                                    append effect)))
    (unless (null generated-effects)
      (setf (lines (events object))
            (append (lines (events object)) generated-effects)))
    (values object)))

