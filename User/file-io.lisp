(cl:in-package :clarafx.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IO
;;;
(defun write-subtitle-file (subtitle file &optional (if-exists :error))
  "Writing SUBTITLE argument to FILE argument.
SUBTITLE argument must be SUBTITLE object.
FILE argument must be pathname designator."
  (declare (type claraoke-subtitle:subtitle subtitle))
  (let ((file-path (pathname file)))
    (with-open-file (stream file-path :direction :output
                                      :if-does-not-exist :create
                                      :if-exists if-exists)
      (print-script subtitle stream))
    (values file-path)))

(defun read-subtitle-effect (object)
  "Return SUBTITLE object after processing effects from OBJECT argument.
OBJECT argument could be SUBTITLE object or SUBTITLE file."
  (parse-effect object))

(defun write-subtitle-effect (object file &optional (if-exists :error))
  "Writing SUBTITLE object from OBJECT argument to FILE argument.
OBJECT argument could be SUBTITLE object or SUBTITLE file.
If OBJECT argument is SUBTITLE file, do not use same file for FILE argument."
  (let ((new-object (read-subtitle-effect object)))
    (write-subtitle-file new-object file if-exists)))

