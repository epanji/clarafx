(cl:in-package :clarafx.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IO
;;;
(defun read-subtitle-effect (object)
  "Return SUBTITLE object after processing effects from OBJECT argument.
OBJECT argument could be SUBTITLE object or SUBTITLE file."
  (parse-effect object))

(defun write-subtitle-effect (object file &optional (if-exists :error))
  "Writing SUBTITLE object from OBJECT argument to FILE argument.
OBJECT argument could be SUBTITLE object or SUBTITLE file.
If OBJECT argument is SUBTITLE file, do not use same file for FILE argument."
  (let ((new-object (read-subtitle-effect object))
        (file-path (pathname file)))
    (with-open-file (stream file-path :direction :output
                                      :if-does-not-exist :create
                                      :if-exists if-exists)
      (print-script new-object stream))
    file-path))

