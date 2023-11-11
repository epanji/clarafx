(cl:in-package :clarafx.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version
;;;
(defun clarafx-version ()
  #.(let ((file (merge-pathnames
                 "../version.lisp-expr"
                 (or *compile-file-pathname* *load-truename*))))
      (format nil "CLARAFX v~A"
              (with-open-file
                  (stream file)
                (read stream)))))

