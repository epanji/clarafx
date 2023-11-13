(cl:in-package :clarafx.cli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Options
;;;
;;; No output = *standard-output*
;;;
(defun ensure-file-type (file &key (suffix "") (type "ass") (probe-p nil))
  (let ((pfile (pathname file)))
    (cond ((string-equal type (pathname-type pfile))
           (let ((rfile (merge-pathnames
                         (format nil "~A~A.~A"
                                 (pathname-name pfile)
                                 suffix
                                 (pathname-type pfile))
                         pfile)))
             (when probe-p
               (or (probe-file rfile)
                   (signal 'opts:arg-parser-failed :raw-arg file)))
             rfile))
          (t (signal 'opts:arg-parser-failed :raw-arg file)))))

(opts:define-opts
  (:name :help
   :description "Show this help message."
   :short #\h)
  (:name :version
   :description "Show version."
   :short #\v)
  (:name :supersede
   :description "Supersede output if it already exists."
   :short #\s)
  (:name :load
   :short #\l
   :description "Load external effects. [From: $PWD/clarafx.lisp | $HOME/.config/clarafx/clarafx.lisp]")
  (:name :input
   :description "Input file.ass which has effects information."
   :short #\i
   :arg-parser (lambda (arg) (ensure-file-type arg :probe-p t))
   :meta-var "<file>")
  (:name :output
   :description "Output file.ass for final subtitle effects according to input."
   :short #\o
   :arg-parser (lambda (arg) (ensure-file-type arg))
   :meta-var "<file>"
   :default "stdout"))

(defun get-options ()
  (handler-bind ((opts:unknown-option
                   (lambda (c)
                     (format t "Warning: Unknown option ~S!~%" (opts:option c))
                     (invoke-restart 'opts:skip-option)))
                 (opts:missing-arg
                   (lambda (c)
                     (format t "Error: Required argument for option ~S.~%"
                             (opts:option c))
                     (opts:exit 1)))
                 (opts:arg-parser-failed
                   (lambda (c)
                     (format t "Error: Invalid argument ~S for option ~S.~%"
                             (opts:raw-arg c)
                             (opts:option c))
                     (opts:exit 1)))
                 (opts:missing-required-option
                   (lambda (c)
                     (format t "Error: ~A.~%" c)
                     (opts:exit 1))))
    (opts:get-opts)))

(defun main ()
  (let ((options (get-options))
        (ifile nil)
        (ofile nil)
        (efile :error))
    (setf ifile (getf options :input))
    (setf ofile (getf options :output))
    (when (getf options :help nil)
      (opts:describe
       :prefix "Read and write customize karaoke effects from subtitle file."
       :suffix (format nil "Author:~%~2TPanji Kusuma <epanji@gmail.com>~2%~
                            License:~%~2TBSD-2-Clause license")
       :usage-of "clarafx")
      (opts:exit 0))
    (when (getf options :version nil)
      (format t "~A~%" (clarafx:clarafx-version))
      (opts:exit 0))
    (when (getf options :load nil)
      (clarafx:load-external-effects))
    (when (getf options :supersede nil)
      (setf efile :supersede))
    (when (getf options :output nil)
      (when (and (eql :error efile)
                 (probe-file ofile))
        (format t "Error: Output file already exists. ~S~%"
                (namestring ofile))
        (opts:exit 1)))
    (when (getf options :input nil)
      (let ((sub (clarafx:read-subtitle-effect ifile)))
        (if (pathnamep ofile)
            (clarafx:write-subtitle-effect sub ofile efile)
            (clarafx:print-script sub *standard-output*))))))

