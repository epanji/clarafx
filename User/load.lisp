(cl:in-package #:clarafx.user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; External effects
;;;
(defvar *external-effects-pathname* (pathname "clarafx.lisp"))

(defun current-effects-pathname ()
  (merge-pathnames *external-effects-pathname* *default-pathname-defaults*))

(defun user-effects-pathname ()
  (merge-pathnames *external-effects-pathname*
                   (make-pathname
                    :directory (append (pathname-directory
                                        (user-homedir-pathname))
                                       (list ".config" "clarafx")))))

(defun effects-pathname ()
  (or (probe-file (current-effects-pathname))
      (probe-file (user-effects-pathname))
      (warn "No external effects")))

(when (null (find-package :clarafx.load))
  (defpackage #:clarafx.load
    (:use #:common-lisp #:clarafx)
    (:shadow #:defpackage
             #:delete-file
             #:delete-package
             #:eval
             #:eval-when
             #:find-package
             #:find-symbol
             #:in-package
             #:load
             #:load-external-effects
             #:load-revision-file
             #:make-package
             #:read-from-string
             #:rename-file
             #:rename-package
             #:unintern
             #:unuse-package
             #:use-package)))

(defun strip-package-from-symbol (string)
  "Make package:symbol or package::symbol to be symbol."
  (let ((parser (clarafx.draw::make-parser string))
        (output (make-string-output-stream))
        (start 0)
        (end 0)
        (strip-p nil)
        (max-loop (length string)))
    (declare (special parser))
    (flet ((step-fun ()
             (case (funcall parser :peek)
               ;; finish
               ((nil)
                (princ (subseq (funcall parser :string)
                               start nil)
                       output)
                :stop)
               ;; single line comment
               (#\;
                (loop (case (funcall parser :peek 1)
                        ((nil)
                         (return))
                        (#\Newline
                         (funcall parser :advance)
                         (return)))
                      (funcall parser :advance))
                (setf end (1+ (funcall parser :index)))
                (funcall parser :consume))
               ;; multiple lines comment
               (#\#
                (case (funcall parser :peek 1)
                  (#\|
                   (funcall parser :advance)
                   ;; maybe nested
                   (let ((level 0))
                     (loop (case (funcall parser :peek 1)
                             ((nil)
                              (return))
                             (#\#
                              (funcall parser :advance)
                              (case (funcall parser :peek 1)
                                (#\|
                                 (incf level))))
                             (#\|
                              (funcall parser :advance)
                              (case (funcall parser :peek 1)
                                ((nil)
                                 (return))
                                (#\#
                                 (funcall parser :advance)
                                 (case level
                                   (0 (return))
                                   (otherwise
                                    (decf level)))))))
                           (funcall parser :advance)))
                   (setf end (1+ (funcall parser :index))))
                  (#\:
                   (case (funcall parser :peek -1)
                     ((#\Space #\Tab #\Newline #\( #\')
                      (funcall parser :advance)))))
                (funcall parser :consume))
               ;; string
               (#\"
                (case (funcall parser :peek -1)
                  (#\\)
                  (otherwise
                   (loop (case (funcall parser :peek 1)
                           ((nil)
                            (return))
                           (#\"
                            (case (funcall parser :peek)
                              (#\\)
                              (otherwise
                               (funcall parser :advance)
                               (return)))))
                         (funcall parser :advance))
                   (setf end (1+ (funcall parser :index)))))
                (funcall parser :consume))
               ;; terminal
               ((#\Space #\Tab #\Newline #\( #\')
                (setf end (1+ (funcall parser :index)))
                (funcall parser :consume))
               ;; package
               (#\:
                (case (funcall parser :peek -1)
                  ((nil #\'))
                  (otherwise
                   (loop (case (funcall parser :peek 1)
                           (#\:)
                           (otherwise
                            (return)))
                         (funcall parser :advance))
                   (case (funcall parser :peek -1)
                     ((#\Space #\Tab #\Newline #\( #\\))
                     (otherwise
                      (if (> start end)
                          (setf end start)
                          (princ (subseq (funcall parser :string)
                                         start end)
                                 output))
                      (setf strip-p t)
                      (setf start (1+ (funcall parser :index)))))))
                (funcall parser :consume))
               ;; default
               (otherwise
                (funcall parser :consume)))))
      ;; refactor using loop because control stack exhausted when
      ;; using recursive for bigger string
      (loop for i from 0 upto max-loop
            for c = (step-fun)
            when (eql :stop c)
              do (return t))
      (let ((result (get-output-stream-string output)))
        (close output)
        (values result strip-p)))))

(defun maybe-revision (file)
  (declare (type pathname file))
  (let ((ori (alexandria:read-file-into-string file)))
    (multiple-value-bind (rev pred)
        (strip-package-from-symbol ori)
      (when pred
        (with-open-file (stream file :direction :output
                                     :if-exists :rename
                                     :if-does-not-exist :create)
          (princ rev stream))
        (values t)))))

(defun load-revision-file (file)
  (declare (type pathname file))
  (let ((*package* (find-package :clarafx.load)))
    (handler-case (prog2
                      (maybe-revision file)
                      (load file)
                    (format *standard-output* "~&Loading ~A~%" file))
      (error (condition)
        (format *error-output*
                "~&~A~%Incomplete loading ~A~%"
                condition file)))))

(defun load-external-effects ()
  (let ((file (effects-pathname)))
    (unless (null file)
      (load-revision-file file))))

