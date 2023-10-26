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
    (:use #:common-lisp #:claraoke #:clarafx.draw #:clarafx.core)
    (:shadow #:defpackage
             #:delete-file
             #:delete-package
             #:eval
             #:eval-when
             #:find-package
             #:in-package
             #:load
             #:make-package
             #:read-from-string
             #:rename-file
             #:rename-package
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
                   (loop (case (funcall parser :peek 1)
                           ((nil)
                            (return))
                           (#\|
                            (funcall parser :advance)
                            (when (char-equal #\# (funcall parser :peek 1))
                              (funcall parser :advance)
                              (return))))
                         (funcall parser :advance)))
                  (otherwise
                   (funcall parser :advance)))
                (setf end (1+ (funcall parser :index)))
                (funcall parser :consume))
               ;; string
               (#\"
                (loop (case (funcall parser :peek 1)
                        ((nil)
                         (return))
                        (#\"
                         (if (char-equal #\\ (funcall parser :peek))
                             (funcall parser :advance)
                             (progn (funcall parser :advance)
                                    (return)))))
                      (funcall parser :advance))
                (setf end (1+ (funcall parser :index)))
                (funcall parser :consume))
               ;; terminal
               ((#\Space #\Tab #\Newline #\( #\')
                (setf end (1+ (funcall parser :index)))
                (funcall parser :consume))
               ;; package
               (#\:
                (loop (case (funcall parser :peek 1)
                        (#\:
                         (funcall parser :advance))
                        (otherwise
                         (return))))
                (case (funcall parser :peek -1)
                  ((#\Space #\Tab #\Newline #\( #\\))
                  (otherwise
                   (if (> start end)
                       (setf end start)
                       (princ (subseq (funcall parser :string)
                                      start end)
                              output))
                   (setf strip-p t)
                   (setf start (1+ (funcall parser :index)))))
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

(defun load-external-effects ()
  (let ((file (effects-pathname))
        (*package* (find-package :clarafx.load)))
    (unless (null file)
      (handler-case (prog2
                        (maybe-revision file)
                        (load file)
                      (format *standard-output* "~&Loading ~A~%" file))
        (error (condition)
          (format *error-output*
                  "~&~A~%Incomplete loading ~A~%"
                  condition file))))))

