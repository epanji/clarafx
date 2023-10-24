(cl:in-package #:clarafx.user)

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
             #:delete-package
             #:eval
             #:eval-when
             #:find-package
             #:in-package
             #:load
             #:make-package
             #:read-from-string
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
    (flet ((step-fun (&optional char)
             (cond
               ;; finish
               ((null char)
                (princ (subseq (funcall parser :string)
                               start nil)
                       output)
                :stop)
               ;; single line comment
               ((char-equal #\; char)
                (let ((next nil))
                  (loop (setf next (funcall parser :peek 1))
                        (when (or (null next)
                                  (char-equal #\Newline next))
                          (funcall parser :advance)
                          (return))
                        (funcall parser :advance))
                  (setf end (1+ (funcall parser :index))))
                (funcall parser :consume))
               ;; multiple lines comment
               ((char-equal #\# char)
                (let ((next (funcall parser :peek 1))
                      (curr nil))
                  (when (char-equal #\| next)
                    (funcall parser :advance)
                    (loop (setf curr (funcall parser :peek)
                                next (funcall parser :peek 1))
                          (when (or (null next)
                                    (and (char-equal #\# next)
                                         (char-equal #\| curr)))
                            (funcall parser :advance)
                            (return))
                          (funcall parser :advance))
                    (setf end (1+ (funcall parser :index)))))
                (funcall parser :consume))
               ;; string
               ((char-equal #\" char)
                (let ((curr nil)
                      (next nil))
                  (loop (setf curr (funcall parser :peek)
                              next (funcall parser :peek 1))
                        (when (or (null next)
                                  (and (char-equal #\" next)
                                       (char-not-equal #\\ curr)))
                          (funcall parser :advance)
                          (return))
                        (funcall parser :advance))
                  (setf end (1+ (funcall parser :index))))
                (funcall parser :consume))
               ;; terminal
               ((or (char-equal #\Space char)
                    (char-equal #\Tab char)
                    (char-equal #\Newline char)
                    (char-equal #\( char)
                    (char-equal #\' char))
                (setf end (1+ (funcall parser :index)))
                (funcall parser :consume))
               ;; package
               ((char-equal #\: char)
                (when (char-equal #\: (funcall parser :peek 1))
                  (let ((col #\:))
                    (loop (setf col (funcall parser :peek 1))
                          (when (or (null col)
                                    (char-not-equal #\: col))
                            (return))
                          (funcall parser :advance))))
                (let ((pchar (funcall parser :peek -1)))
                  (unless (or (char-equal #\Space pchar)
                              (char-equal #\Tab pchar)
                              (char-equal #\Newline pchar)
                              (char-equal #\( pchar)
                              (char-equal #\\ pchar))
                    (if (> start end)
                        (setf end start)
                        (princ (subseq (funcall parser :string)
                                       start end)
                               output))
                    (setf strip-p t)
                    (setf start (1+ (funcall parser :index)))))
                (funcall parser :consume))
               ;; default
               (t (funcall parser :consume)))))
      ;; refactor using loop because control stack exhausted when
      ;; using recursive for bigger string
      (loop for i from 0 upto max-loop
            for c = (step-fun (funcall parser :peek))
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

