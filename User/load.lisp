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
  (cond ((probe-file (current-effects-pathname))
         (current-effects-pathname))
        ((probe-file (user-effects-pathname))
         (user-effects-pathname))
        (t (warn "No external effects"))))

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
        (end 0))
    (declare (special parser))
    (labels ((recursive-fun (&optional char)
               (let ((p (cond
                          ;; finish
                          ((null char)
                           (princ (subseq (funcall parser :string)
                                          start nil)
                                  output)
                           nil)
                          ;; string
                          ((char-equal #\" char)
                           (let ((next nil))
                             (loop (setf next (funcall parser :peek 1))
                                   (when (char-equal #\" next)
                                     (funcall parser :advance)
                                     (return))
                                   (funcall parser :advance))
                             (princ (subseq (funcall parser :string)
                                            start (funcall parser :index))
                                    output)
                             (setf start (funcall parser :index)))
                           (funcall parser :consume))
                          ;; terminal
                          ((or (char-equal #\Space char)
                               (char-equal #\Tab char)
                               (char-equal #\Newline char)
                               (char-equal #\( char))
                           (setf end (1+ (funcall parser :index)))
                           (funcall parser :consume))
                          ;; package
                          ((char-equal #\: char)
                           (when (char-equal #\: (funcall parser :peek 1))
                             (funcall parser :advance))
                           (let ((pchar (funcall parser :peek -1)))
                             (unless (or (char-equal #\Space pchar)
                                         (char-equal #\Tab pchar)
                                         (char-equal #\Newline pchar)
                                         (char-equal #\( pchar))
                               (princ (subseq (funcall parser :string)
                                              start end)
                                      output)
                               (setf start (1+ (funcall parser :index)))))
                           (funcall parser :consume))
                          ;; default
                          (t (funcall parser :consume)))))
                 (if (null p)
                     (get-output-stream-string output)
                     (recursive-fun (funcall parser :peek))))))
      (prog1 (recursive-fun (funcall parser :peek))
        (close output)))))

(defun maybe-revision (file)
  (declare (type pathname file))
  (let ((ori (alexandria:read-file-into-string file)))
    (let ((rev (strip-package-from-symbol ori)))
      (unless (string-equal ori rev)
        (with-open-file (stream file :direction :output
                                     :if-exists :rename
                                     :if-does-not-exist :create)
          (princ rev stream))
        (values t)))))

(defun load-external-effects ()
  (let ((file (effects-pathname))
        (*package* (find-package :clarafx.load)))
    (unless (null file)
      (maybe-revision file)
      (handler-case (prog1 (load file)
                      (format *error-output* "~&Loading ~A~%" file))
        (error (condition)
          (format *error-output*
                  "~&~A~%Incomplete loading ~A~%"
                  condition file))))))

