(cl:in-package :clarafx.draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser
;;;
(defvar *parser* nil)

(defun make-parser (string)
  ;; Base variables
  (let ((ori-string string)
        (ori-length (length string))
        (ori-index 0))
    ;; Base functions
    (labels ((index-validp (&optional (n 0))
               (let ((index (+ ori-index n)))
                 (< -1 index ori-length)))
             (advance (&optional (n 1))
               (incf ori-index n))
             (peek (&optional (n 0))
               (when (index-validp n)
                 (aref ori-string (+ ori-index n))))
             (consume (&optional (n 1))
               (when (index-validp)
                 (prog1 (peek (1- n))
                   (advance n)))))
      (lambda (command &rest args)
        (ecase command
          (:peek (apply #'peek args))
          (:advance (apply #'advance args))
          (:consume (apply #'consume args))
          (:index ori-index)
          (:string ori-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing Commands
;;;
(defun split-element-drawing-commands (string)
  (declare (type string string))
  (let ((*parser* (make-parser string))
        (result '())
        (max-loop (length string)))
    (flet ((step-fun (&optional char)
             (cond
               ((null char) :stop)
               ;; command
               ((case char
                  ((#\M #\m) (push #\m result))
                  ((#\N #\n) (push #\n result))
                  ((#\L #\l) (push #\l result))
                  ((#\B #\b) (push #\b result))
                  ((#\S #\s) (push #\s result))
                  ((#\P #\p) (push #\p result))
                  ((#\C #\c) (push #\c result)))
                (funcall *parser* :consume))
               ;; argument
               ((or (char-equal #\- char)
                    (char-equal #\. char)
                    (digit-char-p char))
                (let ((index (funcall *parser* :index))
                      (dotp nil)
                      (next nil))
                  ;; peek next char
                  (loop (setf next (funcall *parser* :peek 1))
                        (when (or (null next)
                                  (char-equal #\- next)
                                  (char-equal #\Space next)
                                  (alpha-char-p next))
                          (setf dotp nil)
                          (return))
                        (when (char-equal #\. next)
                          (if (null dotp)
                              (setf dotp t)
                              (return)))
                        (funcall *parser* :advance))
                  ;; push numeric argument
                  (push (read-from-string
                         (subseq (funcall *parser* :string)
                                 index
                                 (1+ (funcall *parser* :index))))
                        result)
                  (funcall *parser* :consume)))
               ;; ignored
               (t (funcall *parser* :consume)))))
      (loop for i from 0 upto max-loop
            for c = (step-fun (funcall *parser* :peek))
            when (eql :stop c)
              do (return t))
      (reverse result))))

(defun drawing-commands-expr (object)
  (declare (type drawing-commands object))
  (with-output-to-string (stream)
    (princ "(list" stream)
    (loop with prev-name = nil
          for drawing-command in (drawing-commands object)
          do (let ((name (name drawing-command))
                   (points (points drawing-command)))
               (princ #\Newline stream)
               (case (character name)
                 (#\m (princ "(make-dc-move" stream))
                 (#\n (princ "(make-dc-no-close-move" stream))
                 (#\l (princ "(make-dc-line" stream))
                 (#\b (princ "(make-dc-bezier" stream))
                 (#\s (princ "(make-dc-cubic-bsp" stream))
                 (#\p (princ "(make-dc-extend-bsp " stream))
                 (#\c (princ "(make-dc-close-bsp" stream)))
               (unless (null points)
                 (princ #\Space stream)
                 (loop for (point next) on points
                       do (princ (point-string point) stream)
                          (unless (null next)
                            (princ #\Space stream))))
               (princ #\) stream)))
    (princ #\) stream)))

(defvar *drawing-command-names* (coerce "mnlbspc" 'list))

(defun drawing-command-name-validp (name)
  (and (or (stringp name)
           (characterp name))
       (member name *drawing-command-names* :test 'string-equal)
       t))

(defun parse-drawing-command (sequence)
  (cond ((stringp sequence)
         (parse-drawing-command
          (split-element-drawing-commands sequence)))
        ((consp sequence)
         (let ((char (character (first sequence)))
               (args (rest sequence)))
           (ecase char
             (#\m (apply 'make-dc-move args))
             (#\n (apply 'make-dc-no-close-move args))
             (#\l (apply 'make-dc-lines args))
             (#\b (apply 'make-dc-beziers args))
             (#\s (apply 'make-dc-cubic-bsp args))
             (#\p (apply 'make-dc-extend-bsp args))
             (#\c (make-dc-close-bsp)))))
        (t (error "SEQUENCE is not drawing command."))))

(defun parse-drawing-commands (commands)
  (cond ((stringp commands)
         (parse-drawing-commands
          (split-element-drawing-commands commands)))
        ((consp commands)
         (let ((result '())
               (start 0)
               (dcs (make-instance 'drawing-commands)))
           (labels ((collect-commands ()
                      (let* ((end (position-if
                                   (complement #'numberp)
                                   commands :start (1+ start)))
                             (command (parse-drawing-command
                                       (subseq commands start end))))
                        (if (atom command)
                            (push command result)
                            (mapc (lambda (cmd)
                                    (push cmd result))
                                  command))
                        (setf start end)
                        (unless (null end)
                          (collect-commands)))))
             (collect-commands)
             (setf (drawing-commands dcs) (reverse result))
             (values dcs))))
        (t (error "COMMANDS is not proper drawing commands."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SVG Path Data (Tested with GIMP and Karbon)
;;;
(defun svg-data-to-element-drawing-commands (string)
  (declare (type string string))
  (let ((*parser* (make-parser string))
        (result '())
        (max-loop (length string)))
    (flet ((step-fun (&optional char)
             (cond
               ((null char) :stop)
               ;; command
               ((case char
                  ((#\M #\m) (push #\m result))
                  ((#\L #\l) (push #\l result))
                  ((#\C #\c) (push #\b result))
                  ((#\Z #\z) (push #\c result)))
                (funcall *parser* :consume))
               ;; argument
               ((or (char-equal #\- char)
                    (char-equal #\. char)
                    (digit-char-p char))
                (let ((index (funcall *parser* :index))
                      (dotp nil)
                      (next nil))
                  ;; peek next char
                  (loop (setf next (funcall *parser* :peek 1))
                        (when (or (null next)
                                  (char-equal #\, next)
                                  (char-equal #\- next)
                                  (char-equal #\Space next)
                                  (alpha-char-p next))
                          (setf dotp nil)
                          (return))
                        (when (char-equal #\. next)
                          (if (null dotp)
                              (setf dotp t)
                              (return)))
                        (funcall *parser* :advance))
                  ;; push numeric argument
                  (push (read-from-string
                         (subseq (funcall *parser* :string)
                                 index
                                 (1+ (funcall *parser* :index))))
                        result)
                  (funcall *parser* :consume)))
               ;; ignored
               (t (funcall *parser* :consume)))))
      (loop for i from 0 upto max-loop
            for c = (step-fun (funcall *parser* :peek))
            when (eql :stop c)
              do (return t))
      (reverse result))))

(defun svg-path-to-drawing-commands (path.svg &key (rawp t))
  (declare (type pathname path.svg))
  ;; limitation: only first path from SVG will be proceed
  (let* ((svg-string (alexandria:read-file-into-string path.svg))
         (str1 " d=\"")
         (str2 "\"")
         (pos1 (search str1 svg-string :test 'string-equal))
         (pos2 (search str2 svg-string :test 'string-equal
                                       :start2 (+ pos1 (length str1))))
         (svg-data (subseq svg-string (+ pos1 (length str1)) pos2)))
    #+sbcl (declare (dynamic-extent str1 str2 pos1 pos2
                                    svg-string svg-data))
    (let ((dcs (parse-drawing-commands
                (svg-data-to-element-drawing-commands svg-data))))
      (if (null rawp)
          (drawing-commands-string dcs)
          (values dcs)))))

