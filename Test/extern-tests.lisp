(cl:in-package #:clarafx-test)

(defun sub-string (sub)
  (with-output-to-string (stream)
    (print-script sub stream)))

(defparameter *sub1* nil)
(defparameter *sub2* nil)
(defparameter *sub-string* nil)

(when (and (null *sub1*)
           (null *sub2*)
           (null *sub-string*))
  ;; create subtitle
  (setf *sub1* (subtitle "Test" :text nil))
  (insert-info *sub1* (info "clarafx-1" :value "shrink,Default,62,5,true"))
  (insert-event *sub1* (dialogue "Hello world!" :effect "clarafx-1"))
  ;; duplicate *sub1*
  (setf *sub-string* (sub-string *sub1*))
  (setf *sub2* (parse-script *sub-string*))
  ;; mutate *sub2*
  (parse-effect *sub2*))

(in-suite extern-suite)

(test parse-effect
  (is (string-not-equal *sub-string* (sub-string *sub2*)))
  (is (/= (length (lines (events *sub1*)))
          (length (lines (events *sub2*))))))

(test parse-drawing-commands
  (let ((1str0 "m -5.4 4.3 l 3.2 -2.1 1.1 0.1 1.2 -2.3 3.4 -4.5")
        (1str1 "m-5.4 4.3l3.2-2.1 1.1.1 1.2-2.3 3.4-4.5")
        (2str0 (format nil "(list~%(make-dc-move -5.4 4.3)~
                            ~%(make-dc-line 3.2 -2.1)~
                            ~%(make-dc-line 1.1 0.1)~
                            ~%(make-dc-line 1.2 -2.3)~
                            ~%(make-dc-line 3.4 -4.5))")))
    (is (typep (parse-drawing-commands 1str1) 'drawing-commands))
    (is (string= 1str0 (drawing-commands-string
                        (parse-drawing-commands 1str1))))
    (is (string= 2str0 (drawing-commands-expr
                        (parse-drawing-commands 1str1))))))

(test resize-drawing-commands
  (let ((dcs (parse-drawing-commands "m 2.0 3.0 l 4.0 5.0"))
        (str1 "m 4.0 3.0 l 8.0 5.0")
        (str2 "m 2.0 3.0 l 4.0 5.0")
        (str3 "m 2.0 6.0 l 4.0 10.0")
        (str4 "m 4.0 6.0 l 8.0 10.0"))
    (resize-x dcs 2)
    (is (string= str1 (drawing-commands-string dcs)))
    (resize-x dcs 1/2)
    (is (string= str2 (drawing-commands-string dcs)))
    (resize-y dcs 2)
    (is (string= str3 (drawing-commands-string dcs)))
    (resize-y dcs 1/2)
    (is (string= str2 (drawing-commands-string dcs)))
    (resize-xy dcs 2)
    (is (string= str4 (drawing-commands-string dcs)))
    (resize-xy dcs 1/2)
    (is (string= str2 (drawing-commands-string dcs)))
    (resize-xy dcs 2 2)
    (is (string= str4 (drawing-commands-string dcs)))
    (resize-xy dcs 1/2 1/2)
    (is (string= str2 (drawing-commands-string dcs)))))

(test moving-drawing-commands
  (let ((dcs (parse-drawing-commands "m 2 4 l 8 16"))
        (str1 "m 4 4 l 10 16")
        (str2 "m 2 4 l 8 16")
        (str3 "m 2 6 l 8 18")
        (str4 "m 4 6 l 10 18"))
    (moving-x dcs 2)
    (is (string= str1 (drawing-commands-string dcs)))
    (moving-x dcs -2)
    (is (string= str2 (drawing-commands-string dcs)))
    (moving-y dcs 2)
    (is (string= str3 (drawing-commands-string dcs)))
    (moving-y dcs -2)
    (is (string= str2 (drawing-commands-string dcs)))
    (moving-xy dcs 2)
    (is (string= str4 (drawing-commands-string dcs)))
    (moving-xy dcs -2)
    (is (string= str2 (drawing-commands-string dcs)))
    (moving-xy dcs 2 2)
    (is (string= str4 (drawing-commands-string dcs)))
    (moving-xy dcs -2 -2)
    (is (string= str2 (drawing-commands-string dcs)))))

(test negate-drawing-commands
  (let ((dcs (parse-drawing-commands "m 1 2 l 3 4"))
        (str1 "m -1 2 l -3 4")
        (str2 "m -1 -2 l -3 -4")
        (str3 "m 1 2 l 3 4"))
    (negate-x dcs)
    (is (string= str1 (drawing-commands-string dcs)))
    (negate-y dcs)
    (is (string= str2 (drawing-commands-string dcs)))
    (negate-xy dcs)
    (is (string= str3 (drawing-commands-string dcs)))))

(test round-drawing-commands
  (let ((dcs (parse-drawing-commands "m 1.0 2.0 l 3.0 4.0"))
        (str1 "m 1 2.0 l 3 4.0")
        (str2 "m 1 2 l 3 4"))
    (round-x dcs)
    (is (string= str1 (drawing-commands-string dcs)))
    (round-y dcs)
    (is (string= str2 (drawing-commands-string dcs)))
    (round-xy dcs)
    (is (string= str2 (drawing-commands-string dcs)))))

(test minmax-drawing-commands
  (let ((dcs (parse-drawing-commands "m 1 2 l 3 4")))
    (is (= 1 (min-x dcs)))
    (is (= 2 (min-y dcs)))
    (is (equal '(1 2) (min-xy dcs)))
    (is (= 3 (max-x dcs)))
    (is (= 4 (max-y dcs)))
    (is (equal '(3 4) (max-xy dcs)))
    (is (equal '(1 3) (minmax-x dcs)))
    (is (equal '(2 4) (minmax-y dcs)))
    (is (equal '(1 3 2 4) (minmax-xy dcs)))))

(test rotate-drawing-commands
  (let ((dcs (parse-drawing-commands "m 0 0 l 4 0"))
        (lst1 "m 0 0 l 0 -4")
        (lst2 "m 0 0 l 4 0")
        (lst3 "m 2 2 l 2 -2")
        (lst4 "m 2.0 2.0 l 2.0 -2.0")
        (lst5 "m 0.0 0.0 l 4.0 0.0"))
    (rotate-origin dcs 90)
    (is (string= lst1 (drawing-commands-string dcs)))
    (rotate-origin dcs -90)
    (is (string= lst2 (drawing-commands-string dcs)))
    (rotate-origin dcs 90 2 0)
    (is (string= lst3 (drawing-commands-string dcs)))
    (rotate-origin dcs -90 2 0)
    (is (string= lst2 (drawing-commands-string dcs)))
    (rotate-center dcs 90)
    (is (string= lst4 (drawing-commands-string dcs)))
    (rotate-center dcs -90)
    (is (string= lst5 (drawing-commands-string dcs)))))

