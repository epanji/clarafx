(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alignment code
;;;
(defmethod alignment-code ((input number))
  (ecase input
    ((1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
     (character (write-to-string input :base 22)))))

(defmethod alignment-code ((input character))
  (ecase input
    ((#\1
      #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\A #\B #\C #\D #\E #\F
      #\G #\H #\I #\J #\K #\L)
     (values input))))

(defmethod alignment-code ((input symbol))
  (ecase input
    ((:left-bottom :bottom-left) #\1)
    ((:center-bottom :bottom-center) #\2)
    ((:right-bottom :bottom-right) #\3)
    ((:left-middle :middle-left) #\4)
    ((:center-middle :middle-center) #\5)
    ((:right-middle :middle-right) #\6)
    ((:left-top :top-left) #\7)
    ((:center-top :top-center) #\8)
    ((:right-top :top-right) #\9)
    ;; normal
    ((:left-down :down-left) #\A)
    ((:center-down :down-center) #\B)
    ((:right-down :down-right) #\C)
    ((:left-balance :balance-left) #\D)
    ((:center-balance :balance-center) #\E)
    ((:right-balance :balance-right) #\F)
    ;; reverse
    ((:left-down* :down-left*) #\G)
    ((:center-down* :down-center*) #\H)
    ((:right-down* :down-right*) #\I)
    ((:left-balance* :balance-left*) #\J)
    ((:center-balance* :balance-center*) #\K)
    ((:right-balance* :balance-right*) #\L)))

(defmethod alignment-code ((input string))
  (if (= 1 (length input))
      (let ((input1 (character (string-upcase input))))
        (alignment-code input1))
      (let ((input2 (intern (string-upcase input) :keyword)))
        (alignment-code input2))))

(defmethod alignment-code :around ((input alignment))
  (let ((input1 (call-next-method input)))
    (alignment-code input1)))

