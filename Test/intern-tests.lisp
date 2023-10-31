(cl:in-package #:clarafx-test)

(in-suite intern-suite)

(test strip-package-from-symbol
  (let ((0str0 "symbol")
        (0str1 "package:symbol")
        (0str2 "package::symbol")
        (0str3 "incorrect:::symbol")
        (0str4 "incorrect:package:symbol")
        (1str0 "(list :key1 #'symbol :key2 'symbol :key3 symbol)")
        (1str1 "(list :key1 #'pkg:symbol :key2 'pkg:symbol :key3 pkg:symbol)")
        (1str2 "(list :key1 #'pkg::symbol :key2 'pkg::symbol :key3 pkg::symbol)")
        (2str0 "#\\\" \"pkg:sym\\\"pkg::sym\\\"\" 'symbol \"pkg:sym\\\"pkg::sym\" symbol")
        (2str1 "#\\\" \"pkg:sym\\\"pkg::sym\\\"\" 'pkg:symbol \"pkg:sym\\\"pkg::sym\" pkg:symbol")
        (2str2 "#\\\" \"pkg:sym\\\"pkg::sym\\\"\" 'pkg::symbol \"pkg:sym\\\"pkg::sym\" pkg::symbol")
        (3str0 (format nil "; packag#:symbol~%'symbol"))
        (3str1 (format nil "; packag#:symbol~%'packag#:symbol"))
        (3str2 (format nil "; packag#:symbol~%'packag#::symbol"))
        (4str0 (format nil "#|~%p:s #| p::s~%\"p:s~%; p:s~%|#~%'symbol #|# p:s #|"))
        (4str1 (format nil "#|~%p:s #| p::s~%\"p:s~%; p:s~%|#~%'package:symbol #|# p:s #|"))
        (4str2 (format nil "#|~%p:s #| p::s~%\"p:s~%; p:s~%|#~%'package::symbol #|# p:s #|"))
        ;; check against previous NULL (#\: #\") and next NULL (#\" #\|)
        (5str0 ":string without-close\"")
        (6str0 "\"comment\" #|without-close|")
        ;; check against irregular
        (7str0 "':symbol '#:symbol #:symbol '(#:symbol) #.(function arg) '1 #b1 #o7 #10r9 #xf #c(1 2) #1a(1 2 3)")
        ;; check against pathname
        (8str0 "#p\"host:~/dir-*/file-*.ext\""))
    (flet ((strip (str)
             (clarafx.user::strip-package-from-symbol str)))
      (is (string= 0str0 (strip 0str1)))
      (is (string= 0str0 (strip 0str2)))
      (is (string= 0str0 (strip 0str3)))
      (is (string= 0str0 (strip 0str4)))
      (is (string= 1str0 (strip 1str1)))
      (is (string= 1str0 (strip 1str2)))
      (is (string= 2str0 (strip 2str1)))
      (is (string= 2str0 (strip 2str2)))
      (is (string= 3str0 (strip 3str1)))
      (is (string= 3str0 (strip 3str2)))
      (is (string= 4str0 (strip 4str1)))
      (is (string= 4str0 (strip 4str2)))
      (is (string= 5str0 (strip 5str0)))
      (is (string= 6str0 (strip 6str0)))
      (is (string= 7str0 (strip 7str0)))
      (is (string= 8str0 (strip 8str0))))))

(test split-element-drawing-commands
  (let ((edcs (list #\m -5.4 4.3 #\l 3.2 -2.1 1.1 0.1 1.2 -2.3 3.4 -4.5))
        (str1 "m -5.4 4.3 l 3.2 -2.1 1.1 0.1 1.2 -2.3 3.4 -4.5")
        (str2 "m-5.4 4.3l3.2-2.1 1.1.1 1.2-2.3 3.4-4.5")
        (str3 "M-5.4 4.3L3.2-2.1 1.1.1 1.2-2.3 3.4-4.5")
        (str4 "M-5.4J4.3l3.2-2.1j1.1.1U1.2-2.3u3.4-4.5"))
    (flet ((split (str)
             (clarafx.draw::split-element-drawing-commands str)))
      (is (equal edcs (split str1)))
      (is (equal edcs (split str2)))
      (is (equal edcs (split str3)))
      (is (equal edcs (split str4))))))

(test svg-data-to-element-drawing-commands
  (let ((edcs (list #\m -5.4 4.3 #\b 3.2 -2.1 1.1 0.1 1.2 -2.3))
        (str1 "m -5.4 4.3 c 3.2 -2.1 1.1 0.1 1.2 -2.3")
        (str2 "m-5.4 4.3c3.2-2.1 1.1.1 1.2-2.3")
        (str3 "M-5.4 4.3C3.2-2.1 1.1.1 1.2-2.3")
        (str4 "M-5.4J4.3c3.2-2.1j1.1.1U1.2-2.3"))
    (flet ((convert (str)
             (clarafx.draw::svg-data-to-element-drawing-commands str)))
      (is (equal edcs (convert str1)))
      (is (equal edcs (convert str2)))
      (is (equal edcs (convert str3)))
      (is (equal edcs (convert str4))))))
