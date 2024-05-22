(cl:in-package #:clarafx-test)

(in-suite dialogue-suite)

(test populate-zero-karaoke
  (let ((str0 "{\\k10}a{\\k15}b{\\k20}c")
        (str1 "{\\k0}c{\\k10}a{\\k15}b")
        (str2 "{\\k0}a{\\k10}b{\\k0}b{\\k15}c{\\k0}c{\\k20}d")
        (str3 "{\\k0}H{\\k0}e{\\k0}l{\\k0}l{\\k10}o {\\k0}w{\\k0}o{\\k0}r{\\k0}l{\\k0}d{\\k15}!")
        (txt0 (text "{\\k10}a{\\k15}b{\\k20}c"))
        (txt1 (text "c{\\k10}a{\\k15}b"))
        (txt2 (text "{\\k10}ab{\\k15}bc{\\k20}cd"))
        (txt3 (text "{\\k10}Hello {\\k15}world!")))
    ;; Vertical alignment is the purpose for generating zero karaoke.
    ;; When calculating start time, every characters with zero karaoke
    ;; will start at the same time as the last character in syllables.
    (populate-zero-karaoke txt0)
    (populate-zero-karaoke txt1)
    (populate-zero-karaoke txt2)
    (populate-zero-karaoke txt3)
    (is (string= str0 (ps-string txt0)))
    (is (string= str1 (ps-string txt1)))
    (is (string= str2 (ps-string txt2)))
    (is (string= str3 (ps-string txt3)))))

(test populate-delay-effect
  (flet ((princ-plain-text (sub)
           (let ((dialogues (loop for ev in (lines (events sub))
                                  when (typep ev 'claraoke-subtitle:dialogue)
                                    collect (.text (.text ev)))))
             (with-output-to-string (stream)
               (print-script dialogues stream))))
         (princ-text (sub)
           (let ((dialogues (loop for ev in (lines (events sub))
                                  when (typep ev 'claraoke-subtitle:dialogue)
                                    collect (.text ev))))
             (with-output-to-string (stream)
               (print-script dialogues stream)))))
    ;; Adding invisible separator for each lines in subtitle will delay karaoke effect.
    ;; Invisible separator should not affect position calculation but timing.
    (let* ((str0 "{\\k60}祸从口出 {\\k75}欲速则不达 {\\k60}难得糊涂")
           (str1 nil)
           (str2 nil)
           (sub1 (subtitle "Test" :text str0 :start 100 :duration 300 :effect "clarafx-1"))
           (sub2 (subtitle "Test" :text str0 :start 100 :duration 300 :effect "clarafx-1"))
           (new1 nil)
           (new2 nil)
           (end1 nil)
           (end2 nil))
      ;; Add info effect
      (insert-info sub1 (info "clarafx-1" :value "shaking"))
      (insert-info sub2 (info "clarafx-1" :value "shaking"))
      ;; Change dialogue
      (populate-zero-karaoke (last-event sub1))
      (populate-zero-karaoke (last-event sub2))
      (populate-delay-effect (last-event sub2) 21)
      ;; Do preparation
      (setf str1 (.text (.text (last-event sub1))))
      (setf str2 (.text (.text (last-event sub2))))
      (setf new1 (read-subtitle-effect sub1))
      (setf new2 (read-subtitle-effect sub2))
      (setf end1 (end (find-event new1 0)))
      (setf end2 (end (find-event new2 0)))
      ;; Do tests
      (is (string= (princ-plain-text new1) (princ-plain-text new2)))
      (is (string/= (princ-text new1) (princ-text new2)))
      (is (string= str1 (subseq str2 1)))
      (is (string/= str1 str2))
      (is (char= #\INVISIBLE_SEPARATOR (char str2 0)))
      (is (= 21 (duration-difference end1 end2))))))

