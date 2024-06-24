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
           (sub3 (subtitle "Test" :text str0 :start 100 :duration 300 :effect "clarafx-1"))
           (end1 nil)
           (end2 nil)
           (sta1 nil)
           (sta3 nil))
      ;; Add info effect
      (insert-info sub1 (info "clarafx-1" :value "shaking"))
      (insert-info sub2 (info "clarafx-1" :value "shaking"))
      (insert-info sub3 (info "clarafx-1" :value "shaking"))
      ;; Change dialogue
      (populate-zero-karaoke (last-event sub1))
      (populate-zero-karaoke (last-event sub2))
      (populate-delay-effect (last-event sub2) 21)
      (populate-zero-karaoke (last-event sub3))
      (populate-delay-effect (last-event sub3) -21)
      ;; Do preparation
      (setf str1 (.text (.text (last-event sub1))))
      (setf str2 (.text (.text (last-event sub2))))
      (parse-effect sub1)
      (parse-effect sub2)
      (parse-effect sub3)
      (setf end1 (end (find-event sub1 0)))
      (setf end2 (end (find-event sub2 0)))
      (setf sta1 (start (find-event sub1 0)))
      (setf sta3 (start (find-event sub3 0)))
      ;; Do tests
      (is (string= (princ-plain-text sub1) (princ-plain-text sub2)))
      (is (string/= (princ-text sub1) (princ-text sub2)))
      (is (string= str1 (subseq str2 1)))
      (is (string/= str1 str2))
      (is (char= #\INVISIBLE_SEPARATOR (char str2 0)))
      (is (= 21 (duration-difference end1 end2)))
      (is (duration-lessp end1 end2))
      (is (= 21 (duration-difference sta1 sta3)))
      (is (duration-greaterp sta1 sta3)))))

(test populate-odd-event-effect
  (let ((sub1 (subtitle "text" :text nil)))
    (insert-info sub1 (info "clarafx-1" :value "shaking"))
    (insert-info sub1 (info "clarafx-2" :value "dropping"))
    (with-auto-increment-events
      sub1 30 10 1 nil
      "dialogue odd"                    ;1
      "dialogue even"                   ;2
      "dialogue odd 1"                  ;3
      '("dialogue even 1" -1)           ;4
      "dialogue odd 2"                  ;5
      "dialogue even 2"                 ;6
      "dialogue odd 3"                  ;7
      '("dialogue even 3" 0 0)          ;8
      "dialogue odd 4"                  ;9
      '("dialogue even 4" 0 1 50)       ;10
      "dialogue odd 5"                  ;11
      5
      "dialogue even 5")                ;12
    (populate-odd-even-effect sub1 "clarafx-1" "clarafx-2")
    (insert-event sub1 (dialogue "Title" :start 0 :duration (end (last-event sub1))))
    (sort-events sub1)
    (is (string-equal "" (effect (find-event sub1 0))))
    (is (string-equal "Title" (.text (.text (find-event sub1 0)))))
    (is (string-equal "clarafx-1" (effect (find-event sub1 1))))
    (is (string-equal "dialogue odd" (.text (.text (find-event sub1 1)))))
    (is (string-equal "clarafx-2" (effect (find-event sub1 2))))
    (is (= (* 30 10) (durationinteger (duration-length (find-event sub1 3)))))
    (is (= (* 30 9) (durationinteger (duration-length (find-event sub1 4)))))
    (is (= 30 (duration-difference (end (find-event sub1 5)) (start (find-event sub1 6)))))
    (is (zerop (duration-difference (end (find-event sub1 7)) (start (find-event sub1 8)))))
    (is (null (increase-karaoke (find-override (find-event sub1 9) 0) 0)))
    (is (not (null (increase-karaoke (find-override (find-event sub1 10) 0) 0))))
    (is (= (* 30 5) (duration-difference (end (find-event sub1 11)) (start (find-event sub1 12)))))))

(test partial-effects
  (let ((str0 "{\\k15}Hel{\\k15}lo {\\k15}world!")
        (str1 "{\\k15}Hel{\\k15}lo {\\k15\\what\\part\\it\\is}world!")
        (str2 "{\\k15}Hello {\\k15}world!")
        (str3 "{\\k15}Hel{\\k15}lo \\Nworld!")
        (dia0 nil)
        (dia1 nil)
        (dia2 nil)
        (sub1 (subtitle "Test" :text nil)))
    (flet ((length-effect (&rest args)
             (length (apply 'clarafx.user::shrink-each-syllables args))))
      (setf dia0 (dialogue str0))
      (setf dia1 (dialogue str1))
      (setf dia2 (dialogue str1 :remove-unknown-modifier-p t))
      (insert-info sub1 (info "clarafx-1" :value "shaking,,,5,true"))
      (insert-info sub1 (info "clarafx-2" :value "dropping,,,5,true"))
      (insert-info sub1 (info "clarafx-3" :value "shrink,,,5,true"))
      (insert-event sub1 (dialogue str0 :start 0 :end ":5.00" :effect "clarafx-1"))
      (insert-partial (last-event sub1) "Hel")
      (insert-event sub1 (dialogue str2 :start 0 :end ":5.00" :effect "clarafx-2"))
      (insert-partial (last-event sub1) "lo")
      (insert-karaoke (last-event sub1) "lo" 15)
      (insert-event sub1 (dialogue str3 :start 0 :end ":5.00" :effect "clarafx-3"))
      (insert-partial (last-event sub1) "world")
      (insert-karaoke (last-event sub1) "world" 15)
      ;; Do mutate sub1
      (parse-effect sub1)
      ;; Do tests
      (is (null (find-partial dia0)))
      (is (null (find-partial dia2)))
      (is (find-partial dia1))
      (is (string-equal str0 (ps-string (.text dia2))))
      (is (= 6 (length (lines (events sub1)))))
      (is (= 3 (length-effect dia1 :subtitle sub1 :ignore-partial t)))
      (is (= 1 (length-effect dia1 :subtitle sub1 :ignore-partial nil)))
      (is (= 3 (length-effect dia2 :subtitle sub1 :ignore-partial nil))))))

