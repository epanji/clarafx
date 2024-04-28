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

