(require 'asdf)

(asdf:load-system "clarafx")

(in-package :clarafx.user)

(defparameter *sub* (subtitle "Test" :text nil
                                     :play-res-x 1280
                                     :play-res-y 720
                                     :fontname "DejaVu Sans"
                                     :fontsize 48
                                     :primary-colour "sea green 3"
                                     :secondary-colour "sea green 1"
                                     :outline-colour "gray 30"
                                     :back-colour "#000000D4"
                                     :style-margin-l 250
                                     :style-margin-r 250
                                     :style-margin-v 150
                                     :shadow 5))

(defparameter *d1* (dialogue "This is macro for creating KARAOKE effects"
                             :generate-overrides-p t :start "0.05" :end "10.0"
                             :spell-duration 50 :layer 100))

(defparameter *d2* (dialogue "This is macro for creating KARAOKE effects"
                             :generate-overrides-p t :start "10.05" :end "20.0"
                             :spell-duration 50 :layer 100))


(progn (setf (lines (events *sub*))
             (append (enlarge-with-star-each-syllables *d1* :subtitle *sub* :dpi 62 :alignment-code 9)
                     (complement-effect-syllables *d1* :subtitle *sub* :dpi 62 :alignment-code 9)
                     (clip-expand-vertical-each-syllables *d1* :subtitle *sub* :dpi 62 :alignment-code 5)
                     (complement-effect-syllables *d1* :subtitle *sub* :dpi 62 :alignment-code 5)
                     (clip-expand-horizontal-each-syllables *d1* :subtitle *sub* :dpi 62 :alignment-code 1)
                     (complement-effect-syllables *d1* :subtitle *sub* :dpi 62 :alignment-code 1)
                     ;;
                     (shear-x-each-syllables *d2* :subtitle *sub* :dpi 62 :alignment-code 7)
                     (complement-effect-syllables *d2* :subtitle *sub* :dpi 62 :alignment-code 7)
                     (vacum-center-top-each-syllables *d2* :subtitle *sub* :dpi 62 :alignment-code 5)
                     (complement-effect-syllables *d2* :subtitle *sub* :dpi 62 :alignment-code 5)
                     (shear-y-each-syllables *d2* :subtitle *sub* :dpi 62 :alignment-code 3)
                     (complement-effect-syllables *d2* :subtitle *sub* :dpi 62 :alignment-code 3)))

       (with-open-file (stream #p"/tmp/mysubfx2.ass" :direction :output
                                                     :if-does-not-exist :create
                                                     :if-exists :rename)
         (print-script *sub* stream)))

