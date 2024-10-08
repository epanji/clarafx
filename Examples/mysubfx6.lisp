(require 'asdf)

(asdf:load-system "clarafx")

(in-package :clarafx.user)

(defparameter *sub* (subtitle "Test" :text nil :play-res-x 1280 :play-res-y 720))

(insert-style *sub* (style "title"
                           :fontname "DejaVu Sans"
                           :fontsize 68
                           :primary-colour "cadet blue 1"
                           :secondary-colour "gray 85"
                           :outline-colour "black"
                           :back-colour "#000000D4"
                           :margin-l 150
                           :margin-r 150
                           :margin-v 150
                           :outline 1
                           :shadow 3))

(insert-style *sub* (style "note"
                           :fontname "DejaVu Sans"
                           :fontsize 48
                           :primary-colour "cadet blue 1"
                           :secondary-colour "gray 85"
                           :outline-colour "black"
                           :back-colour "#000000D4"
                           :margin-l 150
                           :margin-r 150
                           :margin-v 268
                           :outline 1
                           :shadow 3))

(insert-style *sub* (style "left"
                           :fontname "DejaVu Sans"
                           :fontsize 48
                           :primary-colour "cadet blue 1"
                           :secondary-colour "gray 85"
                           :outline-colour "black"
                           :back-colour "#000000D4"
                           :margin-l 50
                           :margin-r 690
                           :margin-v 150
                           :outline 1
                           :shadow 5))

(insert-style *sub* (style "right"
                           :fontname "DejaVu Sans"
                           :fontsize 48
                           :primary-colour "cadet blue 1"
                           :secondary-colour "gray 85"
                           :outline-colour "black"
                           :back-colour "#000000D4"
                           :margin-l 690
                           :margin-r 50
                           :margin-v 150
                           :outline 1
                           :shadow 5))

(insert-info *sub* (info "clarafx-1" :value "gradation,title,62,8,true"))
(insert-info *sub* (info "clarafx-2" :value "gradation,note,62,8,true"))
(insert-info *sub* (info "clarafx-3" :value "rgb-range,left,62,2"))
(insert-info *sub* (info "clarafx-4" :value "bgr-range,right,62,2"))

(insert-event *sub* (dialogue "GRADATION COLORS"
                              :generate-overrides-p t
                              :start 0
                              :end ":8"
                              :effect "clarafx-1"))

(populate-zero-karaoke (last-event *sub*))

(insert-event *sub* (dialogue "Each line has gradation from secondary to primary colors"
                              :generate-overrides-p t
                              :start 10
                              :end ":7.50"
                              :effect "clarafx-2"))

(insert-event *sub* (dialogue "RGB range from primary to secondary colors"
                              :generate-overrides-p t
                              :start 20
                              :end ":7.50"
                              :effect "clarafx-3"))

(insert-event *sub* (dialogue "BGR range from primary to secondary colors"
                              :generate-overrides-p t
                              :start 30
                              :end ":7.50"
                              :effect "clarafx-4"))

;; Write effect to file
(write-subtitle-effect *sub* #p"/tmp/mysubfx6.ass" :supersede)

