(require 'asdf)

(asdf:load-system "clarafx")

(in-package :clarafx.user)

(defparameter *sub* (subtitle "Test" :text nil :play-res-x 1280 :play-res-y 720))

(insert-style *sub* (style "fx1" :fontname "DejaVu Sans"
                                 :fontsize 48
                                 :primary-colour "sea green 3"
                                 :secondary-colour "sea green 1"
                                 :outline-colour "gray 30"
                                 :back-colour "#000000D4"
                                 :margin-l 250
                                 :margin-r 250
                                 :margin-v 150
                                 :shadow 5))

(insert-style *sub* (style "fx2" :fontname "DejaVu Sans"
                                 :fontsize 60
                                 :primary-colour "sea green 1"
                                 :secondary-colour "light blue 3"
                                 :outline-colour "steel blue 3"
                                 :back-colour "#000000D4"
                                 :margin-l 250
                                 :margin-r 250
                                 :margin-v 150
                                 :outline 10
                                 :spacing 18
                                 :shadow 5))

(define-effect (my-own-effect var)
  (modifier 'fad :arg1 250 :arg2 0)
  (modifier 'move
            :arg1 (- (width var)) :arg2 (base-y1 var)
            :arg3 (base-x1 var) :arg4 (base-y1 var)
            :arg5 0 :arg6 300))

(register-effect "my-own-effect" 'my-own-effect)

(insert-info *sub* (info "clarafx-1" :value "dropping,fx1,62,5,true"))
(insert-info *sub* (info "clarafx-2" :value "my-own-effect,fx1,62,5,true"))
(insert-info *sub* (info "clarafx-3" :value "shaking,,62,5"))

(insert-event *sub* (dialogue "{\\k150}This is macro for creating\\NKARAOKE effects"
                              :start "0.05" :end "5.0"
                              :layer 100
                              :effect "clarafx-1"))
(insert-event *sub* (dialogue "This is macro for creating KARAOKE effects"
                              :start "5.05" :end "10.0"
                              :layer 100
                              :effect "clarafx-2"))
(insert-event *sub* (dialogue "This is macro for creating KARAOKE effects"
                              :generate-overrides-p t :start "10.05" :end "20.0"
                              :spell-duration 10 :layer 100
                              :style "fx2"
                              :effect "clarafx-3"))

(insert-karaoke (last-event *sub*) "creating " 10)
(insert-karaoke (last-event *sub*) "ing " 10)

(insert-karaoke (last-event *sub*) "RAOKE " 10)
(insert-karaoke (last-event *sub*) "OKE " 10)
(insert-karaoke (last-event *sub*) "KE " 10)

(write-subtitle-effect *sub* #p"/tmp/mysubfx3.ass" :supersede)

