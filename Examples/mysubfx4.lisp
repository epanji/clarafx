(require 'asdf)

(asdf:load-system "clarafx")

(in-package :clarafx.user)

(defparameter *sub* (subtitle "Test" :text nil :play-res-x 1280 :play-res-y 720))

(insert-style *sub* (style "V1" :fontname "WenQuanYi Zen Hei"
                                :fontsize 60
                                :primary-colour "light golden rod 2"
                                :secondary-colour "light blue 3"
                                :outline-colour "steel blue 3"
                                :back-colour "#000000D4"
                                :margin-l 250
                                :margin-r 250
                                :margin-v 150
                                :outline 1
                                :shadow 1
                                :spacing 0))

(insert-style *sub* (style "V2" :fontname "WenQuanYi Zen Hei"
                                :fontsize 60
                                :primary-colour "light golden rod 2"
                                :secondary-colour "light blue 3"
                                :outline-colour "steel blue 3"
                                :back-colour "#000000D4"
                                :margin-l 250
                                :margin-r 250
                                :margin-v 150
                                :outline 2
                                :shadow 2
                                :spacing 10))

(insert-style *sub* (style "V3" :fontname "WenQuanYi Zen Hei"
                                :fontsize 60
                                :primary-colour "light golden rod 2"
                                :secondary-colour "light blue 3"
                                :outline-colour "steel blue 3"
                                :back-colour "#000000D4"
                                :margin-l 250
                                :margin-r 250
                                :margin-v 150
                                :outline 3
                                :shadow 3
                                :spacing 20))

(insert-info *sub* (info "clarafx-1" :value "shaking,V1,62,a"))
(insert-info *sub* (info "clarafx-2" :value "shaking,V2,62,b"))
(insert-info *sub* (info "clarafx-3" :value "shaking,V3,62,c"))

(insert-info *sub* (info "clarafx-4" :value "shaking,V1,62,i"))
(insert-info *sub* (info "clarafx-5" :value "shaking,V2,62,h"))
(insert-info *sub* (info "clarafx-6" :value "shaking,V3,62,g"))

(insert-info *sub* (info "clarafx-7" :value "shaking,V1,62,d"))
(insert-info *sub* (info "clarafx-8" :value "shaking,V2,62,e"))
(insert-info *sub* (info "clarafx-9" :value "shaking,V3,62,f"))

(insert-info *sub* (info "clarafx-10" :value "shaking,V1,62,l"))
(insert-info *sub* (info "clarafx-11" :value "shaking,V2,62,k"))
(insert-info *sub* (info "clarafx-12" :value "shaking,V3,62,j"))

(setf (interval *sub*) 30)
(setf (interval-frequency *sub*) 15)
(setf (interval-counter *sub*) 0)

;; 祸从口出。 (Huò cóng kǒu chū/ 'disaster from mouth exits')
;; 欲速则不达。 (Yù sù zé bùdá / 'desire speed but not attain')
;; 难得糊涂。 (Nándé hútu / 'hard get confusion')

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-1"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-2"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-3"))

(incf (interval-counter *sub*) 100)

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-4"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-5"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-6"))

(incf (interval-counter *sub*) 100)

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-7"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-8"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-9"))

(incf (interval-counter *sub*) 100)

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-10"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-11"))

(insert-event *sub* (dialogue "祸从口出 欲速则不达 难得糊涂"
                              :generate-overrides-p t
                              :layer 100
                              :effect "clarafx-12"))

(setf (interval-counter *sub*) nil)

(write-subtitle-effect *sub* #p"/tmp/mysubfx4.ass" :supersede)

