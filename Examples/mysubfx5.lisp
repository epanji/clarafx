(require 'asdf)

(asdf:load-system "clarafx")

(in-package :clarafx.user)

(defparameter *sub* (subtitle "Test" :text nil :play-res-x 1280 :play-res-y 720))

(insert-style *sub* (style "one" :fontname "DejaVu Sans"
                                 :fontsize 48
                                 :primary-colour "cadet blue 1"
                                 :secondary-colour "gray 85"
                                 :outline-colour "black"
                                 :back-colour "#000000D4"
                                 :margin-l 150
                                 :margin-r 150
                                 :margin-v 150
                                 :outline 1
                                 :shadow 3
                                 :spacing 0))

(insert-info *sub* (info "clarafx-1" :value "enlarge-with-star,one,62,2,true"))
(insert-info *sub* (info "clarafx-2" :value "rotate-cw,one,62,2,true"))
(insert-info *sub* (info "clarafx-3" :value "shrink,one,62,2,true"))
(insert-info *sub* (info "clarafx-4" :value "rotate-ccw,one,62,2,true"))
(insert-info *sub* (info "clarafx-5" :value "shear-x,one,62,2,true"))
(insert-info *sub* (info "clarafx-6" :value "enlarge,one,62,2,true"))
(insert-info *sub* (info "clarafx-7" :value "shear-y,one,62,2,true"))
(insert-info *sub* (info "clarafx-8" :value "dropping,one,62,2,true"))
(insert-info *sub* (info "clarafx-9" :value "shaking,one,62,2,true"))

(insert-info *sub* (info "clarafx-10" :value "clip-expand-vertical,one,62,7"))
(insert-info *sub* (info "clarafx-11" :value "clip-expand-horizontal,one,62,6"))

(defparameter *overrided-text1* "{\\k20}This {\\k10}is {\\k30}dialogue\\N{\\k10}with {\\k20\\part}one {\\k20}partial {\\k20}effect")
(defparameter *overrided-text2* "{\\k20}This {\\k10}is {\\k30}dialogue\\N{\\k10}with {\\k20\\part}two {\\k20\\part}partial {\\k20}effects")
(defparameter *overrided-text3* "{\\k20}This {\\k10}is {\\k30}combination {\\k10}of {\\k20}multiple {\\k20}dialogues {\\k10}with {\\k20}partial {\\k20}effects")

;; One partial
(insert-event *sub* (dialogue *overrided-text1* :start 40 :duration ":7.60" :effect "clarafx-10"))

;; Two partials
(insert-event *sub* (dialogue *overrided-text2* :start 40 :duration ":7.60" :effect "clarafx-11"))

;; Multiple dialogues
(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-1"))
(insert-partial (last-event *sub*) "This")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-2"))
(insert-partial (last-event *sub*) "is co")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-3"))
(insert-partial (last-event *sub*) "combination")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-4"))
(insert-partial (last-event *sub*) "of")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-5"))
(insert-partial (last-event *sub*) "multiple")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-6"))
(insert-partial (last-event *sub*) "dialogues")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-7"))
(insert-partial (last-event *sub*) "with")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-8"))
(insert-partial (last-event *sub*) "partial")

(insert-event *sub* (dialogue *overrided-text3* :start 40 :duration ":7.60" :effect "clarafx-9"))
(insert-partial (last-event *sub*) "effects")

;; Write effect to file
(write-subtitle-effect *sub* #p"/tmp/mysubfx5.ass" :supersede)

