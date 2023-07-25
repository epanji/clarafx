(cl:in-package :claraoke-font)

#+(or)
(defparameter *ini*
  (make-instance
   'alignment
   :canvas
   (make-instance 'canvas
                  :w 640 :h 360
                  :margin-top 75
                  :margin-bottom 75
                  :margin-left 25
                  :margin-right 25)
   :an 1
   :syllables
   (list
    (make-syllable "First " :size 32 :space 0)
    (make-syllable "Second " :size 32 :space 0)
    (make-syllable "Third " :size 32 :space 0)
    (make-syllable "Forth " :size 32 :space 0)
    (make-syllable "Fifth" :size 32 :space 0)
    ;; (make-syllable "Sixth " :size 32 :space 0)
    ;; (make-syllable "Seventh " :size 32 :space 0)
    ;; (make-syllable "Eighth " :size 32 :space 0)
    ;; (make-syllable "Ninth " :size 32 :space 0)
    ;; (make-syllable "Tenth" :size 32 :space 0)
    )))


#+(or)
(let* ((subt (claraoke:subtitle "Coba" :text nil :alignment 1
                                       :fontsize 32
                                       :play-res-x 640
                                       :play-res-y 360
                                       :style-margin-l 25
                                       :style-margin-r 25
                                       :style-margin-v 75)))
  (setf (alignment-code *ini*) 3)
  (loop for syl in (syllables *ini*)
        do (claraoke:insert-event subt (claraoke:dialogue
                                        (format nil "{\\an1\\pos(~A,~A)}~A"
                                                (point-x1 syl)
                                                (point-y2 syl)
                                                (plain-text syl))
                                        :end "1:" :layer 11)))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an1\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 1))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an2\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 2))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an3\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 3))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an4\\1c&ff0000&}First Second Third Forth Fifth" :end "1:" :layer 4))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an5\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 5))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an6\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 6))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an7\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 7))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an8\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 8))
  ;; (claraoke:insert-event subt (claraoke:dialogue "{\\an9\\1c&ff0000&}First Second Third Forth Fifth Sixth Seventh Eighth Ninth Tenth" :end "1:" :layer 9))
  (claraoke-user:claraoke-subtitle-to-file subt #p"/tmp/mmm.ass")
  (claraoke-user:claraoke-subtitle-dummy-video subt #p"/tmp/mmm.mp4")
  (uiop:run-program "mplayer /tmp/mmm.mp4"))

#+(or)
(let* ((subt (claraoke:subtitle "Coba" :text nil
                                       :alignment 1
                                       :fontsize 40
                                       :play-res-x 1280
                                       :play-res-y 720
                                       :style-margin-l 25
                                       :style-margin-r 25
                                       :style-margin-v 75))
       (canv (make-canvas subt "Default"))
       (dlog (claraoke:dialogue "{\\k100}First {\\k200}Second"))
       (algn (make-alignment 2 canv dlog)))
  (setf (alignment-code algn) 4)
  (loop for syl in (syllables algn)
        do (claraoke:insert-event subt (claraoke:dialogue
                                        (format nil "{\\an1\\pos(~A,~A)\\org(~A,~A)\\t(0,1000,\\fr720)}~A"
                                                (point-x1 syl)
                                                (point-y2 syl)
                                                (- (point-x2 syl) (floor (width syl) 2))
                                                (- (point-y2 syl) (floor (height syl) 2))
                                                (plain-text syl))
                                        :start (start syl)
                                        :end "1:"
                                        :layer 11)))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an1\\1c&ff0000&}First Second" :end "1:" :layer 1))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an2\\1c&ff0000&}First Second" :end "1:" :layer 2))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an3\\1c&ff0000&}First Second" :end "1:" :layer 3))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an4\\1c&ff0000&}First Second" :end "1:" :layer 4))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an5\\1c&ff0000&}First Second" :end "1:" :layer 5))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an6\\1c&ff0000&}First Second" :end "1:" :layer 6))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an7\\1c&ff0000&}First Second" :end "1:" :layer 7))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an8\\1c&ff0000&}First Second" :end "1:" :layer 8))
  (claraoke:insert-event subt (claraoke:dialogue "{\\an9\\1c&ff0000&}First Second" :end "1:" :layer 9))
  (claraoke-user:claraoke-subtitle-to-file subt #p"/tmp/aaa.ass")
  (claraoke-user:claraoke-subtitle-dummy-video subt #p"/tmp/aaa.mp4")
  (uiop:run-program "mplayer /tmp/aaa.mp4"))


;;; TODO split lines when outside drawing area
#+(or)
(let ((subt (claraoke:subtitle "Coba" :text nil
                                      :fontname "Georgia"
                                      :bold -1
                                      :italic 0
                                      :spacing 0
                                      :fontsize 42
                                      :shadow 0
                                      :outline 1
                                      :alignment 6
                                      :play-res-x 1280
                                      :play-res-y 720))
      (dlog (claraoke:dialogue "This is macro for creating KARAOKE effects"
                               :generate-overrides-p t :start "0.05" :end "10.0"
                               :spell-duration 50)))
  (claraoke:insert-event subt dlog :start 0 :end "10.0" :layer 10)
  (with-every-syllable-from-karaoke
      (syl dlog subt 2 "Default")
    (claraoke:insert-event
     subt (claraoke:dialogue
           (plain-text syl)
           :overrides
           (list (claraoke:override
                  'alignment-numpad 0
                  :arg1 1
                  :modifiers
                  (list (claraoke:modifier 'pos :arg1 (point-x1 syl) :arg2 (point-y2 syl))
                        (claraoke:modifier 'origin :arg1 (- (point-x2 syl) (floor (width syl) 2)) :arg2 (- (point-y2 syl) (floor (height syl) 2)))
                        (claraoke:modifier 'color1 :arg1 (claraoke:random-color 1/11))
                        (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'fontrotate-z :arg1 720)))))
           :start (start syl)
           :end "10.0"
           :layer 11))
    syl)
  (with-every-syllable-from-karaoke
      (syl dlog subt 6 "Default")
    (claraoke:insert-event
     subt (claraoke:dialogue
           (plain-text syl)
           :overrides
           (list (claraoke:override
                  'alignment-numpad 0
                  :arg1 1
                  :modifiers
                  (list (claraoke:modifier 'pos :arg1 (point-x1 syl) :arg2 (point-y2 syl))
                        (claraoke:modifier 'origin :arg1 (- (point-x2 syl) (floor (width syl) 2)) :arg2 (- (point-y2 syl) (floor (height syl) 2)))
                        (claraoke:modifier 'color1 :arg1 (claraoke:random-color 1/11))
                        (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'alpha :arg1 200)))))
           :start (start syl)
           :end "10.0"
           :layer 11))
    syl)
  (with-every-syllable-from-karaoke
      (syl dlog subt 7 "Default")
    (claraoke:insert-event
     subt (claraoke:dialogue
           (plain-text syl)
           :overrides
           (list (claraoke:override
                  'alignment-numpad 0
                  :arg1 1
                  :modifiers
                  (list (claraoke:modifier 'pos :arg1 (point-x1 syl) :arg2 (point-y2 syl))
                        (claraoke:modifier 'origin :arg1 (- (point-x2 syl) (floor (width syl) 2)) :arg2 (- (point-y2 syl) (floor (height syl) 2)))
                        (claraoke:modifier 'color1 :arg1 (claraoke:random-color 1/11))
                        (claraoke:modifier 'transformation3 :arg1 0 :arg2 1000 :arg3 (claraoke:modifier 'fontrotate-z :arg1 720)))))
           :start (start syl)
           :end "10.0"
           :layer 11))
    syl)
  (claraoke-user:claraoke-subtitle-to-file subt #p"/tmp/bbb.ass")
  (claraoke-user:claraoke-subtitle-dummy-video subt #p"/tmp/bbb.mp4" "gray70")
  (uiop:run-program "mplayer /tmp/bbb.mp4"))
