(cl:in-package :clarafx-font)

(asdf:load-system "claraoke-user")

(let ((subtitle (claraoke:subtitle "Test" :text nil
                                          :play-res-x 1280
                                          :play-res-y 720
                                          :fontname "DejaVu Sans"
                                          :fontsize 48
                                          :primary-colour "&HC59C44&"
                                          :back-colour "#000000d4"
                                          :style-margin-l 250
                                          :style-margin-r 250
                                          :style-margin-v 150
                                          :shadow 5))
      (dialogue (claraoke:dialogue "This is macro for creating KARAOKE effects"
                                   :generate-overrides-p t :start "0.05" :end "10.0"
                                   :spell-duration 50 :layer 100)))
  (setf (claraoke:lines (claraoke:events subtitle))
        (reverse (append (rotate-ccw-syllables dialogue :subtitle subtitle :dpi 62 :alignment-code 9)
                         (resize-big-y-syllables dialogue :subtitle subtitle :dpi 62 :alignment-code :middle-center)
                         (resize-small-y-syllables dialogue :subtitle subtitle :dpi 62 :alignment-code 1))))
  ;; (claraoke:insert-event subtitle (claraoke:dialogue "{\\an1}This is macro for creating KARAOKE effects" :end "10.0" :layer 0))
  (claraoke-user:claraoke-subtitle-to-file subtitle #p"/tmp/bbb.ass")
  (claraoke-user:claraoke-subtitle-dummy-video subtitle #p"/tmp/bbb.mp4" "white")
  (uiop:run-program "mplayer -fs /tmp/bbb.mp4"))

