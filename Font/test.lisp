(cl:in-package :clarafx-font)

;;; TODO split lines when outside drawing area

(let ((subt (claraoke:subtitle "Coba" :text nil
                                      :play-res-x 1280
                                      :play-res-y 720
                                      :fontname "DejaVu Sans"
                                      :fontsize 48
                                      :bold -1
                                      :primary-colour "&HC59C44&"
                                      :back-colour "#000000d4"
                                      :shadow 5))
      (dlog (claraoke:dialogue "This is macro for creating KARAOKE effects"
                               :generate-overrides-p t :start "0.05" :end "10.0"
                               :spell-duration 50)))
  (setf (claraoke:lines (claraoke:events subt))
        (reverse (append (rotate-ccw-syllables dlog :subtitle subt :dpi 61 :alignment-code 9)
                         (rotate-cw-syllables dlog :subtitle subt :dpi 61 :alignment-code 1)
                         (resize-big-y-syllables dlog :subtitle subt :dpi 61 :alignment-code 5))))
  (claraoke-user:claraoke-subtitle-to-file subt #p"/tmp/bbb.ass")
  (claraoke-user:claraoke-subtitle-dummy-video subt #p"/tmp/bbb.mp4" "white")
  (uiop:run-program "mplayer -fs /tmp/bbb.mp4"))
