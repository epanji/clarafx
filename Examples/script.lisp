;;;; Use this file from command line interface
;;;;
;;;; $ clarafx script.lisp
;;;;
(defparameter *sub* (subtitle "Love Frequency" :text nil :play-res-x 960 :play-res-y 540))

(insert-style *sub* (style "s1" :fontname "WenQuanYi Zen Hei"
                                :fontsize 30
                                :primary-colour "yellow"
                                :secondary-colour "light blue 3"
                                :outline-colour "steel blue 3"
                                :back-colour "#000000D4"
                                :margin-l (+ 220 30)
                                :margin-r (+ 220 30)
                                :margin-v 0
                                :outline 1
                                :shadow 1
                                :spacing 0))

(insert-style *sub* (style "s2" :fontname "WenQuanYi Zen Hei"
                                :fontsize 30
                                :primary-colour "yellow"
                                :secondary-colour "light blue 3"
                                :outline-colour "steel blue 3"
                                :back-colour "#000000D4"
                                :margin-l (+ 220 (* 3 30))
                                :margin-r (+ 220 (* 3 30))
                                :margin-v 0
                                :outline 1
                                :shadow 1
                                :spacing 0))

(insert-info *sub* (info "clarafx-1" :value "shaking,s1,,left-balance"))
(insert-info *sub* (info "clarafx-2" :value "shear-y,s2,,left-balance"))

(defparameter *1k* 30)
(defparameter *2k* (* 2 *1k*))
(defparameter *3k* (* 3 *1k*))
(defparameter *1/2k* (ceiling (* 1/2 *1k*)))

;;; Simplify insert events with auto increment for dialogue times.
;;; Below are description for arguments:
;;;
;;; SUBTITLE    is subtitle object targeted for elements.
;;; INTERVAL    is delay time before next line.
;;; FREQUENCY   is interval frequency for one line duration.
;;; COUNTER     is initial time (integer or duration format) for counter.
;;; KTIME       is initial karaoke time for spell duration.
;;; ELEMENT     is item with various value to be executed for subtitle object.
;;;             It could be integer, string, or cons: '(string &optional ±event 0+delay +spell).
;;;             The integer means to be interval frequency for blank line duration.
;;;             The string means to be dialogue text.
;;;             The cons will be parse to dialogue with specific options.
(with-auto-increment-events
  *sub* 30 10 "15.00" *1k*
  "Tell me why 想不透"
  "你讓我多麽心動"
  '("遇見你是生命中" -1)
  '("最美的彩虹" 0 0)
  2
  "冥冥之中你的回眸"
  '("最美的鏡頭" -1 0)
  '("想跟你走 想拉著你的手" 3 0)
  "可好像顯得太沖動"
  2
  "已經幻想靠在懷裡"
  '("已經幻想是我依偎著你" 0 0)
  '("你卻和我保持適當距離" -1)
  '("Ok 明白是我太過著急" 2 0)
  '("戀愛頻率開啟" 2)
  1
  "我會一點一點想你"
  "一天一遍放在心裡"
  "等到那天終於靠近"
  '("臉紅害羞就深呼吸" 1)
  1
  "我會一點一點想你"
  "一遍一遍慢慢累積"
  "累積成永恒的記憶"
  '("想夠一萬遍就在一起" 9)
  39
  ;; Reff
  "冥冥之中你的回眸"
  '("最美的鏡頭" -1 0)
  '("想跟你走 想拉著你的手" 3 0)
  "可好像顯得太沖動"
  2
  "已經幻想靠在懷裡"
  '("已經幻想是我依偎著你" 0 0)
  '("你卻和我保持適當距離" -1)
  '("Ok 明白是我太過著急" 2 0)
  '("戀愛頻率開啟" 2)
  1
  "我會一點一點想你"
  "一天一遍放在心裡"
  "等到那天終於靠近"
  '("臉紅害羞就深呼吸" 1)
  1
  "我會一點一點想你"
  "一遍一遍慢慢累積"
  "累積成永恒的記憶"
  '("想夠一萬遍就在一起" 9)
  4
  ;; Close
  "我會一點一點想你"
  "一天一遍放在心裡"
  "等到那天終於靠近"
  '("臉紅害羞就深呼吸" 1)
  1
  "我會一點一點想你"
  "一遍一遍慢慢累積"
  "累積成永恒的記憶"
  '("想夠一萬遍就在一起" 11))

(dolist (i '(0))
  ;; 0 "Tell me why 想不透"
  ;; 1 "你讓我多麽心動"
  ;; 2 "遇見你是生命中"
  ;; 3 "最美的彩虹"
  (insert-karaoke (find-event *sub* (+ i 0)) "why" *3k*)
  (insert-karaoke (find-event *sub* (+ i 1)) "你" *2k*)
  (insert-karaoke (find-event *sub* (+ i 1)) "多" *2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "你" *3k*))

(dolist (i '(4 21))
  ;; 0 "冥冥之中你的回眸"
  ;; 1 "最美的鏡頭"
  ;; 2 "想跟你走 想拉著你的手"
  ;; 3 "可好像顯得太沖動"
  (insert-karaoke (find-event *sub* (+ i 2)) "走" *3k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "著" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "你" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "可" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "好" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "像" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "顯" *1/2k*))

(dolist (i '(8 25))
  ;; 0 "已經幻想靠在懷裡"
  ;; 1 "已經幻想是我依偎著你"
  ;; 2 "你卻和我保持適當距離"
  ;; 3 "Ok 明白是我太過著急"
  ;; 4 "戀愛頻率開啟"
  (insert-karaoke (find-event *sub* (+ i 0)) "已" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 1)) "已" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "你" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "卻" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "和" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "明" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "白" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 4)) "戀" *2k*)
  (insert-karaoke (find-event *sub* (+ i 4)) "愛" *2k*)
  (insert-karaoke (find-event *sub* (+ i 4)) "開" *3k*))

(dolist (i '(13 30 38))
  ;; 0 "我會一點一點想你"
  ;; 1 "一天一遍放在心裡"
  ;; 2 "等到那天終於靠近"
  ;; 3 "臉紅害羞就深呼吸"
  (insert-karaoke (find-event *sub* (+ i 0)) "點想" *2k*)
  (insert-karaoke (find-event *sub* (+ i 1)) "在" *2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "那" *2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "於" *2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "紅" *2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "害" *2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "羞" *1/2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "就" *2k*))

(dolist (i '(17 34 42))
  ;; 0 "我會一點一點想你"
  ;; 1 "一遍一遍慢慢累積"
  ;; 2 "累積成永恒的記憶"
  ;; 3 "想夠一萬遍就在一起"
  (insert-karaoke (find-event *sub* (+ i 0)) "點想" *2k*)
  (insert-karaoke (find-event *sub* (+ i 1)) "遍慢" *2k*)
  (insert-karaoke (find-event *sub* (+ i 2)) "的" *2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "一萬" *2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "遍" *2k*)
  (insert-karaoke (find-event *sub* (+ i 3)) "在" *3k*))

(populate-odd-even-effect *sub* "clarafx-1" "clarafx-2")
(populate-delay-effect *sub* -180)

(write-subtitle-effect *sub* #p"/tmp/output.ass" :supersede)

