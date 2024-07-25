(cl:in-package :clarafx.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual alignment
;;;
(defun calculate-virtual-alignment-1 (alignment)
  "Calculate coordinates every syllables with origin equal to {\\an1}"
  (let* ((an (alignment-code (alignment-code alignment)))
         (canvas (canvas alignment))
         (lines (line-syllables alignment))
         (fontspace (fontspace canvas)))
    (loop with below-line = (1- (length lines))
          for syllables in lines
          for total-advance = (if (digit-char-p an 10)
                                  (loop for syllable in syllables
                                        sum (width syllable))
                                  (loop for syllable in syllables
                                        sum (if (string-invisiblep (plain-text syllable))
                                                0
                                                (+ (height syllable) fontspace))))
          do (ecase an
               (#\1
                (let ((xmin (margin-left canvas))
                      (ymax (- (height canvas) (margin-bottom canvas)))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (- ymax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      (setf (point-y2 syllable) (- ymax (* (- below-line (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\2
                (let ((xmin (+ (truncate (- (margin-left canvas) (margin-right canvas)) 2) (- (truncate (width canvas) 2) (truncate total-advance 2))))
                      (ymax (- (height canvas) (margin-bottom canvas)))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (- ymax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      (setf (point-y2 syllable) (- ymax (* (- below-line (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\3
                (let ((xmin (- (width canvas) (margin-right canvas) total-advance))
                      (ymax (- (height canvas) (margin-bottom canvas)))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (- ymax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      (setf (point-y2 syllable) (- ymax (* (- below-line (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\4
                (let ((xmin (margin-left canvas))
                      (ymax (truncate (height canvas) 2))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      (setf (point-y2 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\5
                (let ((xmin (+ (truncate (- (margin-left canvas) (margin-right canvas)) 2) (- (truncate (width canvas) 2) (truncate total-advance 2))))
                      (ymax (truncate (height canvas) 2))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      (setf (point-y2 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\6
                (let ((xmin (- (width canvas) (margin-right canvas) total-advance))
                      (ymax (truncate (height canvas) 2))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      (setf (point-y2 syllable) (+ (- ymax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\7
                (let ((xmin (margin-left canvas))
                      (ymax (margin-top canvas))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (+ ymax (* (line syllable) (height syllable))))
                      (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\8
                (let ((xmin (+ (truncate (- (margin-left canvas) (margin-right canvas)) 2) (- (truncate (width canvas) 2) (truncate total-advance 2))))
                      (ymax (margin-top canvas))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (+ ymax (* (line syllable) (height syllable))))
                      (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\9
                (let ((xmin (- (width canvas) (margin-right canvas) total-advance))
                      (ymax (margin-top canvas))
                      (xcnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-x1 syllable) (+ xmin xcnt))
                      (setf (point-x2 syllable) (+ (width syllable) xmin xcnt))
                      (setf (point-y1 syllable) (+ ymax (* (line syllable) (height syllable))))
                      (setf (point-y2 syllable) (+ ymax (* (1+ (line syllable)) (height syllable))))
                      ;; xcnt
                      (setf xcnt (+ xcnt (width syllable)))))))
               (#\A
                (let ((ymin (margin-top canvas))
                      (xmax (margin-left canvas))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (+ xmax (* (line syllable) (height syllable))))
                      (setf (point-x2 syllable) (+ xmax (* (1+ (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\B
                (let ((ymin (margin-top canvas))
                      (xmax (truncate (width canvas) 2))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (+ (- xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      (setf (point-x2 syllable) (+ (- xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\C
                (let ((ymin (margin-top canvas))
                      (xmax (- (width canvas) (margin-right canvas)))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (- xmax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (- xmax (* (- below-line (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\D
                (let ((ymin (+ (truncate (- (margin-top canvas) (margin-bottom canvas)) 2) (- (truncate (height canvas) 2) (truncate total-advance 2))))
                      (xmax (margin-left canvas))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (+ xmax (* (line syllable) (height syllable))))
                      (setf (point-x2 syllable) (+ xmax (* (1+ (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\E
                (let ((ymin (+ (truncate (- (margin-top canvas) (margin-bottom canvas)) 2) (- (truncate (height canvas) 2) (truncate total-advance 2))))
                      (xmax (truncate (width canvas) 2))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (+ (- xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      (setf (point-x2 syllable) (+ (- xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\F
                (let ((ymin (+ (truncate (- (margin-top canvas) (margin-bottom canvas)) 2) (- (truncate (height canvas) 2) (truncate total-advance 2))))
                      (xmax (- (width canvas) (margin-right canvas)))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (- xmax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (- xmax (* (- below-line (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\G
                (let ((ymin (margin-top canvas))
                      (xmax (margin-left canvas))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (+ xmax (* (- below-line (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (+ xmax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\H
                (let ((ymin (margin-top canvas))
                      (xmax (truncate (width canvas) 2))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (- (+ xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (- (+ xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\I
                (let ((ymin (margin-top canvas))
                      (xmax (- (width canvas) (margin-right canvas)))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (- xmax (* (1+ (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (- xmax (* (line syllable) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\J
                (let ((ymin (+ (truncate (- (margin-top canvas) (margin-bottom canvas)) 2) (- (truncate (height canvas) 2) (truncate total-advance 2))))
                      (xmax (margin-left canvas))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (+ xmax (* (- below-line (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (+ xmax (* (- (1+ below-line) (line syllable)) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\K
                (let ((ymin (+ (truncate (- (margin-top canvas) (margin-bottom canvas)) 2) (- (truncate (height canvas) 2) (truncate total-advance 2))))
                      (xmax (truncate (width canvas) 2))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (- (+ xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (1+ (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (- (+ xmax  (truncate (* (1+ below-line) (height syllable)) 2)) (* (line syllable) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))
               (#\L
                (let ((ymin (+ (truncate (- (margin-top canvas) (margin-bottom canvas)) 2) (- (truncate (height canvas) 2) (truncate total-advance 2))))
                      (xmax (- (width canvas) (margin-right canvas)))
                      (ycnt 0))
                  (dolist (syllable syllables)
                    (unless (invisiblep syllable)
                      (setf (point-y1 syllable) (+ ymin ycnt))
                      (setf (point-y2 syllable) (+ (height syllable) ymin ycnt))
                      (setf (point-x1 syllable) (- xmax (* (1+ (line syllable)) (height syllable))))
                      (setf (point-x2 syllable) (- xmax (* (line syllable) (height syllable))))
                      ;; ycnt
                      (setf ycnt (+ ycnt (height syllable) fontspace))))))))))

