(in-package :vecto-graphs)

(defun draw-axis-label (origin
                        axis-length
                        labels
                        orientation
                        step)
  (let ((axis-start (ecase orientation
                      (:x (x origin))
                      (:y (y origin)))))
    (loop with step = (if step
                          (* axis-length step)
                          (floor axis-length (length labels)))
          for x from (+ axis-start step) by step to axis-length
          for label in labels
          do
          (ecase orientation
            (:x
             (draw-line* x (- (y origin) 2)
                         x (+ (y origin) 2))
             (draw-string (point x (- (y origin) 6))
                          label
                          :align-x :center
                          :align-y :top))
            (:y
             (draw-line* (- (x origin) 2) x
                         (+ (x origin) 2) x)
             (draw-string (point (- (x origin) 6) x)
                          label
                          :align-x :right
                          :align-y :center))))))

(defun draw-labels (x-label y-label
                    origin x-axis-length y-axis-length)
  (setf (font-size) *font-size*)
  (draw-string (point (+ (x origin) (/ x-axis-length 2))
                      *margins*)
               x-label
               :align-x :center)
  (draw-string (point (+ (y origin) (/ y-axis-length 2))
                      *margins*)
               y-label
               :rotate :anti-clockwise
               :align-x :center))

(defun calculate-margins (xs ys x-label y-label)
  (let* ((x-box (string-box x-label))
         (x-height (- (ymax x-box) (ymin x-box)))
         (y-box (string-box y-label))
         (y-height (- (ymax y-box) (ymin y-box)))
         (max-x-label-length (ceiling (max-label-length xs :y)))
         (max-y-label-length (ceiling (max-label-length ys :x)))
         (x (- (ceiling
                (+ *margins*
                   y-height
                   3
                   max-y-label-length
                   3))
               .5))
         (y (- (ceiling
                (+ *margins*
                   x-height
                   3
                   max-x-label-length
                   3))
               .5)))
    (draw-borders)
    (values (point x y)
            (- *width* x *margins*)
            (- *height* y *margins*))))

(defvar *arrow-length* 6)

(defun draw-arrows (origin x-axis-length y-axis-length)
  (let ((x-axis-y (+ x-axis-length (x origin)))
        (y-axis-x (+ y-axis-length (y origin)))
        (opposite (* (tan (/ pi 5)) *arrow-length*)))
    (draw-line* (x origin)
                x-axis-y
                (+ (x origin) opposite)
                (- x-axis-y *arrow-length*))
    (draw-line* (x origin)
                x-axis-y
                (- (x origin) opposite)
                (- x-axis-y *arrow-length*))
    (draw-line* y-axis-x
               (y origin)
               (- y-axis-x *arrow-length*)
               (+ (y origin) opposite))
    (draw-line* y-axis-x
               (y origin)
               (- y-axis-x *arrow-length*)
               (- (y origin) opposite))))

(defun draw-axes-lines (xs ys x-label y-label)
  (multiple-value-bind (origin
                        x-axis-length
                        y-axis-length)
      (calculate-margins xs ys x-label y-label) 
    (draw-line origin
               (add-x origin x-axis-length))
    (draw-line origin
               (add-y origin y-axis-length))
    (draw-arrows origin x-axis-length y-axis-length)
    (values origin x-axis-length y-axis-length)))

(defun draw-axes  (xs ys x-label y-label
                   &key y-step x-step)
  (multiple-value-bind (origin x-axis-length y-axis-length)
      (draw-axes-lines xs ys x-label y-label)
    (draw-labels x-label y-label
                 origin
                 x-axis-length y-axis-length)
    (setf (font-size) *axis-font-size*)
    (draw-axis-label origin
                     x-axis-length
                     xs
                     :x
                     x-step)
    (draw-axis-label origin y-axis-length
                     xs
                     :y
                     y-step)
    (values origin x-axis-length y-axis-length)))

(defun max-label-length (labels orientation)
  (loop for label in labels
        for box = (string-box (princ-to-string label))
        maximize (ecase orientation
                   (:x
                    (- (xmax box)
                       (xmin box)))
                   (:y
                    (- (ymax box)
                       (ymin box))))))

(defun labels-from-numbers (step max)
  (loop for i from step by step below max
        collect (princ-to-string i)))

(defun draw-number-axes (max-x max-y divisions x-label y-label)
  (let ((x-step (/ max-x divisions))
        (y-step (/ max-y divisions)))
    (draw-axes (labels-from-numbers x-step max-x)
               (labels-from-numbers y-step max-y)
               x-label
               y-label
               ;; :x-step (/ x-step max-x)
               ;; :y-step (print (/ y-step max-y))
               )))

;; (defun draw-label-axes (max-x max-y divisions x-label y-label)
;;   )
