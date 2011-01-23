(in-package :vecto-graphs)

(defvar *axis-font-size* 10)

(defun string-box (string)
  (string-bounding-box string *axis-font-size* *font*))

(defun max-label-length (step max orientation)
  (loop for i from step by step below max
        for box = (string-box (princ-to-string i))
        maximize (ecase orientation
                   (:x
                    (- (elt box 2)
                       (elt box 0)))
                   (:y
                    (- (elt box 3)
                       (elt box 1))))))

(defun draw-truly-centered-string (x y string)
  (let* ((bbox (string-box string))
         (width/2 (/ (- (elt bbox 2) (elt bbox 0)) 2.0))
         (height/2 (/ (- (elt bbox 3) (elt bbox 1)) 2.0)))
    (draw-string (- x width/2) (- y height/2) string)))

(defun draw-axis-label (axis-length y-margin x-margin max step orientation)
  (loop with y-start = (- y-margin 2)
        with y-end = (+ y-margin 2)
        with label-center = (+ (floor (- y-margin *margins*) 2)
                               *margins*
                               -2)
        with scaled-step = (floor (* axis-length step) max)
        for scaled-x from (+ x-margin scaled-step) by scaled-step to axis-length
        for x from step by step
        for label = (princ-to-string x)
        do
        (ecase orientation
          (:x
           (move-to scaled-x y-start)
           (line-to scaled-x y-end)
           (draw-truly-centered-string scaled-x label-center label))
          (:y
           (move-to y-start scaled-x)
           (line-to y-end scaled-x)
           (draw-truly-centered-string label-center scaled-x label)))))

(defun draw-labels (x-label y-label
                    x-axis-length y-axis-length
                    x-label-height y-label-height
                    x-margin y-margin)
  (set-font *font* *font-size*)
  (draw-truly-centered-string (+ x-margin (floor x-axis-length 2))
                              (+ *margins* (floor x-label-height 2))
                              x-label)
  (rotate-degrees 90)
  (draw-truly-centered-string (+ y-margin (floor y-axis-length 2))
                              (- (+ *margins* (floor y-label-height 2)))
                              y-label)
  (rotate-degrees -90))


(defun calculate-margins (x-step y-step max-x max-y x-label y-label)
  (let* ((x-box (string-bounding-box x-label *font-size* *font*))
         (x-height (- (elt x-box 3) (elt x-box 1)))
         (y-box (string-bounding-box y-label *font-size* *font*))
         (y-height (- (elt y-box 2) (elt y-box 0)))
         (x-margin (- (+ *margins*
                         3
                         (ceiling x-height)
                         3
                         (ceiling (max-label-length x-step max-x :y))
                         3)
                      .5))
         (y-margin (- (+ *margins*
                         3
                         (ceiling y-height)
                         3
                         (ceiling (max-label-length y-step max-y :y))
                         3)
                      .5)))
    (values x-margin y-margin
            x-height y-height)))

(defun draw-axes (max-x max-y divisions x-label y-label)
  (let ((x-step (floor max-x divisions))
        (y-step (floor max-y divisions)))
    (multiple-value-bind (x-margin y-margin
                                   x-label-height y-label-height)
        (calculate-margins x-step y-step max-x max-y x-label y-label)
      (let ((x-axis-length (- *height* x-margin *margins*))
            (y-axis-length (- *width* y-margin *margins*)))
        (draw-labels x-label y-label
                     x-axis-length y-axis-length
                     x-label-height y-label-height
                     x-margin y-margin)
        (set-font *font* *axis-font-size*)
        (move-to x-margin y-margin)
        (line-to x-margin (- *height* *margins*))
        (move-to x-margin y-margin)
        (line-to (- *height* *margins*) y-margin)
        (set-rgb-stroke 0 0 0)
        (set-line-width 1)
        (draw-axis-label x-axis-length y-margin x-margin max-x x-step
                         :x)
        (draw-axis-label y-axis-length x-margin y-margin max-y y-step
                         :y)
        (stroke)
        (translate x-margin y-margin)
        (values x-axis-length
                y-axis-length)))))
