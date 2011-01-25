(in-package :vecto-graphs)

(defvar *axis-font-size* 10)

(defun string-box (string)
  (string-bounding-box string (font-size) (font)))

(defun max-label-length (step max orientation)
  (loop for i from step by step below max
        for box = (string-box (princ-to-string i))
        maximize (ecase orientation
                   (:x
                    (- (xmax box)
                       (xmin box)))
                   (:y
                    (- (ymax box)
                       (ymin box))))))

(defun draw-aligned-string (x y string &key
                            (align-x :left)
                            (align-y :bottom))
  (let* ((bbox (string-box string))
         (x (- x
               (ecase align-x
                 (:left (xmin bbox))
                 (:right (xmax bbox))
                 (:center (+ (/ (- (xmax bbox) (xmin bbox)) 2.0)
                             (xmin bbox))))))
         (y (- y
               (ecase align-y
                 (:top (ymax bbox))
                 (:bottom (ymin bbox))
                 (:center (+ (/ (- (ymax bbox) (ymin bbox)) 2.0)
                             (ymin bbox)))))))
    (draw-string x y string)))

(defun draw-truly-centered-string (x y string)
  (draw-aligned-string x y string
                       :align-x :center :align-y :center))

(defun draw-axis-label (axis-length
                        y-margin x-margin
                        max step max-label-length
                        orientation)
  (loop with y-start = (- y-margin 2)
        with y-end = (+ y-margin 2)
        with label-center = (- y-margin (ceiling max-label-length 2)
                               6)
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
  (setf (font-size) *font-size*)
  (draw-truly-centered-string (+ x-margin (/ x-axis-length 2))
                              (+ *margins* (/ x-label-height 2))
                              x-label)
  (rotate-degrees 90)
  (draw-truly-centered-string (+ y-margin (/ y-axis-length 2))
                              (- (+ *margins* (/ y-label-height 2)))
                              y-label)
  (rotate-degrees -90))

(defun calculate-margins (x-step y-step max-x max-y x-label y-label)
  (let* ((x-box (string-bounding-box x-label *font-size* *font*))
         (x-height (- (ymax x-box) (ymin x-box)))
         (y-box (string-bounding-box y-label *font-size* *font*))
         (y-height (- (ymax y-box) (ymin y-box)))
         (max-x-label-length (ceiling (max-label-length x-step max-x :x)))
         (max-y-label-length (ceiling (max-label-length y-step max-y :y)))
         (x-margin (- (ceiling
                       (+ *margins*
                          3
                          y-height
                          3
                          max-y-label-length
                          3))
                      .5))
         (y-margin (- (ceiling
                       (+ *margins*
                          3
                          x-height
                          3
                          max-x-label-length
                          3))
                      .5)))
    ;; (draw-line 0 x-height
    ;;            *width* x-height)
    ;; (draw-line y-height 0
    ;;            y-height *height*)
    ;; (draw-borders)
    (values x-margin y-margin
            x-height y-height
            max-x-label-length
            max-y-label-length)))

(defun draw-axes (max-x max-y divisions x-label y-label)
  (let ((x-step (floor max-x divisions))
        (y-step (floor max-y divisions)))
    (multiple-value-bind (x-margin y-margin
                          x-label-height y-label-height
                          max-x-label-length
                          max-y-label-length)
        (calculate-margins x-step y-step max-x max-y x-label y-label)
      (let ((x-axis-length (- *height* x-margin *margins*))
            (y-axis-length (- *width* y-margin *margins*)))
        (draw-labels x-label y-label
                     x-axis-length y-axis-length
                     x-label-height y-label-height
                     x-margin y-margin)
        (setf (font-size) *axis-font-size*)
        (move-to x-margin y-margin)
        (line-to x-margin (- *height* *margins*))
        (move-to x-margin y-margin)
        (line-to (- *height* *margins*) y-margin)
        (set-rgb-stroke 0 0 0)
        (set-line-width 1)
        (draw-axis-label x-axis-length y-margin x-margin max-x x-step
                         max-x-label-length
                         :x)
        (draw-axis-label y-axis-length x-margin y-margin max-y y-step
                         max-y-label-length
                         :y)
        (stroke)
        (translate x-margin y-margin)
        (values x-axis-length
                y-axis-length)))))
