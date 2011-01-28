(in-package :vecto-graphs)

(defun draw-curve (points)
  (move-to (car points))
  (mapcar #'line-to (cdr points))
  (vecto:stroke))

(defun max-y (curves)
  (loop for (name . curve) in curves
        maximize (max-value curve)))

(defun scale-points (curve &key width height max-y)
  (let ((x-step (floor width (1+ (length curve)))))
    (loop for (x y) in curve
          for i from x-step by x-step
          collect (point i (/ (* y height) max-y)))))

(defun x-labels (curves)
  (let ((first-labels (mapcar #'car (cdar curves))))
    (loop for (name . curve) in (cdr curves)
          for labels = (mapcar #'car curve)
          do (assert (equalp first-labels labels)
                     nil "Labels of all curves must be equal"))
    first-labels))

(defun draw-line-chart (curves x-label y-label)
  (let ((max-y (max-y curves)))
    (multiple-value-bind (origin x-length y-length)
        (draw-number-axes (x-labels curves)
                          max-y
                          10
                          x-label y-label)
      (translate origin)
      (loop for (name . curve) in curves
            for color in *colors*
            do
            (apply #'vecto:set-rgb-stroke color)
            (draw-curve (scale-points curve
                                      :width x-length :height y-length
                                      :max-y max-y))))))

(defun line-chart (file curves &key x-label y-label)
  "Curves is ((name (x-value y-value)*)*)"
  (with-graph (file)
    (vecto:set-line-width 4)
    (vecto:set-line-join :round)
    (draw-line-chart curves x-label y-label)))
