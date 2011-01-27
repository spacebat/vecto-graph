(in-package :vecto-graphs)

(defvar *bar-width* 20)

(defun draw-bars (points)
  (loop for point in points
        for color in *colors*
        do
        (apply #'vecto:set-rgb-stroke color)
        (apply #'vecto:set-rgb-fill color)
        (draw-rectangle (point (- (x point)
                                  (/ *bar-width* 2))
                               1)
                        *bar-width*
                        (y point))))

(defun draw-bar-graph (alist x-label y-label)
  (let ((max-value (max-value alist)))
    (multiple-value-bind (origin x-length y-length)
        (draw-number-axes (mapcar #'car alist)
                          max-value
                          10
                          x-label y-label)
      (translate origin)
      (draw-bars (scale-points alist
                                :width x-length :height y-length
                                :max-y max-value)))))

(defun bar-graph (file alist x-label y-label)
  "Alist (name value)"
  (with-graph (file)
    (draw-bar-graph alist x-label y-label)))
