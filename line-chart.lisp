(in-package :vecto-graphs)

(defun draw-curve (points)
  (apply #'move-to (car points))
  (loop for (x y) in (cdr points)
        do (line-to x y))
  (stroke))

(defun max-points (curves)
  (loop for (name . curve) in curves
        maximize (reduce #'max curve :key #'first) into max-x
        maximize (reduce #'max curve :key #'second) into max-y
        finally (return (values max-x max-y))))

(defun scale-points (points &key width height max-x max-y)
  (let ((sorted (copy-alist points)))
    (loop for (x y) in sorted
          collect (list (/ (* x width) max-x)
                        (/ (* y height) max-y)))))

(defun draw-line-chart (curves x-label y-label)
  (multiple-value-bind (max-x max-y) (max-points curves)
    (multiple-value-bind (x-length y-length)
        (draw-axes max-x max-y 10
                   x-label y-label)
      (loop for (name . curve) in curves
            for color in *colors*
            do
            (apply #'set-rgb-stroke color)
            (draw-curve (scale-points curve
                                      :width x-length :height y-length
                                      :max-x max-x :max-y max-y))))))

(defun line-chart (file curves &key x-label y-label)
  "Curves is ((name (x-value y-value)*)*)"
  (with-graph (file)
    (set-line-width 4)
    (set-line-join :round)
    (draw-line-chart curves x-label y-label)))
