(in-package :vecto-graphs)

(defun draw-curve (points)
  ;; (apply #'move-to (car points))
  ;; (loop for (x y) in (cdr points)
  ;;       do (line-to x y))
  ;; (stroke)
  )

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
    (multiple-value-bind (origin x-length y-length)
        (draw-number-axes max-x max-y 10
                          x-label y-label)
      (translate origin)
      (loop for (name . curve) in curves
            for color in *colors*
            do
            (apply #'vecto:set-rgb-stroke color)
            (draw-curve (scale-points curve
                                      :width x-length :height y-length
                                      :max-x max-x :max-y max-y))))))

(defun line-chart (file curves &key x-label y-label)
  "Curves is ((name (x-value y-value)*)*)"
  (with-graph (file)
    (vecto:set-line-width 4)
    (vecto:set-line-join :round)
    (draw-line-chart curves x-label y-label)))

(defun move-to (point)
  (vecto:move-to (x point) (y point)))

(defun line-to (point)
  (vecto:line-to (x point) (y point)))
