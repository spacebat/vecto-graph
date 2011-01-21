(in-package :vecto-graphs)

(defun draw-curve (points)
  (apply #'move-to (car points))
  (loop for (x y) in (cdr points)
        do (line-to x y))
  (stroke))

(defun max-points (curves)
  (loop for curve in curves
        maximize (reduce #'max curve :key #'first) into max-x
        maximize (reduce #'max curve :key #'first) into max-y
        finally (return (values max-x max-y))))

(defun scale-points (points &key width height max-x max-y)
  (let ((sorted (copy-alist points)))
    (loop for (x y) in sorted
          collect (list (/ (* x width) max-x)
                        (/ (* y height) max-y)))))

(defun draw-plot (curves)
  (multiple-value-bind (max-x max-y) (max-points curves)
    (loop for curve in curves
          for color in *colors*
          do
          (apply #'set-rgb-stroke color)
          (draw-curve (scale-points curve
                                    :width 400 :height 400
                                    :max-x max-x :max-y max-y)))))

(defun plot (file curves)
  "Curves is a list of alists with (x-value y-value),
each alist represents a different curve."
  (with-canvas (:width 500 :height 500)
    (set-font *font* *font-size*)
    (set-line-width 2)
    (draw-plot curves)
    (save-png file)))
