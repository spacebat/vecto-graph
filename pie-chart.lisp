(in-package :vecto-graphs)

(defun draw-sector-labels (name value
                           center radius
                           start-angle angle)
  (vecto:set-rgb-fill .0 .0 .0)
  (let* ((half-angle (+ start-angle (/ angle 2.0)))
         (x (cos half-angle))
         (y (sin half-angle))
         (quadrant (quadrant half-angle))
         (align-x (ecase quadrant
                    ((1 4) :left)
                    ((2 3) :right)))
         (align-y (ecase quadrant
                    ((2 1) :bottom)
                    ((3 4) :top))))
    (setf (font-size) *axis-font-size*)
    (draw-centered-string (add center
                               (* x (/ radius 1.5))
                               (* y (/ radius 1.5)))
                          value)
    (setf (font-size) *font-size*)
    (draw-line (add center (* x radius) (* y radius))
                (add center
                     (* x (+ radius 10)) (* y (+ radius 10))))
    (draw-string (add center
                     (* x (+ radius 10)) (* y (+ radius 10)))
                 name
                 :align-x align-x
                 :align-y align-y)))

(defun print-pie (alist center radius)
  (let ((total (total-count alist))
        (sorted (sort (copy-alist alist) #'>
                      :key #'second)))
    (loop for color in *colors*
          for (name value) in sorted
          for start-angle = 0 then end-angle
          for end-angle = (+ start-angle (/ (* value pi 2) total))
          do
          (move-to center)
          (apply #'vecto:set-rgb-fill color)
          (arc center radius start-angle end-angle)
          (draw-sector-labels name value
                              center radius
                              start-angle (- end-angle start-angle)))))

(defun pie-chart (file alist)
  "Alist (name value)"
  (with-graph (file)
    (print-pie alist (point 250 250) 80)))
