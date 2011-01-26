(in-package :vecto-graphs)

(defun quadrant (angle)
  (ceiling angle (/ pi 2)))

(defun draw-sector-labels (name value
                           center-x center-y radius
                           start-angle angle)
  (set-rgb-fill 0 0 0)
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
    (draw-truly-centered-string (+ center-x (* x (/ radius 1.5)))
                                (+ center-y (* y (/ radius 1.5)))
                                value)
    (setf (font-size) *font-size*)
    (draw-line (+ center-x (* x radius))
               (+ center-y (* y radius))
               (+ center-x (* x (+ radius 10)))
               (+ center-y (* y (+ radius 10))))
    (stroke)
    (draw-aligned-string (+ center-x (* x (+ radius 10)))
                         (+ center-y (* y (+ radius 10)))
                         name
                         :align-x align-x
                         :align-y align-y)))

(defun print-pie (alist center-x center-y radius)
  (let ((total (total-count alist))
        (sorted (sort (copy-alist alist) #'>
                      :key #'second)))
    (loop for color in *colors*
          for (name value) in sorted
          for start-angle = 0 then end-angle
          for end-angle = (+ start-angle (/ (* value pi 2) total))
          do
          (move-to center-x center-y)
          (arc center-x center-y radius start-angle end-angle)
          (apply #'set-rgb-fill color)
          (fill-and-stroke)
          (draw-sector-labels name (princ-to-string value)
                              center-x center-y radius
                              start-angle (- end-angle start-angle)))))

(defun pie-chart (file alist)
  "Alist (name value)"
  (with-graph (file)
    (print-pie alist 250 250 80)))
