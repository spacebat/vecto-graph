(in-package :vecto-graphs)

(defun quadrant (angle)
  (ceiling angle (/ pi 2)))

(defun draw-sector-value (value
                           center-x center-y radius
                           start-angle angle)
  (set-rgb-fill 0 0 0)
  (setf (font-size) *axis-font-size*)
  (let ((half-angle (+ start-angle (/ angle 2.0))))
    (draw-truly-centered-string (+ center-x (* (cos half-angle) (/ radius 2)))
                                (+ center-y (* (sin half-angle) (/ radius 2)))
                                value)))

(defun draw-sector-name (name
                         center-x center-y radius
                         start-angle angle)
  (set-rgb-fill 0 0 0)
  (setf (font-size) *font-size*)
  (let* ((half-angle (print (+ start-angle (/ angle 2.0))))
         (quadrant (quadrant half-angle))
         (align-x (ecase quadrant
                    ((1 4) :left)
                    ((2 3) :right)))
         (align-y (ecase quadrant
                    ((2 1) :bottom)
                    ((3 4) :top))))
    (draw-string-relative-to-bounds (+ center-x (* (cos half-angle) (* radius 1.2)))
                                    (+ center-y (* (sin half-angle) (* radius 1.2)))
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
          (draw-sector-value (princ-to-string value)
                             center-x center-y radius
                             start-angle (- end-angle start-angle))
          (draw-sector-name name
                            center-x center-y radius
                            start-angle (- end-angle start-angle)))))

(defun pie-chart (file alist)
  "Alist (name value)"
  (with-graph (file)
    (print-pie alist 250 250 80)))
