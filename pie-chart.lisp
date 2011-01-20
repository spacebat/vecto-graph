(in-package :vecto-graphs)

(defun print-pie (alist)
  (let ((total (total-count alist))
        (sorted (sort (copy-alist alist) #'>
                      :key #'second)))
    (loop for color in *colors*
          for (name value) in sorted
          for start-angle = 0 then end-angle
          for end-angle = (+ start-angle (/ (* value pi 2) total))
          do
          (apply #'set-rgb-fill color)
          (move-to 250 250)
          (arc 250 250 80
               (print start-angle)
               (print end-angle))
          (terpri)
          (fill-and-stroke))))

(defun pie-chart (file alist &key (key #'identity))
  "Alist (name value)"
  (with-canvas (:width 500 :height 500)
    (set-font *font* *font-size*)
    (when alist
      (print-pie alist)
      (save-png file))))
