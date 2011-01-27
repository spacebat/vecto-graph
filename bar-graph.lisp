(in-package :vecto-graphs)

(defvar *bar-width* 20)

(defun print-bars (alist right-margin)
  (let* ((max-value (max-value alist)))
    (vecto:set-rgb-fill 1.0 0.65 0.3)
    (loop for row = 10 then (+ row *bar-width* 10)
          for (nil value) in alist
          for bar-length = (/ (* 400 value) max-value)
          do
          (draw-rectangle (point row
                                 (+ right-margin 10))
                          *bar-width*
                          bar-length))))

(defun print-labels (alist)
  (loop for row = 10 then (+ row *bar-width* 10)
        for (text) in alist
        maximize (xmax (draw-string (point row 0) text))))

(defun bar-graph (file alist)
  "Alist (name value)"
  (with-graph (file)
    (print-bars alist (print-labels alist))))
