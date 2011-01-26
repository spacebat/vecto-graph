(in-package :vecto-graphs)

(defvar *bar-width* 20)

(defun print-bars (alist right-margin)
  (let* ((max-value (max-value alist)))
    (set-rgb-fill 1.0 0.65 0.3)
    (loop for row = 10 then (+ row *bar-width* 10)
          for (nil value) in alist
          for bar-length = (/ (* 400 value) max-value)
          do
          (rectangle row
                     (+ right-margin 10)
                     *bar-width*
                     bar-length)
          (fill-path))))

(defun print-labels (alist)
  (loop for row = 10 then (+ row *bar-width* 10)
        for (text) in alist
        do 
        (draw-string row 0 text)
        maximize (elt (string-bounding-box text *font-size* *font*)
                      2)))

(defun bar-graph (file alist)
  "Alist (name value)"
  (with-graph (file)
    (print-bars alist (print-labels alist))))
