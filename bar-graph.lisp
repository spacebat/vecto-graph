(in-package :vecto-graphs)

(defvar *bar-width* 20)

(defun row-length (value width total)
  (truncate (* width value) total))

(defun percent (value total)
  (/ (* 100 value) total))

(defun print-bars (alist right-margin &key key)
  (let* ((max-value (max-value alist))
         (max (if key (funcall key max-value) key))
         (width 400)) ;; kludge
    (set-rgb-fill 1.0 0.65 0.3)
    (loop for row = 10 then (+ row *bar-width* 10)
          for (nil value) in alist
          for bar-length = (row-length width value max)
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
