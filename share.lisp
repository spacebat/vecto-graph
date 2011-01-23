(in-package :vecto-graphs)

(defvar *colors*
  '((0.2 0.4 1.0)
    (1.0 0.2 0.8)
    (0.2 0.8 1.0)
    (0.0 0.23921569 0.9607843)
    (0.0 0.18039216 0.72156864)
    (1.0 0.2 0.4)
    (0.2 1.0 0.8)
    (0.72156864 0.5411765 0.0)
    (0.9607843 0.72156864 0.0)
    (1.0 0.4 0.2)
    (0.2 1.0 0.4)
    (0.4 1.0 0.2)
    (0.8 1.0 0.2)
    (1.0 0.8 0.2)))

(defvar *font* (zpb-ttf:open-font-loader
                (merge-pathnames "DejaVuSans.ttf"
                                 #.(or *compile-file-truename*
                                       *load-truename*))))
(defvar *font-size* 15)
(defvar *width* 500)
(defvar *height* 500)
(defvar *margins* 10)

(defun total-count (alist)
  (reduce #'+ alist :key #'second))

(defun max-value (alist)
  (reduce #'max alist :key #'second))
