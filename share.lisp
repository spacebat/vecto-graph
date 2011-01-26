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
(defvar *axis-font-size* 10)
(defvar *width* 500)
(defvar *height* 500)
(defvar *margins* 5)

(defvar *current-font* nil)
(defvar *current-font-size* nil)

(defun font ()
  (values *current-font*
          *current-font-size*))

(defun font-size ()
  *current-font-size*)

(defun (setf font) (font &optional (size *current-font-size*))
  (set-font font size)
  (setf *current-font* font
        *current-font-size* size))

(defun (setf font-size) (size)
  (setf *current-font-size* size)
  (set-font *current-font* size))

(defun total-count (alist)
  (reduce #'+ alist :key #'second))

(defun max-value (alist)
  (reduce #'max alist :key #'second))

(defmacro with-graph ((file &key width height) &body body)
  `(let ((*width* (or ,width *width*))
         (*height* (or ,height *height*)))
     (with-canvas (:width *width* :height *height*)
       (setf (font *font-size*) *font*)
       ,@body
       (save-png ,file))))

(defun draw-line (from-x from-y to-x to-y)
  (move-to from-x from-y)
  (line-to to-x to-y))

(defun string-box (string)
  (string-bounding-box string (font-size) (font)))

(defun draw-aligned-string (x y string &key
                            (align-x :left)
                            (align-y :bottom))
  (let* ((bbox (string-box string))
         (x (- x
               (ecase align-x
                 (:left (xmin bbox))
                 (:right (xmax bbox))
                 (:center (+ (/ (- (xmax bbox) (xmin bbox)) 2.0)
                             (xmin bbox))))))
         (y (- y
               (ecase align-y
                 (:top (ymax bbox))
                 (:bottom (ymin bbox))
                 (:center (+ (/ (- (ymax bbox) (ymin bbox)) 2.0)
                             (ymin bbox)))))))
    (draw-string x y string)))

(defun draw-truly-centered-string (x y string)
  (draw-aligned-string x y string
                       :align-x :center :align-y :center))
