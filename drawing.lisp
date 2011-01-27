(in-package :vecto-graphs)

(defstruct (point
             (:constructor point (&optional x y))
             (:conc-name))
  (x 0 :type real)
  (y 0 :type real))

(defun add-x (point x)
  (point (+ x (x point))
         (y point)))

(defun add-y (point y)
  (point (x point)
         (+ y (y point))))

(defun add (point x y)
  (point (+ x (x point))
         (+ y (y point))))

(defun distance (point-1 point-2)
  (sqrt (+ (expt (- (x point-2) (x point-1)) 2)
           (expt (- (y point-2) (y point-1)) 2))))

;;;

(defun string-box (string)
  (vecto:string-bounding-box string (font-size) (font)))

(defun draw-string (point string &key
                    (align-x :left)
                    (align-y :bottom))
  (let* ((bbox (string-box string))
         (x (- (x point)
               (ecase align-x
                 (:left (xmin bbox))
                 (:right (xmax bbox))
                 (:center (+ (/ (- (xmax bbox) (xmin bbox)) 2.0)
                             (xmin bbox))))))
         (y (- (y point)
               (ecase align-y
                 (:top (ymax bbox))
                 (:bottom (ymin bbox))
                 (:center (+ (/ (- (ymax bbox) (ymin bbox)) 2.0)
                             (ymin bbox)))))))
    (vecto:draw-string x y string)
    bbox))

(defun draw-centered-string (point string)
  (draw-aligned-string point string
                       :align-x :center :align-y :center))

(defun draw-line (from to)
  (draw-line* (x from) (y from)
              (x to) (y to)))

(defun draw-line* (from-x from-y to-x to-y)
  (vecto:move-to from-x from-y)
  (vecto:line-to to-x to-y)
  (vecto:stroke))

(defun draw-rectangle (origin width height)
  (vecto:rectangle (x origin) (y origin) width height)
  (vecto:fill-path))

(defun translate (point)
  (vecto:translate (x point) (y point)))

(defun arc (center radius start-angle end-angle)
  (vecto:arc (x center) (y center)
             radius start-angle end-angle)
  (vecto:fill-and-stroke))
