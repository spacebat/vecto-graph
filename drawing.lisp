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

(defun quadrant (angle)
  (ceiling angle (/ pi 2)))

(defun rotate-alignment (align-x align-y rotate)
  (let ((flip-x '(:left :right :right :left :center :center))
        (flip-y '(:top :bottom :bottom :top :center :center)))
    (values (case rotate
              (:clockwise (getf flip-x align-x))
              (:anti-clockwise align-x)
              (t align-x))
            (case rotate
              (:clockwise align-y)
              (:anti-clockwise (getf flip-y align-y))
              (t align-y))
            (case rotate
              (:clockwise (- (/ pi 2)))
              (:anti-clockwise (/ pi 2))
              (t rotate)))))

(defun rotate-point (point angle)
  (let ((cos (cos angle))
        (sin (sin angle))
        (x (x point))
        (y (y point)))
    (point (+ (* x sin) (* y cos))
           (- (* x cos) (* y sin)))))

(defun draw-string (point string &key
                    (align-x :left)
                    (align-y :bottom)
                    rotate)
  (multiple-value-bind (align-x align-y angle)
      (rotate-alignment align-x align-y rotate)
    (when angle
      (setf point (rotate-point point angle))
      (vecto:rotate angle))
    (let* ((bbox (string-box string))
           (origin (add point
                        (- (ecase align-x
                             (:left (xmin bbox))
                             (:right (xmax bbox))
                             (:center (+ (/ (- (xmax bbox) (xmin bbox)) 2.0)
                                         (xmin bbox)))))
                        (- (ecase align-y
                             (:top (ymax bbox))
                             (:bottom (ymin bbox))
                             (:center (+ (/ (- (ymax bbox) (ymin bbox)) 2.0)
                                         (ymin bbox))))))))
     
      (vecto:draw-string (x origin)
                         (y origin) string)
      (when angle
        (vecto:rotate (- angle)))
      bbox)))

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
  (vecto:fill-and-stroke))

(defun translate (point)
  (vecto:translate (x point) (y point)))

(defun arc (center radius start-angle end-angle)
  (vecto:arc (x center) (y center)
             radius start-angle end-angle)
  (vecto:fill-and-stroke))

(defun move-to (point)
  (vecto:move-to (x point) (y point)))

(defun line-to (point)
  (vecto:line-to (x point) (y point)))
