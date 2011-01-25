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
  (set-font *current-font-size* size))

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
