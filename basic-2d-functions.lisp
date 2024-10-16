;;;; ics481-benber-hw2.lisp
;;;; implement basic shape drawing functions, classes

;;;; define our package (namespace) and dependencies ===========================

(defpackage #:ics481
  (:use #:cl #:glfw))

;;; we enter this package so that all our definitions are within our namespace
(in-package #:ics481)

;;; define helper functions
(defun full-random (n)
  (- (random (1+ (* 2 n))) n))

;;; define class for a single point
(defclass point ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (z :initarg :z :initform 0.0 :accessor z)))

(defun p! (x y &optional (z 0.0))
  (make-instance 'point :x x :y y :z z))

(defmethod p+ ((p1 point) (p2 point))
  (p! (+ (x p1) (x p2))
      (+ (y p1) (y p2))
      (+ (z p1) (z p2))))

(defmethod p+ ((p1 point) (n number))
  (p! (+ (x p1) n)
      (+ (y p1) n)
      (+ (z p1) n)))

(defmethod print-object ((self point) stream)
  (format stream "Point: ~A, ~A, ~A" (x self) (y self) (z self)))

;;; define class for a color value w/ red, green, blue, alpha
(defclass color ()
  ((red :initarg :r :initform 0.0 :accessor r)
   (green :initarg :g :initform 0.0 :accessor g)
   (blue :initarg :b :initform 0.0 :accessor b)
   (alpha :initarg :a :initform 1.0 :accessor a)))

(defun color! (r g b &optional (a 1.0))
  (make-instance 'color :r r :g g :b b :a a))

(defmethod print-object ((self color) stream)
  (format stream "Color: R:~A, G:~A, B:~A, Alpha:~A" (r self) (g self) (b self) (a self)))

;;; define class for a shape
(defclass shape ()
  ((points :initform () :initarg :p :accessor p)
   (fill-color :initform (color! 0 0 0 0) :initarg :fill-color :accessor fill-color)
   (stroke-color :initform (color! 1 0 0) :initarg :stroke-color :accessor stroke-color)
   (stroke-width :initform 1 :initarg :stroke-width :accessor stroke-width)
   (is-closed :initform t :initarg :is-closed :accessor is-closed)))

(defmethod draw ((shape shape))
  (if (eql (is-closed shape) t)
      (progn
	(let ((col (fill-color shape)))
	  (gl:color (r col) (g col) (b col) (a col))
	  (gl:begin :polygon)))
      (gl:begin :line-strip))
  (draw-helper (p shape))
  (when (is-closed shape)
    (let ((col (stroke-color shape)))
      (gl:color (r col) (g col) (b col) (a col)))
      (gl:begin :line-loop)
      (draw-helper (p shape))))

(defun draw-helper (points)
  (dolist (p points)
    (gl:vertex (x p) (y p) (z p)))
  (gl:end))

;;; define functions to make specific shapes/curves
(defun make-shape (points &key is-closed)
  (make-instance 'shape :p points :is-closed is-closed))

(defun make-square (side)
  (let* ((side/2 (* side 0.5))
	 (points (list (p! (- side/2) (- side/2))
		       (p!    side/2  (- side/2))
		       (p!    side/2     side/2)
		       (p! (- side/2) side/2))))
    (make-shape points :is-closed t)))

(defun make-rectangle (width height)
  (let* ((width/2 (* width 0.5))
	 (height/2 (* height 0.5))
	 (points (list (p! (- width/2) (- height/2))
		       (p!    width/2  (- height/2))
		       (p!    width/2     height/2)
		       (p! (- width/2)    height/2))))
    (make-instance 'shape :p points)))

(defun make-circle (diameter num-points)
  (let* ((radius (* diameter 0.5))
	 (points (list)))
    (dotimes (i num-points)
      (let* ((angle (* i (/ (* 2 PI) num-points))))
	(push (p! (* radius (sin angle)) (* radius (cos angle))) points)))
    (make-shape points :is-closed t)))

(defun make-pentagon (diameter)
  (make-circle diameter 5))

(defun make-hexagon (diameter)
  (make-circle diameter 6))

(defun make-heptagon (diameter)
  (make-circle diameter 7))

(defun make-octagon (diameter)
  (make-circle diameter 8))


;;; make-circle extension, with varying radius and configurable angle stepping/change via num-loops
(defun make-spiral (diameter num-loops num-points)
  (let* ((radius (* diameter 0.5))
	 (points (list)))
    (dotimes (i num-points)
      (let* ((angle (* num-loops (* i (/ (* 2 PI) num-points))))
	     (current-radius (* i (/ radius num-points))))
	(push (p! (* current-radius (sin angle)) (* current-radius (cos angle))) points)))
    (make-shape points :is-closed nil)))

;;; TODO: Add domain, magnitude, phase, frequency parameters
(defun make-sine-curve (num-points)
  (let ((points (list)))
    (dotimes (i num-points)
      (let ((angle (* i (/ (* 2 PI) num-points))))
	(push (p! (* i (/ 2 num-points)) (sin angle)) points)))
    (make-shape points :is-closed nil)))

;;; define transform functions
;;; transform order: scale -> rotate -> move
;;; POSSIBLE TOPIC TO RESEARCH: implementing pivot for shape
(defmethod move-shape-by ((shape shape) (offset point))
  (let ((points ()))
    (dolist (pt (p shape))
      (push (p+ pt offset) points))
    (setf (p shape) points)))

(defmethod rotate-shape-by ((shape shape) (angle number)) ; essentially implements rotation matrix mult
  (let ((points ()))
    (dolist (pt (p shape))
      (let ((pt-x (x pt))
	    (pt-y (y pt)))
	(push (p! (+ (* pt-x (cos angle)) (* pt-y (- (sin angle)))) (+ (* pt-x (sin angle)) (* pt-y (cos angle)))) points)))
    (setf (p shape) points)))

(defmethod scale-shape-by ((shape shape) (scale point))
  (let ((points()))
    (dolist (pt (p shape))
      (push (p! (* (x pt) (x scale)) (* (y pt) (y scale))) points))
    (setf (p shape) points)))

(defmethod randomize-shape-points ((shape shape) (delta number))
  (let ((points()))
    (dolist (pt (p shape))
      (push (p+ pt (full-random delta)) points))
    (setf (p shape) points)))

;;; define scene, scene methods
(defclass scene ()
  ((bg-color :accessor bg-color :initarg :bg-color :initform (color! 0 0 0))
   (shapes :accessor shapes :initarg :shapes :initform ())))

(defmethod add-shape ((scene scene) (shape shape))
  (push shape (shapes scene)))

(defmethod render ((scene scene))
  (let ((col (bg-color scene)))
    (gl:clear-color (r col) (g col) (b col) 0)
    (gl:clear :color-buffer)
    (dolist (shape (shapes scene))
      (let ((col (stroke-color shape)))
	(gl:color (r col) (g col) (b col) (a col)))
      (gl:line-width (stroke-width shape))
      (draw shape))))

;;;; ---------------------------------------------

(defparameter *scene* nil)

(defun setup-scene ()
  (setf *scene* (make-instance 'scene))
  (add-shape *scene* (make-square 1.0))
  (add-shape *scene* (make-rectangle 1.0 1.5))
  (add-shape *scene* (make-pentagon 1.5))
  (add-shape *scene* (make-hexagon 2.0))
  (add-shape *scene* (make-heptagon 2.5))
  (add-shape *scene* (make-octagon 3.0)))

;;; (setup-scene)

(defun setup-scene-2 ()
  (setf *scene* (make-instance 'scene))
  (let ((square (make-square 1.0)))
    (rotate-shape-by square (/ PI 4))
    (move-shape-by square (p! 0.0 -1.0))
    (randomize-shape-points square 0.2)
    (setf (fill-color square) (color! 0 1 0 1))
    (add-shape *scene* square))
  (add-shape *scene* (make-circle 2.0 100)))

;;; (setup-scene-2)

(defun setup-scene-hw1 ()
  (setf *scene* (make-instance 'scene))
  (dotimes (n 50)
    (let* ((col (color! 1.0 (/ n 50) 0.0))
	   (dia (* 4 (/ n 50)))
	   (angle (* (* 2 PI) (/ n 50)))
	   (triangle (make-pentagon dia)))
      (setf (fill-color triangle) col)
      (rotate-shape-by triangle angle)
      (add-shape *scene* triangle))))

(setup-scene-hw1)
   
;;; set up an OpenGL viewport for drawing into
(defun setup-projection ()
  (gl:matrix-mode :projection)		;set up projection mode
  (gl:load-identity)			;apply identity transform
  (gl:ortho -2 2 -2 2 -1 1))		;setup viewport with coordinates from (-2, -2) to (2, 2)


;;; this gets called when the window is interactively resized
(def-window-size-callback update-viewport (window w h)
  (declare (ignore window w h))
  (setup-projection))

;;; this gets called when a keyboard event occurs, closing the window when ESC key pressed
(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

;;; this creates a window and calls the render function in a loop until we exit
(defun start-window ()
  (with-init-window (:title "ICS481" :width 512 :height 512)
    (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
    (set-key-callback 'quit-on-escape)
    (set-window-size-callback 'update-viewport)
    (setup-projection)
    (loop until (window-should-close-p)
	  do (render *scene*)
	  do (swap-buffers)
	  do (poll-events))))

;;; this executes code on main thread -- necessary for interacting with UI elements in MacOS
(defun run ()
  (sb-int:set-floating-point-modes :traps nil)
  (start-window))
  
(run)
