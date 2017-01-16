(defpackage #:opengl-selective-redraw-test
  (:use :cl :alexandria))
(in-package #:opengl-selective-redraw-test)

(defmacro with-gl-in-2d-mode ((w h) &body body)
  (alexandria:once-only (w h)
    `(progn
       (gl:viewport -1 -1 (1+ ,w) (1+ ,h))
       (gl:matrix-mode :projection)
       (gl:with-pushed-matrix
         (gl:load-identity)
         (%gl:ortho 0 ,w 0 ,h -1 1)
         ;;(gl:translate 3/8 3/8 0) ; Redbook Appendix G (exact rasterization)
         (gl:matrix-mode :modelview)
         (gl:with-pushed-matrix
           (gl:load-identity)
           ,@body)))))

(defun setup-gl-for-sprites ()
  (%gl:depth-func :lequal)
  (%gl:alpha-func :greater 0.1)
  (%gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:shade-model :flat)
  (reset-pixel-store))

(defun reset-pixel-store ()
  (mapc #'%gl:pixel-store-i
        '(:unpack-alignment :pack-alignment :unpack-row-length :unpack-skip-pixels)
        '(1 1 0 0)))

(defun draw-cursor (x y)
  (draw-rectangle #xaa1144 x y 16 16))

(defun draw-rectangle (color x y w h)
  (gl:with-primitive :quads
    (gl:color (ldb (byte 8 0) color) (ldb (byte 8 8) color) (ldb (byte 8 16) color))
    (gl:vertex x y)
    (gl:vertex (+ x w) y)
    (gl:vertex (+ x w) (+ y h))
    (gl:vertex x (+ y h))))

(defvar *dirt*)

(defun sweep-dirt (r)
  (destructuring-bind (x y w h) r
    (draw-rectangle #x000000 x y w h)))

(defun main ()
  (sdl:with-init ()
    (handler-bind ((error (lambda (e) (sdl:quit) (invoke-debugger e))))
      (sdl:gl-set-attribute :gl-doublebuffer 1)
      (sdl:gl-set-attribute :gl-swap-control 1)
      (sdl:set-video-mode 640 480 32 '(:opengl :fullscreen))
      (gl:viewport -1 -1 (1+ (sdl:display-width)) (1+ (sdl:display-height)))
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (%gl:ortho 0 (sdl::display-width) 0 (sdl::display-height) -1 1)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (%gl:clear-color 1/5 1/5 1/5 1)
      (setup-gl-for-sprites)

      (gl:clear :color-buffer)
      (sdl:gl-swap-buffers)
      (gl:clear :color-buffer)
      (setf *dirt* (make-array 2 :initial-element nil))
      (sdl:hide-cursor)
      (let ((x 0) (y 0) (buffer-idx 0))
	(cffi:with-foreign-object (event* 'sdl::event)
	  (declare (dynamic-extent event*))
	  (block event-loop
	    (loop
	      (sdl:pump-events)
	      (when-let ((it (aref *dirt* buffer-idx)))
		(loop for r in it
		      do (sweep-dirt r))
		(setf it ())
		(draw-cursor x y)
		(sdl:gl-swap-buffers)
		(setf buffer-idx (mod (1+ buffer-idx) 2)))
	      (multiple-value-bind (buttons mx my) (sdl:get-mouse-state)
		(declare (ignore buttons))
		(when (or (/= x mx) (/= y (- 480 my)))
		  (push (list x y 16 16) (aref *dirt* buffer-idx))
		  (push (list x y 16 16) (aref *dirt* (mod (1+ buffer-idx) 2)))
		  (setf x mx y (- 480 my))))
	      (when (sdl:poll-event event*)
		(let ((type (sdl:event-type event*)))
		  (when (eql type :mousebuttondown)
		    (return-from event-loop)))))))))))
