;; carnifex.lisp
;; 2018 - iomonad <me@trosa.io>

(ql:quickload '(:lispbuilder-sdl)
			  :silent t)
(ql:quickload '(:unix-opts)
			  :silent t)

(defstruct display wwin hwin frate)
(defparameter *display*
  (make-display :wwin 800
                :hwin 800
                :frate 60))

(defun len (list)
  (if list
	  (1+ (len (cdr list)))
	  0))

(defun usage (&key pname)
  (lambda ()
	(format t "usage: ~s <width> <height>~%"
			pname)
	(sb-ext:exit)))

(defun conway-automaton ()
  (sdl:with-init ()
    (let ((wwin (display-wwin *display*))
		  (hwin (display-hwin *display*))
		  (frate (display-frate *display*)))
	  (sdl:window wwin hwin
				  :title-caption "Conway Automaton"
				  :icon-caption "Conway Automaton")
	  (setf (sdl:frame-rate) frate)
	  (sdl:clear-display
	   (sdl:color :r 30 :g 30 :b  57))
	  (sleep 10))))

(defun main ()
  (let* ((al (len *posix-argv*)))
	(if (not (= al 3))
		(usage :pname (nth 0 *posix-argv*))
		nil))
  (let* ((width (nth 1 *posix-argv*))
		 (height (nth 2 *posix-argv*)))
	(format t "Width: ~f, Height: ~f~%"
			width height))
  (conway-automaton)
  (sb-ext:exit))
