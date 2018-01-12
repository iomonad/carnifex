;; carnifex.lisp
;; 2018 - iomonad <me@trosa.io>

(ql:quickload '(:lispbuilder-sdl)
  :silent t)
(ql:quickload '(:unix-opts)
  :silent t)

(defun len (list)
  (if list
	  (1+ (len (cdr list)))
	0))

(defun usage (&key pname)
  (format t "usage: ~s <width> <height>~%"
		  pname))

(defun main ()
  (let* ((al (len *posix-argv*)))
	(if (not (= al 3))
		(usage :pname (nth 0 *posix-argv*))
	  nil))
  (let* ((width (nth 1 *posix-argv*))
		 (height (nth 2 *posix-argv*)))
	(format t "Width: ~f, Height: ~f~%"
			width height))
  (sb-ext:exit))
