;; carnifex.lisp
;; 2018 - iomonad <me@trosa.io>

;; (ql:quickload "lispbuilder-sdl")
(ql:quickload "unix-opts"
  :silent t)

(defun len (list)
  (if list
	  (1+ (len (cdr list)))
	0))

(defun usage ()
  (format t "usage: carniflex <width> <height>~%"))

(defun main ()
  (let* ((al (len *posix-argv*)))
	(if (not (= al 3))
		(usage)
	  nil))
  (let* ((width (nth 1 *posix-argv*))
		 (height (nth 2 *posix-argv*)))
	(format t "Width: ~f, Height: ~f~%"
			width height))
  (sb-ext:exit))
