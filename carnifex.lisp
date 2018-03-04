;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

(ql:quickload '(:lispbuilder-sdl)
              :silent t)
(ql:quickload '(:unix-opts)
              :silent t)

(load "utils.lisp")
(load "data.lisp")
(load "conway.lisp")

(defun main ()
  (usage :pname (car sb-ext:*posix-argv*))
  (let* ((width (parse-integer (nth 1 *posix-argv*)))
         (height (parse-integer (nth 2 *posix-argv*))))
	(if (and (typep width 'integer)
			 (typep height 'integer))
		(progn
		  (format t "Width: ~d, Height: ~d~%"
				  width height)
		  (conway-automaton))
	  (usage :pname (nth 0 *posix-argv*))))
  (sb-ext:exit))
