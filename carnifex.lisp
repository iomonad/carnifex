;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

(ql:quickload '(:lispbuilder-sdl)
              :silent t)
(ql:quickload '(:unix-opts)
              :silent t)

(load "utils.lisp")
(load "data.lisp")

(defun life-loop ()
  (life-loop))

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
	  (sdl:with-events ()
					   (:quit-event () t)
		(:key-down-event ()
		  (when (or (sdl:key-down-p :sdl-key-q)
					(sdl:key-down-p :sdl-key-escape))
			(sdl:push-quit-event)))))))

(defun main ()
  (usage :pname (car sb-ext:*posix-argv*))
  (let* ((width (parse-integer (nth 1 *posix-argv*)))
         (height (parse-integer (nth 2 *posix-argv*))))
	(if (and (typep width 'integer)
			 (typep height 'integer))
		(format t "Width: ~d, Height: ~d~%"
				width height)
	  (usage :pname (nth 0 *posix-argv*))))
  (sb-ext:exit))
