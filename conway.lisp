;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

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
