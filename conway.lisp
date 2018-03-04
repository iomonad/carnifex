;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

(defparameter *matrix* nil) ;; mutable container

(defun key-handler (key)
  "Key  handler"
  (if (sdl:key= key :sdl-key-escape)
	  (sdl:push-quit-event)))

(defun initalise-matrice ()
  "Generate matrice"
  (setq *matrix* (make-array (list *av1* *av2*))))

(defun boucle-cyclique ()
  (conway-loop)
  (sdl:update-display)
  (sdl:clear-display
   (sdl:color
    :r 127
    :g 29
	:b 127)))

(defun conway-automaton ()
  "Automaton main entry point"
  (initalise-matrice)
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
		(:key-down-event
		 (:key k) (key-handler k))
		(boucle-cyclique)))))

(declaim (sb-ext:muffle-conditions cl:warning))
