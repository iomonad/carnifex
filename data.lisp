;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

(defstruct display wwin hwin frate)
(defparameter *display*
  (make-display :wwin 800
                :hwin 800
                :frate 60))
