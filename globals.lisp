;; globals.lisp
;;
;; A Conway’s automaton written in lisp.
;; 2018 - iomonad <me@trosa.io>
;;

(defparameter width 1000)
(defparameter height 1000)
(defparameter tile_size 15)
(defparameter *invert* 1)
(defparameter *trace* 0)
(defparameter *debug* 0)
(defparameter xi 0)                     ; Matrix width
(defparameter yi 0)                     ; Matrix height
(defparameter last_x 0)
(defparameter last_y 0)
(defparameter move_x 0)
(defparameter move_y 0)
(defparameter pause 1)
(defparameter zoom_dec_speed 0)
(defparameter move_speed 0)
(defparameter speed_game 20)
(defparameter change_speed_game 5)
(defparameter cur_time 0)
(defparameter last_time 0)
(defparameter arr nil)
(defparameter next_generation nil)
(defparameter *white* sdl:*white*)
(defparameter *red* (sdl:color :r 137 :g 137 :b 137))
