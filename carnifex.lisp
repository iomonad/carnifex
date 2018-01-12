;; carnifex.lisp
;; 2018 - iomonad <me@trosa.io>

;; (ql:quickload "lispbuilder-sdl")
(ql:quickload "unix-opts"
			  :silent t)

(defun main ()
  (format t "Hello world.~%")
  (sb-ext:exit))
