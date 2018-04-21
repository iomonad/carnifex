;; carnifex.lisp
;; A Conway’s automaton written in lisp
;; at 42.fr school.
;; 2018 - iomonad <me@trosa.io>

(ql:quickload '(:lispbuilder-sdl)
              :silent t)
(ql:quickload '(:unix-opts)
              :silent t)

(defun init (&key pname)
  "Check arguments and display usage when options is incorrect"
    (let ((banner "usage: ~s <width:int> <height:int> [-h --help -i --invert -t --traces]
positional arguments:
  width                 width of the grid
  height                height of the grid
optional arguments:
  -h, --help            show this help message and exit~%"))
      (if (or (loop for arg in *posix-argv*
                    when (or (string-equal "-h" arg)
                             (string-equal "--help" arg))
                    return t)
              (not (< 2 (length *posix-argv*))))
          (progn
            (format t banner pname)
            (sb-ext:exit))))
    'continue)

(defun main ()
  (init :pname
    (car sb-ext:*posix-argv*))
  (exit))

(sb-int:with-float-traps-masked
 (:invalid :inexact :overflow) (main))
