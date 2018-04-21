;; carnifex.lisp
;;
;; A Conway’s automaton written in lisp.
;; 2018 - iomonad <me@trosa.io>
;;

(ql:quickload '(:lispbuilder-sdl)
              :silent t)
(ql:quickload '(:unix-opts)
              :silent t)
(load "globals.lisp")

(defun sanitize-input (&key argv)
  "Check if input arguments are correct"
  (let* ((y (parse-integer
             (nth 1 argv)))
         (x (parse-integer
             (nth 2 argv))))
    (when (or (< 1000 y)
              (< 1000 x))
      (lambda ()
        (format t "Les dimensions ne doivent pas exceder une longeur de 1000~%")
        (sb-ext:exit :code 1)))
    (when (or (> 0 y)
              (> 0 x))
      (lambda ()
        (format t "Les valeurs doivent etre positives~%")
        (sb-ext:exit :code 1)))
    (setq xi x)
    (setq yi y)))

(defun init (&key pname)
  "Check arguments and display usage when options is incorrect"
  (let ((banner "usage: ~s <width:int> <height:int> [-h --help -i --invert -t --traces]
positional arguments:
  width                 width of the grid
  height                height of the grid
optional arguments:
  -h, --help            show this help message and exit~%"))
    (when (or (loop for arg in *posix-argv*
                    when (or (string-equal "-h" arg)
                             (string-equal "--help" arg))
                    return t)
              (not (< 2 (length *posix-argv*))))
      (progn
        (format t banner pname)
        (sb-ext:exit :code 1))))
  'continue)

(defun init-globals ()
  (let ((d 1000)
        (defa 0))
    (setq width d)
    (setq height d)
    (setq zoom_dec_speed 2)
    (setq move_speed 10)
    (setq change_speed_game 5)
    (setq arr
          (make-array
           (list yi xi) :initial-element
           defa))
    (setq next_generation
          (make-array
           (list yi xi) :initial-element
           defa))))

(defun main ()
  (init :pname
        (car sb-ext:*posix-argv*))
  (sanitize-input :argv *posix-argv*)
  (sb-ext:exit :code 0))

(sb-int:with-float-traps-masked
 (:invalid :inexact :overflow) (main))
