;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

(defun len (list)
  "Compute len af a list"
  (if list
      (1+ (len (cdr list)))
      0))
