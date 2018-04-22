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

;; Input

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
      (lambda ()
        (format t banner pname)
        (sb-ext:exit :code 1))))
  'continue)

(defun init-globals ()
  "Initialise constants"
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

(defun parse-arguments (&key argv)
  "Retrieve options"
  (map 'list
       #'(lambda (a)
           (when (or (string-equal "-i" a)
                     (string-equal "--invert" a))
             (setq *invert* 0))
           (when (or (string-equal "-t" a)
                     (string-equal "--traces" a))
             (setq *trace* 1)))
       argv))

;; Algorithm

(defun get-current-cell (i j)
  "Retrieve cell in matrice"
  (aref arr i j))

(defun display-trace ()
  "Predicate for trace support"
  (if (EQUAL *trace* 1) 2
    0))

(defun get-next-cell (i j)
  "Retrieve cell in next-level matrice"
  (aref next_generation i j))

(defun is-alive (i j)
  "Check if cell is alive"
  (EQUAL (get-current-cell i j)
         1))

(defun randomize ()
  "Fill matrice"
  (dotimes (i yi)
    (dotimes (j xi)
      (setf (aref arr i j)
            (random 2))))
  arr)

(defun is-nearby (i j)
  "Neighbors predicate"
  (when (AND (> i -1)
           (< i yi)
           (> j -1)
           (< j xi))
      (if (equal (get-current-cell i j) 1) 1
        0)
      0))

(defun get-neighbor (i j)
  "Retrieve neighbors"
  (+ (is-nearby (+ i 1) j)
     (is-nearby (- i 1) j)
     (is-nearby i (+ j 1))
     (is-nearby i (- j 1))
     (is-nearby (- i 1) (- j 1))
     (is-nearby (+ i 1) (+ j 1))
     (is-nearby (- i 1) (+ j 1))
     (is-nearby (+ i 1) (- j 1))))

(defun determine-neighbor ()
  "Calculate neighbors"
  (dotimes (i yi)
    (dotimes (j xi)
      (setf (aref next_generation i j)
            (get-neighbor i j))))
  next_generation)

;; Conways Rules

(defun rule1 (i j)
  (if (NOT(is-alive i j))
      (get-current-cell i j)
    (if (NOT(< (get-next-cell i j) 2))
        1
      (display-trace))))

(defun rule2 (i j)
  (if (NOT (is-alive i j))
      (get-current-cell i j)
    (if (OR (EQUAL (get-next-cell i j) 2)
            (EQUAL (get-next-cell i j) 3))
        1
      (display-trace))))

(defun rule3 (i j)
  (if (NOT(is-alive i j))
      (get-current-cell i j)
    (if (NOT (> (get-next-cell i j) 3))
        1
      (display-trace))))

(defun rule4 (i j)
  (if (is-alive i j)
      (get-current-cell i j)
    (if (EQUAL (get-next-cell i j) 3)
        1
      (get-current-cell i j))))

;; Post Rules

(defun alter-life ()
  "Calculate the life of the rule"
  (dotimes (i yi)
    (dotimes (j xi)
      (setf (aref arr i j)
            (rule1 i j))
      (setf (aref arr i j)
            (rule3 i j))
      (setf (aref arr i j)
            (rule4 i j))))
  arr)

(defun conway-automaton ()
  "Algorithm entry point"
  (setq next_generation                 ; Get next generation
        (determine-neighbor))
  (setq arr (alter-life)))                ; Retrieve cursor

;; Graphic

(defun print-box (xn yn size color)
  "Display box in the sdl scene"
  (sdl:draw-box (sdl:rectangle-from-midpoint-*
                 (+ xn (floor size 2))
                 (+ yn (floor size 2)) size size)
                :color color))

(defun swap-color ()
  "Predicate to determine colors from params"
  (if (EQUAL *invert* 1) 1 0))

(defun print-gui-board (arr x y tile_size)
  "Display board from references"
  (sdl:clear-display sdl:*black*)
  (dotimes (y2 y)
    (dotimes (x2 x)
      (if (eq (eq (aref arr y2 x2) (swap-color)) T)
          (print-box (+ 1 (* x2 tile_size ) move_x)
                     (+ 1 (* y2 tile_size) move_y)
                     (- tile_size (floor tile_size 10)) *white*))
      (if (eq (aref arr y2 x2) 2)
          (print-box (+ 1 (* x2 tile_size) move_x)
                     (+ 1 (* y2 tile_size) move_y)
                     (- tile_size (floor tile_size 10)) *red*))
      ))
  (sdl:update-display))

(defun zoom ()
  "Zoom in the global scope"
  (when (< tile_size 400)
    (lambda()
      (setf tile_size (+ tile_size zoom_dec_speed))
      (setf move_x (- move_x 50))
      (setf move_y (- move_y 50)))))

(defun dezoom ()
  "Un-zoom the goba scope"
  (when (> tile_size zoom_dec_speed)
    (lambda ()
      (setf tile_size (- tile_size zoom_dec_speed))
      (setf move_x (+ move_x 50))
      (setf move_y (+ move_y 50)))))

(defun manage-speed (arg)
  "Function helper to manage game speed"
  (if (eq arg T)
      (lambda ()
        (if (>= speed_game 20)
            (setf speed_game
                  (- speed_game change_speed_game))))
    (lambda ()
      (if (<= speed_game 100)
          (setf speed_game(+ speed_game
                             change_speed_game))))))

;; Keybinds


(defun keybind-callback-handler (key)
  "Handle keybinds with severals callbacks"
  (if (sdl:key= key :sdl-key-escape)
      (sdl:push-quit-event))
  (if (or (sdl:key= key :sdl-key-left) (sdl:key= key :sdl-key-a))
      (setf move_x (+ move_x move_speed)))
  (if (or (sdl:key= key :sdl-key-right) (sdl:key= key :sdl-key-d))
      (setf move_x (- move_x move_speed)))
  (if (or (sdl:key= key :sdl-key-up) (sdl:key= key :sdl-key-w))
      (setf move_y (+ move_y move_speed)))
  (if (or (sdl:key= key :sdl-key-down) (sdl:key= key :sdl-key-s))
      (setf move_y (- move_y move_speed)))
  (if (sdl:key= key :sdl-key-period)
      (manage-speed T))
  (if (sdl:key= key :sdl-key-comma)
      (manage-speed nil))
  (if (or (sdl:key= key :sdl-key-q) (sdl:key= key :sdl-key-kp-minus))
      (dezoom))
  (if (or (sdl:key= key :sdl-key-e) (sdl:key= key :sdl-key-kp-plus))
      (zoom))
  (if (or (sdl:key= key :sdl-key-r))
      (setq arr (make-array (list yi xi) :initial-element 0)))
  (if (or (sdl:key= key :sdl-key-p) (sdl:key= key :sdl-key-space))
      (if (= pause 0)
          (setf pause 1)
        (setf pause 0)))
  (print-gui-board arr xi yi tile_size))

;; Main

(defun main ()
  (when (= 2 (length *posix-argv*))
    (init :pname
          (car sb-ext:*posix-argv*))
    (sanitize-input :argv *posix-argv*)
    (parse-arguments :argv *posix-argv*))
  (sb-ext:exit :code 0))

(sb-int:with-float-traps-masked
 (:invalid :inexact :overflow) (main))
