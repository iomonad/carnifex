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
      (progn
        (format t "Les dimensions ne doivent pas exceder une longeur de 1000~%")
        (sb-ext:exit :code 1)))
    (when (or (> 0 y)
              (> 0 x))
      (progn
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
  "Initialise constants"
  (let ((d 1000))
    (setq width d)
    (setq height d)
    (setq zoom_dec_speed 2)
    (setq move_speed 10)
    (setq change_speed_game 5)
    (setq arr
      (make-array
       (list yi xi) :initial-element 0))
    (setq next_generation
      (make-array
       (list yi xi) :initial-element 0))))

(defun parse-arguments (&key argv)
  "Retrieve options"
  (map 'list
       #'(lambda (a)
           (when (or (string-equal "-i" a)
                     (string-equal "--invert" a))
             (setq *invert* 0))
           (when (or (string-equal "-d" a)
                     (string-equal "--debug" a))
             (setq *debug* 1))
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
  (if (AND (> i -1)
             (< i yi)
             (> j -1)
             (< j xi))
    (if (equal (get-current-cell i j) 1) 1 0) 0))

(defun get-neighbor (i j)
  "Retrieve neighbors"
  (when (eq *debug* 1)
    (format t "Neighbor call i:~d - j:~d~%"
            i j))
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
  (when (eq *debug* 1)
    (format t "Value of yi:~d and xi:~d ~%"
            yi xi))
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
  (setf next_generation                 ; Get next generation
        (determine-neighbor))
  (setf arr (alter-life)))                ; Retrieve cursor

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

(defun debug-sdl-board (arr)
  (when (EQUAL 1 *debug*)
    (format t "[*] speed:~d - cur:~d with objects: ~% ~s ~%"
            speed_game cur_time arr)))

(defun print-sdl-board (arr x y tile_size)
  "Display board from references"
  (sdl:clear-display sdl:*black*)
  (debug-sdl-board arr)
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
    (progn
      (setf tile_size (+ tile_size zoom_dec_speed))
      (setf move_x (- move_x 50))
      (setf move_y (- move_y 50)))))

(defun dezoom ()
  "Un-zoom the goba scope"
  (when (> tile_size zoom_dec_speed)
    (progn
      (setf tile_size (- tile_size zoom_dec_speed))
      (setf move_x (+ move_x 50))
      (setf move_y (+ move_y 50)))))

(defun manage-speed (arg)
  "Function helper to manage game speed"
  (if (eq arg T)
      (progn
        (if (>= speed_game 20)
            (setf speed_game
                  (- speed_game change_speed_game))))
    (progn
      (if (<= speed_game 100)
          (setf speed_game (+ speed_game
                              change_speed_game))))))

;; Keybinds

(defun keybind-callback-handler (key)
  "Handle keybinds with severals callbacks"
  (when (eq *debug* 1)
    (format t " Key handled: ~s ~%"
            key))
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
  (if (or (sdl:key= key :sdl-key-p)
          (sdl:key= key :sdl-key-space))
      (progn
        (if (= pause 0)
            (setf pause 1)
          (setf pause 0))
        (when (eq *debug* 1)
          (format t "State: ~d~%" pause))))
  (print-sdl-board arr xi yi tile_size)) ; Recompute board

;; Mouse events

(defun mouse-callback-handler ()
  "Handle mouse events "
  (if (sdl:mouse-left-p)
      (progn
        (if (or (sdl:key-down-p :sdl-key-lctrl)
                (sdl:key-down-p :sdl-key-rctrl))
            (progn
              (if (and (eq last_x 0)
                       (eq last_y 0))
                  (progn
                    (setf last_x (sdl:mouse-x))
                    (setf last_y (sdl:mouse-y)))
                (progn
                  (let*
                      ((x_act (sdl:mouse-x))
                       (y_act (sdl:mouse-y)))
                    (setf move_x (+ move_x (- x_act last_x)))
                    (setf move_y (+ move_y (- y_act last_y)))
                    (setf last_x x_act)
                    (setf last_y y_act)))))
          (let ((i_tab (floor (- (sdl:mouse-y) move_y)
                               tile_size))
                 (j_tab (floor (- (sdl:mouse-x) move_x)
                               tile_size)))
            (if (eq (and (>= i_tab 0)
                           (< i_tab yi) ; Global y
                           (>= j_tab 0)
                           (< j_tab xi)) T) ;Global x
              (setf (aref arr i_tab j_tab) 1))))))
  (if (sdl:mouse-right-p)
      (let
          ((i_tab (floor (- (sdl:mouse-y) move_y)
                         tile_size))
           (j_tab (floor (- (sdl:mouse-x) move_x)
                         tile_size)))
        (if (eq (and (>= i_tab 0)
                       (< i_tab yi)     ; Global y
                       (>= j_tab 0)
                       (< j_tab xi)) T) ; Global x
          (setf (aref arr i_tab j_tab)
                0))))
  (print-sdl-board arr xi yi tile_size)) ; Recompute board

;; Entry point

(defun automaton-kernel ()
  "Entrypoint automaton loop"
  (sdl:with-init ()
    (sdl:window width height :title-caption "Carnifex")
    (setf (sdl:frame-rate) 60)
    (sdl:update-display)
    (sdl:enable-key-repeat nil nil)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event ()
        (sdl:update-display))
      (:mouse-button-up-event
       (:button button)
       (if (= button 4) ; When down
           (if (or (sdl:key-down-p :sdl-key-lshift)
                   (sdl:key-down-p :sdl-key-rshift))
               (manage-speed nil) ; shifted
             (dezoom)))
       (if (= button 5) ; When up
           (if (or (sdl:key-down-p :sdl-key-lshift)
                   (sdl:key-down-p :sdl-key-rshift))
               (manage-speed t) ; shifted
             (zoom)))
       (if (= button 1)
           (progn
             (setf last_x 0)
             (setf last_y 0))))
      (:key-down-event (:key key)
        (keybind-callback-handler key))
      (:idle ()
        (mouse-callback-handler)
        (if (eq (eq pause 0) T)
            (progn
              (setf cur_time (get-internal-run-time))
              (let ((time_wait (- last_time
                                  (- cur_time speed_game))))
                (if (> time_wait 0)
                    (sleep (/ time_wait
                              100))))
              (conway-automaton) ; This is the meat
              (setf last_time
                    cur_time)))))))

;; Main

(defun main ()
  (init :pname
        (car sb-ext:*posix-argv*))
  (sanitize-input :argv *posix-argv*)
  (parse-arguments :argv *posix-argv*)
  (init-globals)
  (automaton-kernel)                 ; Run automaton
  (sb-ext:exit :code 0))

(sb-int:with-float-traps-masked
 (:invalid :inexact :overflow) (main))
