;; carnifex.lisp
;; A Conway’s automaton.
;; 2018 - iomonad <me@trosa.io>

(defun len (list)
  "Compute len af a list"
  (if list
      (1+ (len (cdr list)))
	0))

(defun numargs (&rest arguments)
  "Retrieve len of args"
  (length arguments))

(defmacro if-let (test bindings &body body)
  (let ((bindings (mapcar #'(lambda (binding)
                              (destructuring-bind (variable &optional then else)
                                  (if (listp binding) binding (list binding))
                                (list variable then else)))
                          bindings)))
    `(multiple-value-bind,
	  (mapcar 'first bindings)
         (if ,test
             (values ,@(mapcar 'second bindings))
             (values ,@(mapcar 'third bindings)))
		 ,@body)))

(defun usage (&key pname)
  "Pretty printer for usage and arg checker"
  (let ((banner "usage: ~s [-h --help] <width:int> <height:int>
positional arguments:
  width                 width of the grid
  height                height of the grid
optional arguments:
  -h, --help            show this help message and exit~%"))
	(if (or (loop for arg in *posix-argv*
				  when (or (string-equal "-h" arg)
						   (string-equal "--help" arg))
				  return t)
			(not (equal 3 (length *posix-argv*))))
		(progn
		  (format t banner pname)
		  (sb-ext:exit))))
	'continue) ; Don't kill thread
