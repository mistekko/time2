;; todo:
;; T clean up object model
;; F find some way to restrict integer arguments to being positive
;; A better result reporting
;; F handle CLI interrupts
;; F remove `sets' functionality as it's utterly useless
;; A -h help option
;; change *options* from a global dynamic variable to a class-allocated slot of option
;; support pipes
;; (case 0
;;   (0 nil) ;; use command-line arguments
;;   (1 (setq sb-ext:*posix-argv* '("profile-command" "-s" "9" "-t" "99" "ls"))) ;; no issue
;;   (2 (setq sb-ext:*posix-argv* '("profile-command" "-s" "-t" "99" "ls"))) ;; missing arg for option
;;   (3 (setq sb-ext:*posix-argv* '("profile-command" "-q" "-t" "99" "ls"))) ;; nonexistent flag
;;   (4 (setq sb-ext:*posix-argv* '("profile-command" "-s" "" "-t" "99" "ls"))) ;; empty argument for flag
;;   (5 (setq sb-ext:*posix-argv* '("profile-command" "-s" "10" "" "-t" "99" "ls"))) ;; random empty argument
;;   (6 (setq sb-ext:*posix-argv* '("profile-command" "-s" "beryllium" "ls"))) ;; nonsense argument
;;   (7 (setq sb-ext:*posix-argv* '("profile-command" "-s" "-10" "ls")))) ;; bad argument--currentnly unhandled

(require 'asdf)

(defvar *usage* (format nil "Usage: ~A [-eh] [-s sets] [-t times] command arg* ~%"
			(nth 0 sb-ext:*posix-argv*))
  "Command help message")
(defvar *options* nil "Alist of flags and their corresponding option objects")
(defvar *set-averages* nil "List of average timings for each set")
(defvar *overall-average* 0 "Average `command' execution time")

;; debug

(defmacro print-warning (message)
  "Print ``Warning: \" followed by `message' to std. error"
  `(format *error-output* "Warning: ~a~%" ,message))

(defmacro print-err-and-exit (format-string &rest args)
  "If `format-string' is non-nil, print ``Error:\" followed by the string
formatted using `format-string' as the control-string and `args' as the args;
finally, exit."
  `(progn
     (when ,format-string (format *error-output*
				 "Error: ~a~%"
				 (format nil ,format-string ,@args)))
     (format *error-output* *usage*)
     (exit :code 1)))

;;; option parsing

(defclass option ()
  ((flag  :initarg :flag :type string)
   (value :initarg :value :accessor value)))
(defclass string-option  (option) ((value :type string)))
(defclass integer-option (option) ((value :type integer)))
(defclass boolean-option (option) ((value :type boolean)))

(defgeneric set-value (option value)
  (:documentation "Set the value slot of an option object after
converting `value' to the appropriate type"))

(defmethod set-value ((option integer-option) (value string))
  (setf (value option) (parse-integer value)))
(defmethod set-value ((option string-option) (value string))
  (setf (value option) value))
(defmethod set-value ((option boolean-option) (value (eql t)))
  (setf (value option) t))
(defmethod set-value ((option boolean-option) (value (eql nil)))
  (setf (value option) nil))

(define-condition premature-arglist-end-error (error) ())

(defmacro make-options (&rest options)
  "options::= (option*)
option::= (option-class flag initial-value).
`flag' is evaluated multiple times in the macro-expansion."
  `(list ,@(mapcar (lambda (option)
		     (destructuring-bind (class flag ival) option
		       `(cons ,flag (make-instance ',class
						   :flag  ,flag
						   :value ,ival))))
		   options)))

(defmacro handle-option (flag &optional args)
  "Set the option in `*options*' corresponding to `flag', popping items from
`args' for option arguments as needed"
  `(let* ((option (cdr (assoc ,flag *options*
			      :test #'string=)))
	  (value (if (eq (type-of option) 'boolean-option)
		     t
		     (pop ,args))))
     (unless option (print-err-and-exit "Unrecognised option: ~s" ,flag))
     (handler-case (set-value option value)
       (parse-error ()
	 (print-err-and-exit "Bad argument for option ~a: ~s" ,flag value)))))

(defun process-argv ()
  "Parse command-line arguments and assign options in `*options*'accordingly"
  (loop for current-flag = (pop sb-ext:*posix-argv*)
	when (not (handler-case (char= #\- (elt current-flag 0))
		    (type-error () (error 'premature-arglist-end-error)))) ;;;; sohuld
	  return (push current-flag sb-ext:*posix-argv*)
	do (handle-option current-flag sb-ext:*posix-argv*)))

(defun get-option-value (flag)
  "Return the value of the option in `*options*' associated with `flag'."
  (value (cdr (assoc flag *options* :test #'string=))))

;;; program

(defun command-run-time (command &key (output :interactive))
  "Run program given in `command' as a list of a command name followed by
arguments and return the time it takes to complete execution."
  (- (- (get-internal-run-time)
	(progn (uiop:run-program command :output output
					 :force-shell nil)
	       (get-internal-run-time)))))

(defun main ()
  (setq *options* (make-options (integer-option "-s" 10)
				(integer-option "-t" 100)
				(boolean-option "-e" nil)
				(boolean-option "-h" nil)))
  (pop sb-ext:*posix-argv*) ; argv[0] is the invoked command, e.g. `sbcl'
  (handler-case (process-argv)
    (premature-arglist-end-error ()
      (print-err-and-exit (unless (get-option-value "-h")
			    "Command argument not found"))))
  (when (string= (car sb-ext:*posix-argv*) "")
    (print-err-and-exit "Empty string passed as command"))
  (when (get-option-value "-h")
    (print-err-and-exit nil))
  (let ((sets  (get-option-value "-s"))
	(times (get-option-value "-t"))
	(ignore-errors (get-option-value "-e")))
    (loop for set from 0 below sets
	  for duration
	    = (loop for time from 0 below times
		    summing (handler-case (command-run-time sb-ext:*posix-argv*
							    :output nil)
			      (uiop:subprocess-error ()
				(unless ignore-errors
				  (print-err-and-exit
				   "~a failed on iteration ~d in set ~d"
				   (car sb-ext:*posix-argv*) time set)))))
	  ;;;; the following `do'-forms should somehow be turned into
	  ;;;; accumulating `loop'-clauses
	  do (push (/ duration times)
		   *set-averages*)
	     (incf *overall-average* duration)
	  finally (setq *overall-average* (/ *overall-average*
					     (* times sets))))
    (format t "Average execution time: ~fμs~%" *overall-average*)))
