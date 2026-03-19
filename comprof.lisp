;; I'm not sure why this is ``necessary'' but many programs do it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt) :silent t))

(defpackage :comprof
  (:use :cl)
  (:export :main
	   :comprof))

(in-package :comprof)

;;
;; Misc.
;;
(defun exit-with-error (message &optional (code 1))
  (format *error-output* "~A~%Exiting...~%" message)
  (uiop:quit code))


;;
;; Condition handling
;;
(define-condition shell-error (error) ()
  (:documentation "Error for when the given command has a non-zero exit status"))

(defun ignore-shell-error (e)
  (declare (ignore e))
  (invoke-restart 'continue))

(defun do-not-ignore-shell-error (e)
  (declare (ignore e))
  (error 'shell-error))

(defun handle-vague-simple-error (e)
  "UIOP:RUN-PROGRAM signals a SIMPLE-ERROR if the given command does not
exist. It may very well signal the same sort of error for a host of
other reasons, too. This function acts appropriately."
  (declare (simple-error e))
  (let ((error-text (format nil "~A" e)))
    (cond ((string= (subseq error-text 9 16) "execute")
           (exit-with-error error-text))
          (t (exit-with-error (format nil "Unknown error: ~A" error-text))))))


;;
;; Com profing
;;
(defun execution-duration (command)
  "Return the number of internal time units it takes for the system to
execute the command specified by `command'."
  (- (- (get-internal-real-time)
	(progn (uiop:run-program command)
	       (get-internal-real-time)))))

(defun comprof (command &optional (times 20) (interval 0) (ignore-shell-error nil))
  "Run `command' `times' times, waiting `interval' seconds between runs,
then print the total duration of all executions alongside the average
duration of each execution to stdout.

If `ignore-shell-error' is non-nil comprof accumulates the times of
executions which result in non-zero exit codes. Otherise comprof exits
on such occasions."
  (declare (integer times)
           (list command))
  (handler-bind ((uiop/run-program:subprocess-error
                   (if ignore-shell-error
                       #'ignore-shell-error
                       #'do-not-ignore-shell-error))
                 (simple-error #'handle-vague-simple-error))
    (let* ((total (loop for time from 1 to times
		        summing (execution-duration command)
                        do (sleep interval)))
	   (total-ms (/ total (/ internal-time-units-per-second 1000.0)))
	   (average-ms (/ total-ms times)))
      (format t "Executed ~d times:~@
                 - Total execution time: ~dms~@
                 - Avg. execution time:  ~dms~%"
	      times
	      total-ms
	      average-ms))))


;;;
;;; CLI
;;;
(defparameter *option-interface*
  (adopt:make-interface
   :name    "comprof"
   :summary "time shell commands/scripts"
   :usage   "[-e] [-t TIMES] COMMAND"
   :help    (format nil "Run COMMAND exactly TIMES times and report the average~
                         duration of its execution")
   :contents `(,(adopt:make-option
		 'times
		 :short     #\t
		 :help      "number of times to run COMMAND"
		 :parameter "TIMES"
		 :reduce    (lambda (old new)
			      (declare (ignore old))
			      (parse-integer new))
		 :initial-value 20)
               ,(adopt:make-option
                 'interval
                 :short     #\s
                 :help      "seconds to sleep between COMMAND executions"
                 :parameter "INTERVAL"
                 :reduce    (lambda (old new)
                              (declare (ignore old))
			      (with-input-from-string (stream new)
                                (read stream)))
                 :initial-value 0)
	       ,(adopt:make-option
		 'ignore-shell-error
		 :short     #\e
		 :help      "don't exit after encountering a non-zero return value"
		 :reduce    (constantly t)
		 :initial-value nil)
               ,(adopt:make-option
                 'help
                 :short     #\h
                 :long      "help"
                 :help      "print help"
                 :reduce    (constantly t)
                 :initial-value nil))))

(defun main ()
  "Entrypoint for command-line-invocation of comprof."
  (multiple-value-bind (shell-invocation options)
      (adopt:parse-options-or-exit *option-interface* (rest (adopt:argv)) t)
    (when (gethash 'help options)
      (adopt:print-help-and-exit *option-interface* :option-width 12))
    (when (null shell-invocation)
      (exit-with-error "No command given."))
    (handler-case (with-user-abort:with-user-abort
		    (comprof shell-invocation
                             (gethash 'times              options)
                             (gethash 'interval           options)
                             (gethash 'ignore-shell-error options)))
      (with-user-abort:user-abort ()
	(exit-with-error "Received user interrupt." 130))
      (shell-error ()
	(exit-with-error "Given command encountered an error and -e was not specified")))))
