;;; daemon.lisp --- Function for running the spread daemon.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :spread)

(defvar *daemon-parameters*
  '((port              *default-port*)
    (program           *default-daemon-program*)
    (host              "localhost")
    (host-address      "127.0.0.1")
    (broadcast-address "127.0.0.255")
    (wait              5))
  "List of keyword parameter names and default values for
start-daemon* functions.")

(defmacro define-start-daemon-function (name (&rest extra-args)
					doc
					&body body)
  "Define a \"start-daemon\" function named NAME with body BODY that
takes keyword arguments EXTRA-ARGS in addition to those defined in
`*daemon-parameters*'. DOC is concatenated with a default
documentation string."
  `(defun ,name (&rest args
		 &key
		 ,@*daemon-parameters*
		 ,@extra-args)
     (declare (ignorable args program wait))
     ,(concatenate
       'string
       "Start a spread daemon with the specified parameters and return
the process object. If the attempt fails, an `failed-to-start-daemon'
error is signaled."
       (unless (emptyp doc) " ")
       doc)
     (flet ((collect-daemon-options ()
	      `((:port              ,port)
		(:host              ,host)
		(:host-address      ,host-address)
		(:broadcast-address ,broadcast-address))))
       (declare (ignorable (function collect-daemon-options)))
       ,@body)))

(define-start-daemon-function start-daemon/no-restart ()
  ""
  ;; Generate a randomized configuration file name.
  (let ((config-filename (format nil "/tmp/spread-~8,'0X.conf"
				 (random (ash 1 (* 8 4))))))
    ;; Write the specified configuration to that file.
    (with-output-to-file (stream config-filename
				 :if-exists :supersede)
      (format stream
	      "Spread_Segment ~A:~A {~%~4T~A ~A~%}"
	      broadcast-address port host host-address))
    ;; Start a spread daemon that uses the configuration file.
    (let* ((output  (make-string-output-stream))
	   (process (sb-ext:run-program
		     program `("-n" ,host "-c" ,config-filename)
		     :output output
		     :error  output
		     :search t
		     :wait   nil)))
      ;; Store the port number and configuration file name in the
      ;; process object.
      (setf (getf (sb-ext:process-plist process) :port)
	    port
	    (getf (sb-ext:process-plist process) :config-file)
	    config-filename)
      ;; Give the spread daemon five seconds to start up then check
      ;; whether it died in the meantime.
      (sleep wait)
      (unless (sb-ext:process-alive-p process)
	(%cleanup-after-spread-daemon port config-filename)
	(sb-ext:process-wait process)
	(error 'failed-to-start-daemon
	       :program   program
	       :exit-code (sb-ext:process-exit-code process)
	       :options   (collect-daemon-options)
	       :output    (get-output-stream-string output)))
      ;; If everything looks good, return the process object.
      process)))

(define-start-daemon-function start-daemon ()
    "This function establishes a restart called `retry' around the
attempt to start the Spread daemon. Invoking the restart will cause
another attempt with identical options to be made."
  (let (result)
    (tagbody
     :start
       (restart-case
	   (setf result (apply #'start-daemon/no-restart args))
	 (retry ()
	   :report (lambda (stream)
		     (format stream "~@<Retry starting the Spread ~
daemon (executable ~S) with parameters ~_~{~{~A = ~S~}~^, ~_~}.~@:>"
			     program
			     (collect-daemon-options)))
	   (go :start))))
    result))

(define-start-daemon-function start-daemon/retry ((num-retries 4)
						  (retry-delay 10))
    "This function makes NUM-RETRIES attempts to start the Spread
daemon, waiting RETRY-DELAY seconds between attempts. If the final
attempt fails, an `failed-to-start-daemon' error is signaled."
  (let ((attempt 1))
    (handler-bind
	((failed-to-start-daemon
	  #'(lambda (condition)
	      (declare (ignore condition))
	      (let ((restart (find-restart 'retry)))
		(when (and restart (<= attempt num-retries))
		  (warn "~@<Spread daemon failed to start on ~:r ~
attempt~:[.~;; retrying.~]~@:>"
			attempt (< attempt num-retries))
		  (sleep retry-delay)
		  (incf attempt)
		  (invoke-restart restart))))))
      (apply #'start-daemon
	     (remove-from-plist args :num-retries :retry-delay)))))

(defun stop-daemon (process)
  "Stop the spread daemon PROCESS and clean the mess it leaves
behind."
  (let ((pid             (sb-ext:process-pid process))
	(port            (getf (sb-ext:process-plist process) :port))
	(config-filename (getf (sb-ext:process-plist process) :config-file)))
    ;; Send a SIGINT signal to the process and wait for it to die. Do
    ;; some cleanup afterwards.
    (sb-posix:kill pid sb-posix:SIGINT)
    (sb-ext:process-wait process)
    (%cleanup-after-spread-daemon port config-filename)
    (values)))

(defmacro with-daemon ((&key
			(port              '*default-port*)
			(program           '*default-daemon-program*)
			(host              "localhost")
			(host-address      "127.0.0.1")
			(broadcast-address "127.0.0.255")
			(wait              5)
			(num-retries       4)
			(retry-delay       10))
		       &body body)
  "Execute BODY with a Spread daemon using the specified parameters
running. see `start-daemon/retry' for an explanation of the
parameters."
  (with-unique-names (process-var)
    `(let ((,process-var (start-daemon/retry
			  :port              ,port
			  :program           ,program
			  :host              ,host
			  :host-address      ,host-address
			  :broadcast-address ,broadcast-address
			  :wait              ,wait
			  :num-retries       ,num-retries
			  :retry-delay       ,retry-delay)))
       (declare (ignorable ,process-var))
       (unwind-protect
	    (progn ,@body)
	 (stop-daemon ,process-var)))))


;;; Utility functions
;;

(defun %cleanup-after-spread-daemon (port config-filename)
  "Clean up the socket corresponding to PORT and the configuration
file CONFIG-FILENAME."
  (ignore-errors
    (delete-file config-filename))
  (ignore-errors
    (delete-file (format nil "/tmp/~A" port))))
