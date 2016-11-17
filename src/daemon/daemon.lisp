;;;; daemon.lisp --- Function for running the spread daemon.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.daemon)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *daemon-parameters*
    '((port              *default-port*)
      (program           *default-daemon-program*)
      (host              "localhost")
      (host-address      "127.0.0.1")
      (broadcast-address "127.0.0.255")
      (reuse-socket?     t)
      (wait              2))
    "List of keyword parameter names and default values for
     start-daemon* functions."))

(defmacro define-start-daemon-function (name (&rest extra-args)
                                        doc
                                        &body body)
  "Define a \"start-daemon\" function named NAME with body BODY that
   takes keyword arguments EXTRA-ARGS in addition to those defined in
   `*daemon-parameters*'.

   DOC is concatenated with a default documentation string."
  `(defun ,name (&rest args
                 &key
                 ,@*daemon-parameters*
                 ,@extra-args)
     (declare (ignorable args program wait))
     ,(format nil
              "Start a spread daemon with the specified parameters and ~
               return the process object. If the attempt fails, an ~
               `failed-to-start-daemon' error is signaled.~@[ ~A~]"
              doc)
     (when (eq port :random)
       (setf port (+ 1024 (random (- 65535 1024)))))
     (flet ((collect-daemon-options ()
              `((:port              ,port)
                (:host              ,host)
                (:host-address      ,host-address)
                (:broadcast-address ,broadcast-address)
                (:reuse-socket?     ,reuse-socket?))))
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
              "Spread_Segment ~A:~A {~%~
                   ~4T~A ~A~%~
               }~
               ~:[~;~%~%SocketPortReuse = ON~%~]~%"
              broadcast-address port host host-address reuse-socket?))
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
   attempt to start the Spread daemon.

   Invoking the restart will cause another attempt with identical
   options to be made."
  (let (result)
    (tagbody
     :start
       (restart-case
           (setf result (apply #'start-daemon/no-restart args))
         (use-daemon (program)
           :report      (lambda (stream)
                          (format stream "~@<Use a different Spread ~
                                          daemon executable (instead ~
                                          of ~S) and retry starting ~
                                          the daemon.~@:>"
                                  program))
           :interactive (lambda ()
                          (format *query-io* "Spread daemon executable (not evaluated): ")
                          (force-output *query-io*)
                          (list (read-line *query-io*)))
           (setf (getf args :program) program)
           (go :start))
         (retry ()
           :report (lambda (stream)
                     (format stream "~@<Retry starting the Spread ~
                                     daemon (executable ~S) with ~
                                     parameters ~_~{~{~A = ~S~}~^, ~
                                     ~_~}.~@:>"
                             program
                             (collect-daemon-options)))
           (go :start))))
    (values result (format nil "~D~:[@~A~;~]"
                           port (string= host "localhost") host))))

(define-start-daemon-function start-daemon/retry ((num-attempts 4)
                                                  (retry-delay 10))
  "This function makes NUM-ATTEMPTS attempts to start the Spread
   daemon, waiting RETRY-DELAY seconds between attempts.

   If the final attempt fails, an `failed-to-start-daemon' error is
   signaled."
  (let ((attempt 1))
    (handler-bind
        ((failed-to-start-daemon
          (lambda (condition)
            (declare (ignore condition))
            (let ((restart (find-restart 'retry)))
              (when (and restart (< attempt num-attempts))
                (warn "~@<Spread daemon failed to start on ~:r ~
                       attempt~:[.~;; retrying.~]~@:>"
                      attempt (< attempt num-attempts))
                (sleep retry-delay)
                (incf attempt)
                (invoke-restart restart))))))
      (apply #'start-daemon
             (remove-from-plist args :num-attempts :retry-delay)))))

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
                        (reuse-socket?     t)
                        (wait              5)
                        (num-attempts      4)
                        (retry-delay       10))
                       &body body)
  "Execute BODY with a Spread daemon using the specified parameters
   running.

   See `start-daemon/retry' for an explanation of the parameters."
  (with-unique-names (process-var)
    `(let ((,process-var (start-daemon/retry
                          :port              ,port
                          :program           ,program
                          :host              ,host
                          :host-address      ,host-address
                          :broadcast-address ,broadcast-address
                          :reuse-socket?     ,reuse-socket?
                          :wait              ,wait
                          :num-attempts      ,num-attempts
                          :retry-delay       ,retry-delay)))
       (declare (ignorable ,process-var))
       (unwind-protect
            (progn ,@body)
         (stop-daemon ,process-var)))))

;;; Utility functions

(defun %cleanup-after-spread-daemon (port config-filename)
  ;; Clean up the socket corresponding to PORT and the configuration
  ;; file CONFIG-FILENAME.
  (ignore-errors (delete-file config-filename))
  (ignore-errors (delete-file (format nil "/tmp/~A" port))))
