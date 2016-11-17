;;;; conditions.lisp --- Conditions used in the daemon module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.daemon)

(define-condition failed-to-start-daemon (network.spread:spread-error)
  ((program   :initarg  :program
              :type     string
              :reader   failed-to-start-daemon-program
              :documentation
              "The name of the Spread daemon executable that was
               used.")
   (exit-code :initarg  :exit-code
              :type     integer
              :reader   failed-to-start-daemon-exit-code
              :documentation
              "The exit code returned by the Spread daemon.")
   (options   :initarg  :options
              :type     list
              :reader   failed-to-start-daemon-options
              :documentation
              "The configuration with which the Spread daemon was
               started.")
   (output    :initarg  :output
              :type     string
              :reader failed-to-start-daemon-output
              :documentation
              "The output (concatenation of standard output and
               standard error) of the Spread daemon."))
  (:default-initargs
   :program   (missing-required-initarg 'failed-to-start-daemon :program)
   :exit-code (missing-required-initarg 'failed-to-start-daemon :exit-code)
   :options   (missing-required-initarg 'failed-to-start-daemon :options)
   :output    (missing-required-initarg 'failed-to-start-daemon :output))
  (:report
   (lambda (condition stream)
     (format stream "~@<Spread daemon (executable ~S) failed to start ~
                     with exit code ~D. Options were ~_~{~{~A = ~
                     ~S~}~^, ~_~}. ~_Spread said: ~&~A~@:>"
             (failed-to-start-daemon-program   condition)
             (failed-to-start-daemon-exit-code condition)
             (failed-to-start-daemon-options   condition)
             (failed-to-start-daemon-output    condition))))
  (:documentation
   "This error is signaled when an attempt to start the Spread daemon
    fails."))
