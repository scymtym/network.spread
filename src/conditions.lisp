;;;; conditions.lisp --- Conditions signaled by the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(define-condition spread-error (error)
  ()
  (:documentation
   "This class is used a superclass for Spread-related error condition
    classes."))

(define-condition spread-client-error (spread-error)
  ((code :initarg  :code
         :type     keyword
         :reader   spread-error-code
         :documentation
         "Stores the keyword corresponding to the numeric error code
          returned by Spread."))
  (:default-initargs
   :code (missing-required-initarg 'spread-client-error :code))
  (:report
   (lambda (condition stream)
     (format stream "~@<Spread error: ~A.~@:>"
             (spread-error-code condition))))
  (:documentation
   "This class is used a superclass for error condition classes
    related to acting as a Spread client."))

(define-condition simple-spread-client-error (spread-client-error
                                              simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~?: ~A"
             (simple-condition-format-control   condition)
             (simple-condition-format-arguments condition)
             (spread-error-code                 condition))))
  (:documentation
   "Condition instances of this class contain a simple problem
    description and a Spread error code."))

(define-condition connect-failed (spread-client-error)
  ((name :initarg  :name
         :type     string
         :reader   spread-error-name
         :documentation
         "Stores the name of the Spread daemon to which a connection
          has been attempted."))
  (:default-initargs
   :name (missing-required-initarg 'connect-failed :name))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to connect to the spread daemon ~
                     designated by ~S: ~A.~@:>"
             (spread-error-name condition)
             (spread-error-code condition))))
  (:documentation
   "This condition is signaled when establishing a connection to the
    Spread daemon fails."))

(define-condition message-too-long (spread-error)
  ((data :initarg  :data
         :type     octet-vector
         :reader   message-too-long-data
         :documentation
         "The data that caused the error."))
  (:default-initargs
   :data (missing-required-initarg 'message-too-long :data))
  (:report
   (lambda (condition stream)
     (format stream "~@<Data is ~:D octets long, which is longer than ~
                     the maximum message length ~:D.~@:>"
             (length (message-too-long-data condition))
             +maximum-message-data-length+)))
  (:documentation
   "This error is signaled when an attempt is made to send an
    octet-vector which has more than `+maximum-message-data-length+'
    octets."))

(define-condition group-too-long-error (spread-error)
  ((group :initarg  :group
          :type     octet-vector
          :reader   group-too-long-error-group
          :documentation
          "The group name that caused the error."))
  (:default-initargs
   :group (missing-required-initarg 'group-too-long-error :group))
  (:report
   (lambda (condition stream)
     (format stream "~@<Group name is ~:D octets long, which is longer
                     than the maximum group name length ~:D.~@:>"
             (length (group-too-long-error-group condition))
             +maximum-group-name-length+)))
  (:documentation
   "This error is signaled when a Spread group is specified which has
    more than `+maximum-group-name-length+' octets."))

;;; Daemon-related conditions

(define-condition failed-to-start-daemon (spread-error)
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
