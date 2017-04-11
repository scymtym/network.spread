;;;; conditions.lisp --- Conditions signaled by the network.spread system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;; TODO do we needs these? what should they do?

(define-condition spread-client-error (spread-error
                                       chainable-condition)
  ((code :initarg  :code
         :type     (or null keyword)
         :reader   spread-error-code
         :initform nil
         :documentation
         "Stores the keyword corresponding to the numeric error code
          returned by Spread."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Spread error~@[: ~A~]~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (spread-error-code condition)
             condition)))
  (:documentation
   "This class is used a superclass for error condition classes
    related to acting as a Spread client."))

(define-condition simple-spread-client-error (spread-client-error
                                              simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<~?~@[: ~A~]~/more-conditions:maybe-print-cause/~@:>"
             (simple-condition-format-control   condition)
             (simple-condition-format-arguments condition)
             (spread-error-code                 condition)
             condition)))
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
                     designated by ~S~@[: ~A~]~
                     ~/more-conditions:maybe-print-cause/.~@:>"
             (spread-error-name condition)
             (spread-error-code condition)
             condition)))
  (:documentation
   "This condition is signaled when establishing a connection to the
    Spread daemon fails."))

(define-condition message-too-long (spread-error)
  ((data        :initarg  :data
                :type     octet-vector
                :reader   message-too-long-data
                :documentation
                "The data that caused the error.")
   (group-count :initarg  :group-count
                :type     (or null (integer 0 #.+group-count-limit+))
                :reader   message-too-long-group-count
                :initform nil
                :documentation
                "If non-nil, the number of destination groups of the
                 message."))
  (:default-initargs
   :data (missing-required-initarg 'message-too-long :data))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o message-too-long- data group-count) condition))
       (format stream "~@<Data is ~:D octets long, which is longer than ~
                       the maximum message length ~:D~@[ allowed for a ~
                       message with ~D destination group~:P~].~@:>"
               (length data)
               (message-data-length-limit (or group-count 0))
               group-count))))
  (:documentation
   "This error is signaled when an attempt is made to send an
    octet-vector which is longer than permitted by the Spread
    protocol."))

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
             +group-name-length-limit+)))
  (:documentation
   "This error is signaled when a Spread group is specified which is
    longer than permitted by the Spread protocol."))
