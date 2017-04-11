;;;; conditions.lisp --- Conditions signaled by the wire-protocol module.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol)

(define-condition wire-protocol-condition (condition)
  ((context :initarg :context
            :reader  wire-protocol-condition-context))
  (:default-initargs
   :context (missing-required-initarg 'wire-protocol-condition :context))
  (:documentation
   "TODO"))

(define-condition wire-protocol-error (network.spread.base:spread-error
                                       stream-error
                                       wire-protocol-condition)
  ())

;;; Communication errors

(define-condition communication-error (wire-protocol-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Communication with the Spread daemon ~
                     failed.~/more-conditions:maybe-print-cause/~@:>"
             condition)))
  (:documentation
   "TODO"))

(define-condition generic-communication-error (communication-error ; TODO better name
                                               chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<~:[Communication~;~:*When ~A, communication~] ~
                     with the Spread daemon ~
                     failed.~/more-conditions:maybe-print-cause/~@:>"
             (wire-protocol-condition-context condition)
             condition)))
  (:documentation
   "TODO"))

(define-condition short-read-error (communication-error)
  ((expected-count :initarg  :expected-count
                   :reader   short-read-error-expected-count
                   :documentation
                   "Stores the number of expected octets.")
   (received       :initarg  :received
                   :reader   short-read-error-received
                   :initform nil
                   :documentation
                   "Stores a sequence containing the actually received
                    octets."))
  (:default-initargs
   :expected-count (missing-required-initarg 'short-read-error :expected-count))
  (:report
   (lambda (condition stream)
     (let+ (((&accessors-r/o (context        wire-protocol-condition-context)
                             (expected-count short-read-error-expected-count)
                             (received       short-read-error-received))
             condition))
       (format stream "~@<~:[Could~;~:*When ~A, could~] ~
                       ~[~
                         ~*not receive any data ~
                       ~:;~
                         ~:*only read ~:D octet~:P:~@:_~
                         ~,,,16:/utilities.binary-dump:print-binary-dump/~@:_~
                       ~]~
                       of ~:D required octet~:P.~@:>"
               context (length received) (coerce received 'vector)
               expected-count))))
  (:documentation
   "TODO"))

;;; Protocol errors
;;;
;;; These errors are signaled if the communication itself succeeds but
;;; the transmitted messages violate the protocol.

(define-condition protocol-error (wire-protocol-error)
  ()
  (:documentation
   "Superclass for protocol-related conditions."))

(define-condition failure-result-error (protocol-error)
  ((code :initarg  :code
         :type     (integer -128 -1)
         :reader   failure-result-error-code
         :documentation
         "The error code transmitted by the remote end."))
  (:default-initargs
   :code (missing-required-initarg 'failure-result-error :code))
  (:report
   (lambda (condition stream)
     (let* ((context     (wire-protocol-condition-context condition))
            (code        (failure-result-error-code condition))
            (result      (find code +results+ :key #'result-code)) ; TODO make a function which also handle the unknown code case
            (description (if result
                             (result-description result)
                             "An unknown error")))
       (format stream "~@<~:[The~;~:*When ~A, the~] Spread daemon ~
                       responded with ~S (code ~D).~@:>"
               context description code))))
  (:documentation
   "Signaled if the remote end indicates a failed operation."))

(define-condition incompatible-daemon-error (protocol-error)
  ((actual-version     :initarg :actual-version
                       :reader  actual-version
                       :type    list)
   (supported-versions :initarg :supported-versions
                       :reader  supported-versions))
  (:default-initargs
   :actual-version    (missing-required-initarg 'incompatible-daemon-error
                                                :actual-version)
   :supported-version (missing-required-initarg 'incompatible-daemon-error
                                                :supported-version))
  (:report
   (lambda (condition stream)
     (format stream "~@<The daemon version is ~{~D~^.~} which is not ~
                     in the range of supported versions, that is ~@?.~@:>"
             (actual-version condition)
             (supported-versions condition))))
  (:documentation
   "Signaled when daemon version is incompatible with this client.

    The daemon version is transmitted from the daemon to the client
    during the process of establishing a connection."))
