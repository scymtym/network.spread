;;;; conditions.lisp --- Conditions used by the authentication module.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:network.spread.authentication)

(defun print-methods (stream methods &optional colon? at?)
  (declare (ignore colon? at?))
  (format stream "method~P~
                  ~@:_~@:_~
                  ~{+ ~S~^~@:_~}~
                  ~@:_~@:_"
          (length methods) methods))

(define-condition authentication-error (error)
  ()
  (:documentation
   "Superclass for authentication-related errors."))

(define-condition method-rejected-error (authentication-error)
  ((requested :initarg :requested
              :type    list
              :reader  method-rejected-error-requested
              :documentation
              "Stores a list of the names of the requested
               authentication methods.")
   (accepted  :initarg :accepted
              :type    list
              :reader  method-rejected-error-accepted
              :documentation
              "Stores a list of the names of the accepted
               authentication methods."))
  (:default-initargs
   :requested (missing-required-initarg 'method-rejected-error :requested)
   :accepted  (missing-required-initarg 'method-rejected-error :accepted))
  (:report
   (lambda (condition stream)
     (let ((requested (method-rejected-error-requested condition))
           (accepted  (method-rejected-error-accepted  condition)))
       (format stream "~@<The authentication ~
                       ~/network.spread.authentication::print-methods/~
                       ~[~;has~:;have~] been rejected by the ~
                       server. The server ~
                       ~[~
                         does not accept any authentication methods~
                       ~:;~
                         only accepts the following authentication ~
                         ~/network.spread.authentication::print-methods/~
                       ~].~@:>"
               (mapcar #'method-name requested) (length requested)
               (length accepted) accepted))))
  (:documentation
   "This error is signaled when suggested authentication methods are rejected.

    In particular, in the context of the Spread protocol the client
    suggests a list of authentication methods to the server which the
    later either accepts or rejects."))

(define-condition authentication-failed-error (authentication-error
                                               chainable-condition)
  ((method :initarg :method
           :reader  authentication-failed-error-method
           :documentation
           "Stores the authentication method or methods that have been
            used in the failed authentication attempt."))
  (:default-initargs
   :method (missing-required-initarg 'authentication-failed-error :method))
  (:report
   (lambda (condition stream)
     (let ((method (authentication-failed-error-method condition)))
       (format stream "~@<Authentication with ~
                       ~/network.spread.authentication::print-methods/~
                       failed.~/more-conditions:maybe-print-cause/~@:>"
               (mapcar #'method-name (ensure-list method))
               condition))))
  (:documentation
   "This error is signaled when an authentication attempt fails."))
