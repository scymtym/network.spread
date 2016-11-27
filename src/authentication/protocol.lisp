;;;; protocol.lisp --- Protocol provided by the authentication module.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.authentication)

;;; Authentication method protocol

(defgeneric method-name (method)
  (:documentation
   "Return a string that is the name of METHOD."))

;;; Authentication protocol

(defgeneric validate (method accepted &key if-invalid)
  (:documentation
   "Determine whether METHOD is in the accepted methods ACCEPTED.

    METHOD can be an authentication method or a sequence of
    authentication methods.

    IF-INVALID controls the behavior in case METHOD or some elements
    of METHOD are not accepted. The following values are allowed:

    'error, #'error

      Signal a `method-rejected-error' using `error'.

    ANY-OTHER-VALUE

      Return ANY-OTHER-VALUE. "))

(defgeneric authenticate (method stream)
  (:documentation
   "Perform authentication according to METHOD on STREAM.

    If METHOD is of type `sequence', its elements are authentication
    methods which all have to perform successful authentications.

    METHOD uses STREAM to send and receive data to/from the peer with
    which the authentication is performed."))

;;; Default behavior

(defmethod validate ((method standard-object) (accepted sequence)
                     &rest args &key if-invalid)
  (declare (ignore if-invalid))
  (apply #'validate (ensure-list method) accepted args))

(defmethod validate ((method sequence) (accepted sequence)
                     &key (if-invalid #'error))
  (log:debug "~@<Peer accepts authentication method~P: ~{~S~^, ~}~@:>"
             (length accepted) accepted)
  (let+ (((&flet accepted? (method)
            (find (method-name method) accepted :test #'string=))))
    (if-let ((offenders (remove-if #'accepted? method)))
      (error-behavior-restart-case (if-invalid
                                    (method-rejected-error
                                     :requested (coerce method 'list)
                                     :accepted  (coerce accepted 'list))))
      method)))

(defmethod authenticate :around ((method standard-object) (stream t))
  (tagbody
   :start
     (log:debug "~@<Authenticating with method ~A~@:>" method)
     (with-condition-translation (((error authentication-failed-error)
                                   :method method))
       (restart-case
           (return-from authenticate (call-next-method))
         (retry ()
           :report (lambda (stream)
                     (format stream "~@<Retry authenticating with ~
                                     method ~A.~@:>"
                             method))
           (go :start))))))

(defmethod authenticate ((method sequence) (stream t))
  (map (class-of method)
       (lambda (method)
         (restart-case
             (authenticate method stream)
           (skip ()
             :report (lambda (stream)
                       (format stream "~@<Skip authentication ~
                                       method ~A.~@:>"
                               method)))))
       method))

;;; Authentication service

(service-provider:define-service authentication-method
  (:documentation
   "Providers of this service are authentication methods."))

(defun make-authentication-method (name &rest initargs)
  (apply #'service-provider:make-provider
         'authentication-method name initargs))
