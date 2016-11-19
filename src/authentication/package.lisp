;;;; package.lisp --- Package definition for the authentication module.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.authentication
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:import-from #:network.spread.base
   #:retry)

  ;; Conditions
  (:export
   #:authentication-error

   #:method-rejected-error
   #:method-rejected-error-requested
   #:method-rejected-error-accepted

   #:authentication-failed-error
   #:authentication-failed-error-method)

  ;; Restarts
  (:export
   #:retry)

  ;; Authentication method protocol
  (:export
   #:method-name)

  ;; Authentication protocol
  (:export
   #:validate
   #:authenticate)

  ;; Authentication service
  (:export
   #:authentication-method
   #:make-authentication-method)

  (:documentation
   "This package contains an implementation of the client role of
    Spread's authentication mechanism."))
