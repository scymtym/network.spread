;;;; package.lisp --- Package definition for tests of the authentication module.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.authentication.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions
   #:fiveam

   #:network.spread.authentication)

  (:shadowing-import-from #:network.spread.authentication
   #:skip)

  (:documentation
   "This package contains unit tests for the authentication module."))

(cl:in-package #:network.spread.authentication.test)

(def-suite :network.spread.authentication
  :in :network.spread
  :description
  "Unit test suite for the authentication module.")

;;; `mock' authentication method

(defclass method-mock ()
  ((name     :initarg  :name
             :reader   method-name)
   (behavior :initarg  :behavior
             :accessor method-behavior
             :initform :succeed))
  (:default-initargs
   :name (missing-required-initarg 'method-mock :name)))

(service-provider:register-provider/class
 'authentication-method :mock :class 'method-mock)

(defmethod authenticate ((method method-mock) (stream stream))
  (ecase (method-behavior method)
    (:succeed
     :success)
    (:fail
     (error "~@<Intentional authentication error.~@:>"))
    (:fail-once
     (setf (method-behavior method) :succeed)
     (error "~@<Intentional authentication error.~@:>"))))
