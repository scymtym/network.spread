;;;; method-null.lisp --- NULL authentication method.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.authentication)

(defclass method-null ()
  ()
  (:documentation
   "Complete authentication protocol without actual authentication."))

(service-provider:register-provider/class
 'authentication-method :null :class 'method-null)

(defmethod method-name ((method method-null))
  "NULL")

(defmethod authenticate ((method method-null) (stream stream)))
