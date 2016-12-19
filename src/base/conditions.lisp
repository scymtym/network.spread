;;;; conditions.lisp --- Conditions used by the base module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.base)

(define-condition spread-error (error)
  ()
  (:documentation
   "This class is used as a superclass for Spread-related error
    condition classes."))

(define-condition daemon-name-syntax-error (spread-error
                                            chainable-condition)
  ((string :initarg :string
           :type    string
           :reader  daemon-name-syntax-error-string
           :documentation
           "Stores the offending string."))
  (:report
   (lambda (condition stream)
     (format stream "~@<~S is not a valid Spread daemon ~
                     name.~/more-conditions:maybe-print-cause/~@:>"
             (daemon-name-syntax-error-string condition)
             condition)))
  (:documentation
   "This error is signal for syntax errors in Spread daemon names."))
