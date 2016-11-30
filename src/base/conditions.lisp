;;;; conditions.lisp --- Conditions used by the base module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.base)

(define-condition spread-error (error)
  ()
  (:documentation
   "This class is used as a superclass for Spread-related error
    condition classes."))
