;;;; package.lisp --- Package definition for unit tests of the network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:network.spread)

  (:import-from #:network.spread
   #:+max-group-name+)

  (:export
   #:root)

  (:documentation
   "This package contains unit tests for the network.spread system"))

(cl:in-package #:network.spread.test)

(deftestsuite root ()
  (port
   daemon)
  (:setup
   (setf port   (asdf:component-property
		 (asdf:find-system :network.spread-test) :port)
	 daemon (format nil "~D" port)))
  (:timeout 20)
  (:documentation
   "Root unit test suite for the network.spread system."))
