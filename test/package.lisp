;;;; package.lisp --- Package definition for unit tests of the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:fiveam

   #:nibbles

   #:network.spread)

  ;; Test utilities
  (:export
   #:mock-server-coroutine)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the network.spread system"))

(cl:in-package #:network.spread.test)

(def-suite :network.spread
  :description
  "Root unit test suite for the network.spread system.")

(defparameter *port* network.spread-system:*test-port*)

(defparameter *daemon* (format nil "~D" *port*))

(defun run-tests ()
  (run! :network.spread))
