;;;; package.lisp --- Package definition for unit tests of the wire-protocol module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.wire-protocol.test
  (:use
   #:cl
   #:fiveam

   #:network.spread.wire-protocol)

  (:documentation
   "This package contains unit tests for the wire-protocol module."))

(cl:in-package #:network.spread.wire-protocol.test)

(def-suite :network.spread.wire-protocol
  :in :network.spread
  :description
  "Unit test suite for the wire-protocol module.")
