;;;; package.lisp --- Package definition for unit tests of the wire-protocol module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.wire-protocol.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:nibbles

   #:fiveam

   #:network.spread.wire-protocol)

  (:import-from #:network.spread.base
   #:ascii-to-octets)

  (:import-from #:network.spread.wire-protocol
   #:+authentication-data-length-limit+)

  (:import-from #:network.spread.test
   #:mock-server-coroutine)

  (:documentation
   "This package contains unit tests for the wire-protocol module."))

(cl:in-package #:network.spread.wire-protocol.test)

(def-suite :network.spread.wire-protocol
  :in :network.spread
  :description
  "Unit test suite for the wire-protocol module.")
