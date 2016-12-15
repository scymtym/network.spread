;;;; package.lisp --- Package definition for unit tests of the low-level module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.low-level.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:nibbles
   #:fiveam

   #:network.spread.low-level)

  (:import-from #:network.spread.base
   #:ascii-to-octets)

  (:import-from #:network.spread.wire-protocol
   #:+regular-message/unreliable+
   #:+regular-message/fifo+)

  (:import-from #:network.spread.low-level
   #:make-mailbox)

  (:import-from #:network.spread.wire-protocol.test
   #:coerce-to-group-name
   #:coerce-to-group-names)

  (:documentation
   "This package contains unit tests for the low-level module."))

(cl:in-package #:network.spread.low-level.test)

(def-suite :network.spread.low-level
  :in :network.spread
  :description
  "Unit test suite for the low-level module.")
