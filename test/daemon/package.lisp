;;;; package.lisp --- Package definition for unit tests of the daemon module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.daemon.test
  (:use
   #:cl
   #:fiveam

   #:network.spread.daemon)

  (:import-from #:network.spread.test
   #:*port*)

  (:documentation
   "This package contains unit tests for the daemon module."))

(cl:in-package #:network.spread.daemon.test)

(def-suite :network.spread.daemon
  :in :network.spread
  :description
  "Test suite for the daemon module of the network.spread system.")
