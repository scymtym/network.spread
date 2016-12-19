;;;; package.lisp --- Package definition for unit tests of the base module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.base.test
  (:use
   #:cl
   #:let-plus
   #:fiveam

   #:network.spread.base)

  (:documentation
   "This package contains unit tests for the base module."))

(cl:in-package #:network.spread.base.test)

(def-suite :network.spread.base
  :in :network.spread
  :description
  "Test suite for the base module of the network.spread system.")
