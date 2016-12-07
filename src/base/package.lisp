;;;; package.lisp --- Package definition for the base module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.base
  (:use
   #:cl
   #:let-plus)

  ;; Conditions
  (:export
   #:spread-error)

  ;; Restarts
  (:export
   #:retry
   #:use-daemon)

  ;; Utilities
  (:export
   #:parse-daemon-name

   #:ascii-to-octets
   #:octets-to-ascii)

  (:documentation
   "Basic functionality used by the other modules."))
