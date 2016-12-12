;;;; package.lisp --- Package definition for unit tests of the wire-protocol module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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

  (:import-from #:network.spread.authentication
   #:method-rejected-error

   #:authentication-failed-error

   #:make-authentication-method)

  (:import-from #:network.spread.wire-protocol
   #:+authentication-data-length-limit+

   #:+regular-message/unreliable+
   #:+regular-message/fifo+)      ; TODO export these?

  (:import-from #:network.spread.test
   #:mock-server-coroutine)

  (:documentation
   "This package contains unit tests for the wire-protocol module."))

(cl:in-package #:network.spread.wire-protocol.test)

(def-suite :network.spread.wire-protocol
  :in :network.spread
  :description
  "Unit test suite for the wire-protocol module.")

;;; Test utilities

(defun maybe-ascii-to-octets (thing)
  (etypecase thing
    (octet-vector thing)
    (string       (ascii-to-octets thing))))

(defun pad-sequence (sequence length)
  (let ((remainder (nth-value 1 (ceiling (length sequence) length))))
    (if (zerop remainder)
        sequence
        (concatenate 'octet-vector sequence (make-octet-vector (- remainder))))))

(defun coerce-to-group-name (thing)
  (pad-sequence (maybe-ascii-to-octets thing) +group-name-length-limit+))

(defun coerce-to-group-names (sequence)
  (apply #'concatenate 'octet-vector
         (mapcar #'coerce-to-group-name sequence)))
