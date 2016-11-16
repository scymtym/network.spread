;;;; types.lisp --- Types used in the Spread wire protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol)

;;; Groups

(deftype group-name-length ()
  `(integer 0 ,+group-name-length-limit+))

(deftype group-count ()
  `(integer 0 ,+group-count-limit+))

(deftype group-data-length ()
  `(integer 0 ,+group-data-length-limit+))

(deftype group-name-vector ()
  `(simple-octet-vector ,+group-name-length-limit+))

(deftype group-names ()
  `(or simple-octet-vector simple-vector))

;;; Connection

(deftype connection-priority ()
  'bit)

;;; Messages

(deftype service-type ()
  '(unsigned-byte 32))

(declaim (inline service-type-regular-message?
                 service-type-membership-message?))

(defun service-type-regular-message? (service-type)
  (plusp (logand service-type +regular-message-mask+)))

(defun service-type-membership-message? (service-type)
  (plusp (logand service-type +membership-message-mask+)))

(deftype message-type ()
  '(unsigned-byte 16))

(deftype message-data-length (&optional (max-group-count 0))
  (check-type max-group-count (integer 0 #.+group-count-limit+))
  `(integer 0 ,(message-data-length-limit max-group-count)))
