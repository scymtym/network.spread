;;;; types.lisp --- Types used in the low-level module.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.low-level)

;;;

(deftype port-number ()
  '(unsigned-byte 16))

;;; Quality of service

(deftype quality-of-service ()
  `(member :unreliable :reliable :fifo :causal :agreed :safe))

(declaim (inline quality-of-service->regular-message-kind))
(defun quality-of-service->regular-message-kind (quality-of-service)
  (ecase quality-of-service
    (:unreliable network.spread.wire-protocol:+regular-message/unreliable+)
    (:reliable   network.spread.wire-protocol:+regular-message/reliable+)
    (:fifo       network.spread.wire-protocol:+regular-message/fifo+)
    (:causal     network.spread.wire-protocol:+regular-message/causal+)
    (:agreed     network.spread.wire-protocol:+regular-message/agreed+)
    (:safe       network.spread.wire-protocol:+regular-message/safe+)))

(declaim (inline maybe-add-self-discard))
(defun maybe-add-self-discard (service-type self-discard?)
  (if self-discard?
      (logior service-type
              network.spread.wire-protocol::+self-discard+)
      service-type))

;;; Messages kinds

(deftype message-kind ()
  '(member :regular :join :leave))

(defconstant +join-message+
  (logior network.spread.wire-protocol:+membership-message+
          network.spread.wire-protocol:+membership-message/caused-by-join+))

(defconstant +leave-message-mask+
  (logior network.spread.wire-protocol:+membership-message+
          network.spread.wire-protocol:+membership-message/caused-by-leave+
          network.spread.wire-protocol:+membership-message/caused-by-disconnect+))

(declaim (inline service-type->message-kind))
(defun service-type->message-kind (service-type)
  (cond
    ((network.spread.wire-protocol::service-type-regular-message? service-type)
     :regular)
    ((= service-type +join-message+)
     :join)
    ((plusp (logand service-type +leave-message-mask+))
     :leave)))
