;;;; protocol.lisp --- Functions implementing the Spread protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.low-level)

;; TODO move to some other file?
(defstruct (mailbox
             (:constructor make-mailbox (socket stream private-group)))
  (socket        nil :type usocket:usocket     :read-only t)
  (stream        nil :type stream              :read-only t)
  (private-group nil :type simple-octet-vector :read-only t))

(defmethod print-object ((object mailbox) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A@~A"
            (sb-ext:octets-to-string (client-private-group object) :external-format :ascii) ; TODO make a function
            (usocket:get-peer-name (mailbox-socket object)))))

;;; Connection management

(declaim (ftype (function ((or null string) port-number
                           &key
                           (:name                   (or null simple-base-string))
                           (:membership?            boolean)
                           (:priority               (unsigned-byte 4)) ; TODO wire-protocol:connection-priority ?
                           (:authentication-methods list))
                          (values mailbox &optional))
                client-connect))
(setf (documentation 'client-connect 'function)
      "Connect to the Spread daemon designated by HOST and PORT.

       TODO")

(declaim (ftype (function (mailbox) (values &optional)) client-disconnect))
(setf (documentation 'client-disconnect 'function)
      "TODO")

(declaim (ftype (function (mailbox) (values simple-octet-vector &optional))
                client-private-group))
(setf (documentation 'client-private-group 'function)
      "TODO")

;;; Sending and receiving messages

(declaim (ftype (function (mailbox) (values boolean &optional))
                client-poll))
(setf (documentation 'client-poll 'function)
      "TODO")

(declaim (ftype (function (mailbox network.spread.wire-protocol::service-type
                           (or simple-octet-vector simple-vector)
                           network.spread.wire-protocol::message-type simple-octet-vector)
                          (values &optional))
                client-send/service-type))
(setf (documentation 'client-send/service-type 'function)
      "Send PAYLOAD in a message of type SERVICE-TYPE via MAILBOX to GROUPS.

       GROUPS TODO

       No values are returned.")

(declaim (ftype (function (mailbox (or simple-octet-vector simple-vector)
                           boolean network.spread.wire-protocol:message-type simple-octet-vector
                           quality-of-service)
                          (values &optional))
                client-send))
(setf (documentation 'client-send 'function)
      "Send PAYLOAD via MAILBOX to GROUPS with the given QUALITY-OF-SERVICE.

       QUALITY-OF-SERVICE is a value of type `quality-of-service'
       which specifies the ordering and reliability requirements of
       the message being sent.

       See `client-send/service-type' for a description of the
       remaining parameters.

       No values are returned.")

(declaim (ftype (function (mailbox simple-octet-vector array-index array-index t t)
                          (values message-kind
                                  (or null simple-octet-vector) (or null simple-octet-vector)
                                  network.spread.wire-protocol::message-type array-index ; TODO
                                  &optional))
                client-receive-into))
(setf (documentation 'client-receive-into 'function)
      "TODO")

;;; Group membership operations

(declaim (ftype (function (mailbox simple-octet-vector) (values &optional))
                client-join client-leave))
(setf (documentation 'client-join 'function)
      "The connection represented by MAILBOX joins Spread group GROUP."
      (documentation 'client-leave 'function)
      "The connection represented by MAILBOX leaves Spread group GROUP.")
