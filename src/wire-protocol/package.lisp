;;;; package.lisp --- Package definition network.spread system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.wire-protocol
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:nibbles)

  (:import-from #:network.spread.base
   #:ascii-to-octets
   #:octets-to-ascii)

  ;; Constants and variables
  (:export
   #:*major-version*
   #:*minor-version*
   #:*patch-version*

   #:+group-name-length-limit+
   #:+group-count-limit+

   #:+authentication-method-name-length-limit+
   #:+authentication-method-count-limit+

   #:message-data-length-limit)

  ;; Types
  (:export
   #:group-name-length
   #:group-count
   #:group-data-length
   #:group-name-vector
   #:group-names

   #:service-type
   #:message-type
   #:message-data-length)

  ;; Message bitfields
  (:export
   #:+regular-message/unreliable+
   #:+regular-message/reliable+
   #:+regular-message/fifo+
   #:+regular-message/causal+
   #:+regular-message/agreed+
   #:+regular-message/safe+
   #:+regular-message-mask+

   #:+self-discard+

   #:+membership-message+
   #:+membership-message/caused-by-join+
   #:+membership-message/caused-by-leave+
   #:+membership-message/caused-by-disconnect+
   #:+membership-message/caused-by-network+
   #:+membership-message-mask+

   #:+command-message/join+
   #:+command-message/leave+
   #:+command-message/kill+
   #:+command-message/groups+
   #:+command-message-mask+

   #:+reject-message+)

  ;; Conditions
  (:export
   #:communication-error

   #:protocol-error

   #:failure-result-error
   #:failure-result-error-code

   #:incompatible-daemon-error)

  ;; Client protocol functions
  (:export
   #:client-connect
   #:client-disconnect

   #:client-authenticate

   #:client-send
   #:client-receive-into

   #:client-join
   #:client-leave)

  (:documentation
   "An implementation of the wire protocol used by Spread.

    Only the client role of the protocol is implemented.

    On this level, all string-like data is represented as
    `nibbles:simple-octet-vector' objects."))
