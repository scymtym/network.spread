;;;; package.lisp --- Package definition network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.low-level
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:nibbles)

  ;; Types
  (:export
   #:quality-of-service

   #:message-kind)

  ;; Client protocol functions
  (:export
   #:client-connect
   #:client-disconnect

   #:client-private-group

   #:client-poll
   #:client-send/service-type #:client-send
   #:client-receive-into

   #:client-join
   #:client-leave)

  (:documentation
   "A network-enabled implementation of the wire protocol used by Spread.

    Messages can be transported via either UNIX domain sockets or TCP
    sockets."))
