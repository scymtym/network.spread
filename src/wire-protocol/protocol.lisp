;;;; protocol.lisp --- Functions implementing the Spread wire protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol)

;;; Connection management

(declaim (ftype (function (stream (or null simple-octet-vector) boolean connection-priority list)
                          (values simple-octet-vector cons &optional))
                client-connect))
(setf (documentation 'client-connection 'function)
      "Perform initial handshake and authentication for the connection
       represented by STREAM.

       NAME is either `nil' or the desired \"private name\" for this
       connection.

       The Boolean MEMBERSHIP? controls whether this connection should
       receive membership messages.

       PRIORITY is currently ignored (by the Spread daemon).

       AUTHENTICATION-METHODS is a list of authentication methods as
       provided by the network.spread.authentication package.

       Return two values:

       1) the \"private name\" assigned to this connection by the
          Spread daemon

       2) the version of the Spread daemon as a list of the
          form (MAJOR MINOR PATCH)

       An error of type `failure-result-error' is signaled if the
       daemon rejects the connection attempt.")

(Declaim (ftype (function (stream list) (values &optional))
                client-authenticate))
(setf (documentation 'client-authenticate 'function)
      "Perform authentication using METHOD for the connection
       represented by STREAM.

       METHODS is proposed to the Spread daemon and executed
       sequentially, if accepted.

       An error of type
       `network.spread.authentication:method-rejected-error' is
       signaled if the daemon does not accept the proposed list
       METHODS.

       An error of type
       `network.spread.authentication:authentication-failed-error' is
       signaled if a method in METHODS fails or the daemon rejects the
       authentication attempt.")

(declaim (ftype (function (stream simple-octet-vector) (values &optional))
                client-disconnect))
(setf (documentation 'client-disconnect 'function)
      "Write a termination message to STREAM.")

;;; Sending and receiving messages

(declaim (ftype (function (stream simple-octet-vector service-type group-names
                           message-type simple-octet-vector)
                          (values &optional))
                client-send))
(setf (documentation 'client-send 'function)
      "Write a message of type SERVICE-TYPE, addressed to GROUPS to STREAM.

       PRIVATE-GROUP

       SERVICE-TYPE

       GROUPS is either a `simple-octet-vector' containing one group
       name, a `simple-octet-vector' containing multiple concatenated
       and appropriately padded group names or a `simple-vector'
       containing multiple group names as `simple-octet-vector's

       MESSAGE-TYPE can be used arbitrarily by the client.

       PAYLOAD ")

(declaim (ftype (function (stream simple-octet-vector (or null array-index) (or null array-index) t t) ; TODO types
                          (values service-type
                                  (or null group-name-vector) (or null simple-octet-vector)
                                  message-type (message-data-length 1) &optional))
                client-receive-into))
(setf (documentation 'client-receive-into 'function)
      "Receive a message on the connection represented by STREAM.

       Return five values:

       1) service-type

       2) private-group

       3) groups

       4) message-type

       5) payload-length")

;;; Group membership operations

(declaim (ftype (function (stream simple-octet-vector simple-octet-vector)
                          (values &optional))
                client-join client-leave))
(setf (documentation 'client-join 'function)
      "The connection represented by STREAM should join the group GROUP."
      (documentation 'client-leave 'function)
      "The connection represented by STREAM should leave the group GROUP.")
