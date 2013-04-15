;;;; protocol.lisp --- Protocol of the Common Lisp spread bindings.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defgeneric connect (daemon)
  (:documentation
   "Connect to the spread daemon designated by DAEMON."))

(defgeneric disconnect (connection)
  (:documentation
   "Explicitly disconnect CONNECTION from the spread daemon to which
it is connected."))

(defgeneric join (connection group)
  (:documentation
   "Make CONNECTION a member of the spread group designated by
GROUP."))

(defgeneric leave (connection group)
  (:documentation
   "Remove CONNECTION from the spread group designated by GROUP."))

(defgeneric send (connection destination data)
  (:documentation
   "Send DATA to the spread group designated by DESTINATION within the
spread session in which CONNECTION participates."))

(defgeneric send-bytes (connection destination data)
  (:documentation
   "Like `send', but data has to be of type `octet-vector' and other
checks are also omitted."))

(defgeneric receive-into (connection buffer
                          &key
                          start end
                          block?
                          return-sender? return-groups?)
  (:documentation
   "Receive data that is send to any spread group in which CONNECTION
is a member into BUFFER. Return three values: 1. the number of
received octets 2. the name of the sender of the received message 3. a
list of names of the groups to which the received message has been
sent.

START and END can be used to specify a subsequence of BUFFER into
which data should be received.

If BLOCK? is non-nil the call blocks until a message has been
received. Otherwise, the call returns immediately and may return nil
if no message has been received.

RETURN-SENDER? and RETURN-GROUPS? control whether the sender and/or
group names should be extracted from the received message."))

(defgeneric receive (connection
                     &key
                     block?
                     return-sender? return-groups?)
  (:documentation
   "Receive and return data that is send to any spread group in which
CONNECTION is a member. Return three values: 1. an
`nibbles:simple-octet-vector' containing the received data 2. the name
of the sender of the received message 3. a list of names of the groups
to which the received message has been sent.

If BLOCK? is non-nil the call blocks until a message has been
received. Otherwise, the call returns immediately and may return nil
if no message has been received.

RETURN-SENDER? and RETURN-GROUPS? control whether the sender and/or
group names should be extracted from the received message."))
