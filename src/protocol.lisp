;;;; protocol.lisp --- Protocol of the Common Lisp spread bindings.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defgeneric connect (daemon)
  (:documentation
   "Connect to the Spread daemon designated by DAEMON.

    If the connection attempt succeeds, a `connection' instance is
    returned."))

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
    is a member into BUFFER.

    Return three values: 1. the number of received octets 2. the name
    of the sender of the received message 3. a list of names of the
    groups to which the received message has been sent.

    START and END can be used to specify a subsequence of BUFFER into
    which data should be received.

    ")) ; See `+receive*-shared-documentation+' below

(defgeneric receive (connection
                     &key
                     block?
                     return-sender? return-groups?)
  (:documentation
   "Receive and return data that is send to any spread group in which
    CONNECTION is a member.

    Return three values: 1. an `nibbles:simple-octet-vector'
    containing the received data 2. the name of the sender of the
    received message 3. a list of names of the groups to which the
    received message has been sent.

    ")) ; See `+receive*-shared-documentation+' below

(define-constant +receive*-shared-documentation+
    "If BLOCK? is non-nil the call blocks until a message has been
     received.

     Otherwise, the call returns immediately and may return nil if no
     message has been received.

     RETURN-SENDER? and RETURN-GROUPS? control whether the sender
     and/or group names should be extracted from the received
     message. For both parameters, valid values are of type
     `return-aspect-switch', i.e.:

     * nil              Never return sender/groups.
     * t                Always return sender/groups.

     * :when-membership Return sender/groups for membership messages
                        but not for regular messages."
  :test #'string=)

(dolist (function '(receive-into receive))
  (let ((current (documentation function 'function)))
   (unless (ends-with-subseq +receive*-shared-documentation+ current)
     (setf (documentation function 'function)
           (concatenate 'string current +receive*-shared-documentation+)))))
