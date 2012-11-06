;;;; protocol.lisp --- Protocol of the Common Lisp spread bindings.
;;;;
;;;; Copyright (C) 2011, 2012 Jan Moringen
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

(defgeneric receive (connection
		     &key
		     block?)
  (:documentation
   "Receive and return data that is send to any spread group in which
CONNECTION is a member. If block? is non-nil the call blocks until a
message has been received. Otherwise, the call returns immediately and
may return nil if no message has been received."))
