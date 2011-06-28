;;; connection.lisp --- Class representing connections to the Spread network.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :spread)

(defclass connection ()
  ((handle      :initarg  :handle
		:type     integer
		:documentation
		"The handle of this connection as assigned by the
Spread daemon.")
   (daemon-name :initarg  :daemon-name
		:type     string
		:reader  connection-daemon-name
		:documentation
		"The name of the Spread daemon to which this
connection is connected.")
   (name        :initarg  :name
		:type     string
		:reader   connection-name
		:documentation
		"The unique name of this connection within the Spread
segment.")
   (groups      :initarg  :groups
		:type     list
		:reader   connection-groups
		:initform nil
		:documentation
		"The list of groups this connection is a member of.")
   (join-hook   :initarg  :join-hook
		:type     list
		:initform nil
		:documentation
		"This hook is run when a Spread client joins one of the
groups this connection is a member of.")
   (leave-hook  :initarg  :leave-hook
		:type     list
		:initform nil
		:documentation
		"This hook is run when a Spread client leaves one of
the groups this connection is a member of."))
  (:documentation
   "Instances of this class represent connections to Spread
segments. Each connection can participate in zero or more Spread
groups. Group membership is required for receiving messages addressed
at groups, but not for sending messages to groups."))

(defmethod initialize-instance :after ((instance connection) &key)
  (let ((handle (slot-value instance 'handle)))
    (tg:finalize instance #'(lambda () (%disconnect handle)))))

(defmethod disconnect ((connection connection))
  (tg:cancel-finalization connection)
  (%disconnect (slot-value connection 'handle)))

(defmethod join ((connection connection) (group string))
  (%join (slot-value connection 'handle) group))

(defmethod join :after ((connection connection) (group string))
  (pushnew group (slot-value connection 'groups) :test #'string=))

(defmethod join ((connection connection) (group list))
  (map 'nil (curry #'join connection) group))

(defmethod leave ((connection connection) (group string))
  (%leave (slot-value connection 'handle) group))

(defmethod leave :after ((connection connection) (group string))
  (removef (slot-value connection 'groups) group :test #'string=))

(defmethod leave ((connection connection) (group list))
  (map nil (curry #'leave connection) group))

(defmethod leave ((connection connection) (group (eql t)))
  (leave connection (slot-value connection 'groups)))

(defmethod receive :around ((connection connection)
			    &key
			    (block? t))
  (declare (ignore block?))

  (if *incoming-stream*
      (bind (((:values buffer sender recipients) (call-next-method)))
	(format *incoming-stream* "~@<~{~2,'0X~^ ~}~@:>"
		(coerce buffer 'list))
	(values buffer sender recipients))
      (call-next-method)))

(defmethod receive ((connection connection)
		    &key
		    (block? t))
  ;; Do not enter/break out of loop when non-blocking and no messages
  ;; queued.
  (iter (while (or block? (%poll (slot-value connection 'handle))))
	;; Receive next message, blocking if necessary.
	(for message next (%receive (slot-value connection 'handle)))
	;; Return regular messages, run hooks for membership messages.
	(if (eq (first message) :regular)
	    (return (values-list (rest message)))
	    (bind (((type group members) message)
		   (hook (case type
			   (:join  'join-hook)
			   (:leave 'leave-hook))))
	      (when hook
		(run-hook (object-hook connection hook)
			  group members))))))

(defmethod send-bytes :around ((connection  connection)
			       (destination t)
			       (data        simple-array))
  (when (and *outgoing-stream*
	     (typep data 'octet-vector))
    (format *outgoing-stream* "~@<~{~2,'0X~^ ~}~@:>"
	    (coerce data 'list)))

  (unless (<= (length data) +maximum-message-data-length+)
    (error 'message-too-long
	   :data data))

  (call-next-method))

(defmethod send-bytes ((connection  connection)
		       (destination string)
		       (data        simple-array))
  (%send-one (slot-value connection 'handle) destination data))

(defmethod send-bytes ((connection  connection)
		       (destination list)
		       (data        simple-array))
  (%send-multiple (slot-value connection 'handle) destination data))

(defmethod send ((connection  connection)
		 (destination string)
		 (data        simple-array))
  (check-type data octet-vector)

  (send-bytes connection destination data))

(defmethod send ((connection  connection)
		 (destination list)
		 (data        simple-array))
  (check-type data octet-vector)

  (if (length= 1 destination)
      (send-bytes connection (first destination) data)
      (send-bytes connection destination data)))

(defmethod send ((connection  connection)
		 (destination string)
		 (data        string))
  (send-bytes connection destination (sb-ext:string-to-octets data)))

(defmethod send ((connection  connection)
		 (destination list)
		 (data        string))
  (let ((octets (sb-ext:string-to-octets data)))
    (if (length= 1 destination)
	(send-bytes connection (first destination) octets)
	(send-bytes connection destination octets))))

(defmethod print-object ((object connection) stream)
  (with-slots (handle name groups) object
    (print-unreadable-object (object stream :type t)
      (format stream "~A (~D) #~D"
	      name (length groups) handle))))


;;;
;;

(defmethod connect ((daemon string))
  "Connect to the spread segment designated by DAEMON. If the
connection attempt succeeds, a `connection' instance is returned. "
  (bind (((:values handle name) (%connect daemon :membership? t)))
    (make-instance 'connection
		   :handle      handle
		   :daemon-name daemon
		   :name        name)))

(defmethod connect :around ((daemon string))
  "Install restarts around the connection attempt."
  (let (result)
    (tagbody
     retry
       (restart-case
	   (setf result (call-next-method daemon))
	 (retry ()
	   :report (lambda (stream)
		     (format stream "~@<Retry connecting to the Spread ~
segment designated by ~S~@:>"
			     daemon))
	   (go retry))
	 (use-daemon (new-daemon)
	   :interactive (lambda ()
			  (format *query-io* "Specify daemon: ")
			  (force-output *query-io*)
			  (list (read-line *query-io*)))
	   :report (lambda (stream)
		     (format stream "~@<Retry connecting to the Spread ~
segment with a different daemon designator.~@:>"))
	   (setf daemon new-daemon)
	   (go retry))))
    result))
