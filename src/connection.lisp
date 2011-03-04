;;; connection.lisp ---
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
  ((handle     :initarg  :handle
	       :type     integer
	       :documentation
	       "")
   (name       :initarg  :name
	       :type     string
	       :reader   connection-name
	       :documentation
	       "The unique name of this connection within the spread
segment.")
   (groups     :initarg  :groups
	       :type     list
	       :reader   connection-groups
	       :initform nil
	       :documentation
	       "The list of groups this connection is a member of.")
   (join-hook  :initarg  :join-hook
	       :type     list
	       :initform nil
	       :documentation
	       "")
   (leave-hook :initarg  :leave-hook
	       :type     list
	       :initform nil
	       :documentation
	       ""))
  (:documentation
   "Instances of this represent connection to spread segments. Each
connection can participate in zero or more spread groups."))

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

(defmethod leave ((connection connection) (group string))
  (%leave (slot-value connection 'handle) group))

(defmethod leave ((connection connection) (group (eql t)))
  (map nil (curry #'leave connection) (slot-value connection 'groups)))

(defmethod leave :after ((connection connection) (group string))
  (removef (slot-value connection 'groups) group :test #'string=))

(defmethod receive ((connection connection)
		    &key
		    (block? t))
  (iter (while (or block? (%poll (slot-value connection 'handle))))
	(for message next (%receive (slot-value connection 'handle)))
	(if (eq (first message) :regular)
	    (return (values-list (rest message)))
	    (bind (((type group members) message))
	      (run-hook (object-hook connection (case type
						  (:join  'join-hook)
						  (:leave 'leave-hook)))
			group members)))))

(defmethod send ((connection connection) (destination string) (data string))
  (%send-one (slot-value connection 'handle) destination data))

(defmethod send ((connection connection) (destination list) (data string))
  (if (length= 1 destination)
      (%send-one (slot-value connection 'handle) (first destination) data)
      (%send-multiple (slot-value connection 'handle) destination data)))

(defmethod print-object ((object connection) stream)
  (with-slots (handle name groups) object
    (print-unreadable-object (object stream :type t)
      (format stream "~A (~D) #~D"
	      name (length groups) handle))))

(defmethod connect ((daemon string))
  "Connect to the spread segment designated by DAEMON. If the
connection attempt succeeds, a `connection' instance is returned. "
  (bind (((:values handle name) (%connect daemon :membership? t)))
    (make-instance 'connection
		   :handle handle
		   :name   name)))
