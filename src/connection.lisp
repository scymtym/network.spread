;;;; connection.lisp --- Class representing connections to the Spread network.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Utilities

(declaim (inline check-group-name))
(defun check-group-name (name)
  (unless (<= (length name)  +maximum-group-name-length+)
    (error 'group-too-long-error
           :group (etypecase name
                    (string       (sb-ext:string-to-octets name))
                    (octet-vector name))))
  name)
(declaim (notinline check-group-name))

;;; `connection'

(defclass connection ()
  ((handle      :initarg  :handle
                :type     integer
                :documentation
                "The handle of this connection as assigned by the
Spread daemon.")
   (daemon-name :initarg  :daemon-name
                :type     string
                :reader   connection-daemon-name
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
                "This hook is run when a Spread client joins one of
the groups this connection is a member of.

Handlers should accept two arguments: 1. a string designating the
group for whose members changed 2. a list of members after the
change.")
   (leave-hook  :initarg  :leave-hook
                :type     list
                :initform nil
                :documentation
                "This hook is run when a Spread client leaves one of
the groups this connection is a member of.

Handlers should accept two arguments: 1. a string designating the
group for whose members changed 2. a list of members after the
change."))
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
  (%join (slot-value connection 'handle) (check-group-name group)))

(defmethod join :after ((connection connection) (group string))
  (pushnew group (slot-value connection 'groups) :test #'string=))

;; Relies on the `string'-specialized method
(defmethod join ((connection connection) (group sequence))
  (map nil (curry #'join connection) group))

(defmethod leave ((connection connection) (group string))
  (%leave (slot-value connection 'handle) (check-group-name group)))

(defmethod leave :after ((connection connection) (group string))
  (removef (slot-value connection 'groups) group :test #'string=))

;; Relies on the `string'-specialized method
(defmethod leave ((connection connection) (group sequence))
  (map nil (curry #'leave connection) group))

(defmethod leave ((connection connection) (group (eql t)))
  (leave connection (slot-value connection 'groups)))

(defmethod receive-into ((connection connection)
                         (buffer     simple-array)
                         &key
                         (start          0)
                         (end            (length buffer))
                         (block?         t)
                         (return-sender? t)
                         (return-groups? t))
  (check-type buffer simple-octet-vector)

  (let ((handle (slot-value connection 'handle)))
    ;; Do not enter/break out of loop when non-blocking and no
    ;; messages queued.
    (loop :while (or block? (%poll handle)) :do
       ;; Receive next message, blocking if necessary. Handle
       ;; membership messages via hooks (callbacks). Keep receiving
       ;; until the message is a regular message.
       (let+ (((&values type received-bytes sender groups)
               (%receive-into handle buffer start end
                              return-sender? return-groups?)))
         (case type
           (:regular ; Return regular messages.
            (return (values received-bytes sender groups)))
           (:join    ; Run hooks for membership messages.
            (run-hook
             (object-hook connection 'join-hook) sender groups))
           (:leave   ; Same for leave; ignore other messages.
            (run-hook
             (object-hook connection 'leave-hook) sender groups)))))))

(defmethod receive ((connection connection)
                    &rest args
                    &key
                    (block?         t)
                    (return-sender? t)
                    (return-groups? t))
  (declare (ignore block? return-sender? return-groups?))

  ;; SBCL won't do stack allocation otherwise
  (locally (declare (optimize (speed 3) (debug 0) (safety 0)))
    (let ((buffer (make-octet-vector +max-message+)))
      (declare (type simple-octet-vector buffer)
               (dynamic-extent buffer))
      (let+ (((&values received-bytes sender groups)
              (apply #'receive-into connection buffer args)))
        (when received-bytes
          (locally (declare (type (integer 0 #.+maximum-message-data-length+)
                                  received-bytes))
            (values (subseq buffer 0 received-bytes) sender groups)))))))

(labels ((check-data (data)
           (check-type data simple-octet-vector)
           (unless (<= (length data) +maximum-message-data-length+)
             (error 'message-too-long :data data)))
         (maybe-coerce-destination (destination)
           (etypecase destination
             (simple-octet-vector
              destination)
             (octet-vector
              (coerce destination 'simple-octet-vector))
             (string
              (sb-ext:string-to-octets destination :external-format :ascii))))
         (prepare-destination (destination)
           (let ((destination (maybe-coerce-destination destination)))
             (declare (type simple-octet-vector destination)
                      (inline check-group-name))
             (check-group-name destination))))

  (defmethod send-bytes ((connection  connection)
                         (destination simple-array)
                         (data        simple-array))
    (typecase destination
      (simple-octet-vector
       (check-data data)
       (%send-one (slot-value connection 'handle) destination data))
      (t
       (call-next-method))))

  (defmethod send-bytes ((connection  connection)
                         (destination string)
                         (data        simple-array))
    (check-data data)
    (%send-one (slot-value connection 'handle)
               (prepare-destination destination)
               data))

  (defmethod send-bytes ((connection  connection)
                         (destination sequence)
                         (data        simple-array))
    (check-data data)
    (%send-multiple (slot-value connection 'handle)
                    (map 'vector #'prepare-destination destination)
                    data)))

(defmethod send ((connection  connection)
                 (destination string)
                 (data        simple-array))
  (send-bytes connection destination data))

;; Relies on the `string'-specialized method
(defmethod send ((connection  connection)
                 (destination sequence)
                 (data        simple-array))
  (if (length= 1 destination)
      (send-bytes connection (elt destination 0) data)
      (send-bytes connection destination data)))

(defmethod send ((connection  connection)
                 (destination string)
                 (data        string))
  (send-bytes connection destination (sb-ext:string-to-octets data)))

;; Relies on the `string'-specialized method
(defmethod send ((connection  connection)
                 (destination sequence)
                 (data        string))
  (let ((octets (sb-ext:string-to-octets data)))
    (if (length= 1 destination)
        (send-bytes connection (elt destination 0) octets)
        (send-bytes connection destination octets))))

(defmethod print-object ((object connection) stream)
  (with-slots (handle name groups) object
    (print-unreadable-object (object stream :type t)
      (format stream "~A (~D) #~D"
              name (length groups) handle))))

;;; Constructing a connection

(defmethod connect ((daemon string))
  (let+ (((&values handle name) (%connect daemon :membership? t)))
    (make-instance 'connection
                   :handle      handle
                   :daemon-name daemon
                   :name        name)))

(defmethod connect :around ((daemon string))
  ;; Install restarts around the connection attempt.
  (loop (restart-case
            (return-from connect (call-next-method daemon))
          (retry ()
            :report (lambda (stream)
                      (format stream "~@<Retry connecting to the ~
                                      Spread segment designated by ~
                                      ~S~@:>"
                              daemon)))
          (use-daemon (new-daemon)
            :interactive (lambda ()
                           (format *query-io* "Specify daemon: ")
                           (force-output *query-io*)
                           (list (read-line *query-io*)))
            :report (lambda (stream)
                      (format stream "~@<Retry connecting to the ~
                                      Spread segment with a ~
                                      different daemon ~
                                      designator.~@:>"))
            (setf daemon new-daemon)))))
