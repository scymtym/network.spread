;;;; connection.lisp --- Class representing connections to the Spread network.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Utilities

(declaim (inline check-group-name))
(defun check-group-name (name)
  (unless (<= (length name) +group-name-length-limit+)
    (error 'group-too-long-error
           :group (etypecase name
                    (string       (ascii-to-octets name))
                    (octet-vector name))))
  name)
(declaim (notinline check-group-name))

;;; `connection'

(defclass connection ()
  ((mailbox     :initarg  :mailbox
                :documentation
                "The mailbox of this connection as assigned by the
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

                 Handlers should accept two arguments: 1. a string
                 designating the group whose members changed 2. a list
                 of members after the change.")
   (leave-hook  :initarg  :leave-hook
                :type     list
                :initform nil
                :documentation
                "This hook is run when a Spread client leaves one of
                 the groups this connection is a member of.

                 Handlers should accept two arguments: 1. a string
                 designating the group whose members changed 2. a list
                 of members after the change."))
  (:documentation
   "Instances represent connections to Spread segments.

    Each connection can participate in zero or more Spread
    groups. Group membership is required for receiving messages
    addressed at groups, but not for sending messages to groups."))

(defmethod initialize-instance :after ((instance connection) &key)
  (let ((mailbox (slot-value instance 'mailbox)))
    (tg:finalize instance (lambda ()
                            (network.spread.low-level:client-disconnect
                             mailbox)))))

(defmethod disconnect ((connection connection))
  (tg:cancel-finalization connection)
  (network.spread.low-level:client-disconnect
   (slot-value connection 'mailbox)))

;; TODO where to put this?
;; TODO inline
(defun coerce-group-name (group)
  (typecase group
    (simple-octet-vector group)
    (octet-vector        (coerce group 'simple-octet-vector))
    (string              (sb-ext:string-to-octets group :external-format :ascii)))) ; TODO speed this case up

(defmethod join ((connection connection) (group simple-array))
  (check-type group octet-vector)
  (network.spread.low-level:client-join
   (slot-value connection 'mailbox) group))

(defmethod join ((connection connection) (group string))
  (network.spread.low-level:client-join
   (slot-value connection 'mailbox)
   (coerce-group-name (check-group-name group))))

(defmethod join :after ((connection connection) (group string))
  (pushnew group (slot-value connection 'groups) :test #'string=))

;;; Relies on the `string'- and `simple-array'-specialized methods
(defmethod join ((connection connection) (group sequence))
  (map nil (curry #'join connection) group))

(defmethod leave ((connection connection) (group simple-array))
  (check-type group octet-vector)
  (network.spread.low-level:client-leave
   (slot-value connection 'mailbox) group))

(defmethod leave ((connection connection) (group string))
  (network.spread.low-level:client-leave
   (slot-value connection 'mailbox)
   (coerce-group-name (check-group-name group))))

(defmethod leave :after ((connection connection) (group string))
  (removef (slot-value connection 'groups) group :test #'string=))

;;; Relies on the `string'- and `octet-vector'-specialized methods
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

  (let ((mailbox (slot-value connection 'mailbox)))
    ;; Do not enter/break out of loop when non-blocking and no
    ;; messages queued.
    (loop :while (or block? (network.spread.low-level:client-poll mailbox)) :do
       ;; Receive next message, blocking if necessary. Handle
       ;; membership messages via hooks (callbacks). Keep receiving
       ;; until the message is a regular message.
       (let+ (((&values type received-bytes sender groups message-type)
               (network.spread.low-level:client-receive-into
                mailbox buffer start end return-sender? return-groups?)))
         (case type
           (:regular ; Return regular messages.
            (return (values received-bytes sender groups message-type)))
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
    (let ((buffer (make-octet-vector (message-data-length-limit 0))))
      (declare (type simple-octet-vector buffer)
               (dynamic-extent buffer))
      (let+ (((&values received-bytes sender groups message-type)
              (apply #'receive-into connection buffer args)))
        (when received-bytes
          (locally (declare (type (integer 0 #.(message-data-length-limit 0))
                                  received-bytes))
            (values (subseq buffer 0 received-bytes) sender groups message-type)))))))

(labels ((check-data (data group-count)
           (check-type data simple-octet-vector)
           (unless (<= (length data) (message-data-length-limit group-count))
             (error 'message-too-long :data data :group-count group-count)))
         (maybe-coerce-destination (destination)
           (etypecase destination
             (simple-octet-vector
              destination)
             (octet-vector
              (coerce destination 'simple-octet-vector))
             (string
              (ascii-to-octets destination))))
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
       (check-data data 0) ; TODO group-count
       (network.spread.low-level:client-send
        (slot-value connection 'mailbox) destination t 0 data :fifo)) ; TODO
      (t
       (call-next-method))))

  (defmethod send-bytes ((connection  connection)
                         (destination string)
                         (data        simple-array))
    (check-data data 1)
    (network.spread.low-level:client-send
     (slot-value connection 'mailbox)
     (prepare-destination destination) t
     0 data :fifo)) ; TODO

  (defmethod send-bytes ((connection  connection)
                         (destination sequence)
                         (data        simple-array))
    (check-data data (length destination))
    (network.spread.low-level:client-send
     (slot-value connection 'mailbox)
     (map 'vector #'prepare-destination destination) t
     0 data :fifo))) ; TODO

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
  (send-bytes connection destination (ascii-to-octets data)))

;; Relies on the `string'-specialized method
(defmethod send ((connection  connection)
                 (destination sequence)
                 (data        string))
  (let ((octets (ascii-to-octets data)))
    (if (length= 1 destination)
        (send-bytes connection (elt destination 0) octets)
        (send-bytes connection destination octets))))

(defmethod print-object ((object connection) stream)
  (with-slots (name groups) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A (~D)" name (length groups)))))

;;; Constructing a connection

(defmethod connect ((daemon string)
                    &key
                    (name        nil)
                    (membership? t)
                    (priority    0))
  (let+ (((&values host port) (network.spread.base:parse-daemon-name daemon))
         (mailbox (network.spread.low-level:client-connect
                   host port
                   :name name :membership? membership? :priority priority)))
    (make-instance 'connection
                   :mailbox     mailbox
                   :daemon-name daemon
                   :name        (sb-ext:octets-to-string
                                 (network.spread.low-level:client-private-group
                                  mailbox)))))

(defmethod connect :around ((daemon string) &key name membership? priority)
  (declare (ignore name membership? priority))
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
