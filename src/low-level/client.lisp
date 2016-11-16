;;;; client.lisp --- Client role of the Spread wire protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.low-level)

(defvar *default-authentication-methods*
  (list (network.spread.authentication:make-authentication-method :null)))

(defun socket-connect/unix (path &key element-type)
  #+(and sbcl unix)
  (let ((pathname (pathname path))
        (socket   (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket (sb-ext:native-namestring pathname))
    (let ((stream (sb-bsd-sockets:socket-make-stream
                   socket :input t :output t :element-type element-type)))
      (usocket::make-stream-socket :socket socket :stream stream)))
  #-(and sbcl unix)
  (error "~@<UNIX domain socket connections are not supported on this ~
          platform or with this implementation.~@:>"))

;; TODO should name be an octet-vector?
(defun client-connect (host port
                       &key
                       name
                       (membership?            t)
                       (priority               0)
                       (authentication-methods *default-authentication-methods*))
  (let* ((socket        (if host
                            (usocket:socket-connect host port :element-type 'octet)
                            (socket-connect/unix (format nil "/tmp/~D" port)
                                                 :element-type 'octet)))
         (stream        (usocket:socket-stream socket))
         (private-group (unwind-protect-case ()
                            (network.spread.wire-protocol:client-connect
                             stream name membership? priority authentication-methods)
                          (:abort
                           (ignore-errors (usocket:socket-close socket))))))
    (make-mailbox socket stream private-group)))

(defun client-disconnect (mailbox)
  (let+ (((&structure-r/o mailbox- stream private-group) mailbox))
    (network.spread.wire-protocol:client-disconnect stream private-group))
  (values))

(defun client-private-group (mailbox)
  (mailbox-private-group mailbox))

(declaim (inline client-poll))
(defun client-poll (mailbox)
  (listen (mailbox-stream mailbox)))

(defun client-send/service-type (mailbox service-type groups message-type payload)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let+ (((&structure-r/o mailbox- stream private-group) mailbox))
    (locally
        (declare (inline network.spread.wire-protocol:client-send))
      (network.spread.wire-protocol:client-send
       stream private-group service-type groups message-type payload))
    (force-output stream)
    (values)))

(declaim (inline client-send))
(defun client-send (mailbox groups self-discard? message-type payload
                    quality-of-service)
  (let* ((message-kind (quality-of-service->regular-message-kind
                        quality-of-service))
         (service-type (maybe-add-self-discard message-kind self-discard?)))
    (client-send/service-type mailbox service-type groups message-type payload)
    (values)))

(defun client-receive-into (mailbox buffer start end return-sender? return-groups?)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let+ (((&structure-r/o mailbox- stream) mailbox)
         ((&values service-type private-group groups message-type payload-length)
          (locally
              (declare (inline network.spread.wire-protocol:client-receive-into))
            (network.spread.wire-protocol:client-receive-into
             stream buffer start end return-sender? return-groups?))))
    (values (service-type->message-kind service-type)
            private-group groups message-type payload-length)))

(defun client-join (mailbox group)
  (let+ (((&structure-r/o mailbox- stream private-group) mailbox))
    (network.spread.wire-protocol:client-join stream private-group group)
    (force-output stream))
  (values))

(defun client-leave (mailbox group)
  (let+ (((&structure-r/o mailbox- stream private-group) mailbox))
    (network.spread.wire-protocol:client-leave stream private-group group)
    (force-output stream))
  (values))
