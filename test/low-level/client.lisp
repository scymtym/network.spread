;;;; client.lisp --- Unit test for the low-level client implementation.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.low-level.test)
(in-suite :network.spread.low-level)

;;; Connection management

(test client-connect/smoke
  "Smoke test for the `client-connect' function."

  (with-mock-server ('network.spread.wire-protocol.test::mock-server/connect
                     :send-private-group (ascii-to-octets "foo"))
      (port)
    (finishes (client-connect "localhost" port))))

(test client-disconnect/smoke
  "Smoke test for the `client-disconnect' function."

  (with-mock-connection ('network.spread.wire-protocol.test::mock-server/disconnect)
      (mailbox)
    (finishes (client-disconnect mailbox))))

(test client-private-group/smoke
  "Smoke test for the `client-private-group' function."

  #+no (with-mock-connection ('network.spread.wire-protocol.test::mock-server/disconnect)
      (mailbox)
    (is-true (client-private-group mailbox))))

;;; Sending and receiving messages

(test client-poll/smoke
  "Smoke test for the `client-poll' function."

  )

(test client-send/service-type/smoke
  "Smoke test for the `client-send/service-type' function."

  (map-product
   (lambda (service-type groups message-type payload)
     (with-mock-connection ('network.spread.wire-protocol.test::mock-server/send)
         (mailbox)
       (finishes (client-send/service-type mailbox service-type groups message-type payload))))
   ;; service-type
   `(,+regular-message/unreliable+ ,+regular-message/fifo+)
   ;; groups
   `(,(vector (octet-vector 1 2 3)))
   ;; message-type
   `(0 1 ,(1- (ash 1 16))) ; TODO same in all client-send test cases
   ;; payload
   `(,(octet-vector) ,(octet-vector 1 2 3))))

(test client-send/smoke
  "Smoke test for the `client-send' function."

  (map-product
   (lambda (quality-of-service groups message-type payload)
     (with-mock-connection ('network.spread.wire-protocol.test::mock-server/send)
         (mailbox)
       (finishes (client-send/service-type
                  mailbox quality-of-service groups message-type payload))))
   ;; quality-of-service
   `(:unreliable)
   ;; groups
   `(,(vector (octet-vector 1 2 3)))
   ;; message-type
   `(0 1 ,(1- (ash 1 16)))
   ;; payload
   `(,(octet-vector) ,(octet-vector 1 2 3))))

(test client-receive-into/smoke
  "Smoke test for the `client-receive-into' function."

  (map-product
   (lambda (service-type sender groups message-type payload
            return-sender? return-groups? short-buffer?)
     (let ((membership? (plusp (logand service-type network.spread.wire-protocol::+membership-message-mask+))))
       (with-mock-connection ('network.spread.wire-protocol.test::mock-server/receive
                              :service-type service-type
                              :sender       sender
                              :groups       groups
                              :message-type message-type
                              :payload      payload)
           (mailbox)
         (let+ ((buffer (if short-buffer?
                            (make-octet-vector 2)
                            (make-octet-vector (network.spread.wire-protocol:message-data-length-limit
                                                (length groups)))))
                ((&values received-kind received-sender received-groups
                          received-message-type received-payload-length)
                 (client-receive-into mailbox buffer 0 (length buffer)
                                      return-sender? return-groups?)))
           (is (eq :regular received-kind))
           (when (or (eq return-sender? t)
                     (and (eq return-sender? :when-membership) membership?))
             (is (equalp (coerce-to-group-name sender) received-sender)))
           (when (or (eq return-groups? t)
                     (and (eq return-groups? :when-membership) membership?))
             (is (equalp (coerce-to-group-names groups) received-groups)))
           (is (equal  message-type     received-message-type))
           (is (eql    (length payload) received-payload-length))
           (is (equalp (subseq payload 0 (min (length buffer)
                                              (length payload)))
                       (subseq buffer  0 (min (length buffer)
                                              received-payload-length))))))))
   ;; service-type
   `(,+regular-message/unreliable+ ,+regular-message/fifo+)
   ;; sender
   '("foo")
   ;; groups
   `(("bar")
     ("bar" "baz"))
   ;; message-type
   '(0)
   ;; payload
   `(,(octet-vector) ,(octet-vector 1 2 3))
   ;; return-sender?
   '(nil t :when-membership)
   ;; return-groups?
   '(nil t :when-membership)
   ;; short buffer
   '(nil t)))

;;; Group membership operations

(test client-join/smoke
  "Smoke test for the `client-join' function."

  (let ((group (octet-vector 1 2 3)))
    (with-mock-connection ('network.spread.wire-protocol.test::mock-server/join)
        (mailbox)
      (finishes (client-join mailbox group)))))

(test client-leave/smoke
  "Smoke test for the `client-leave' function."

  (let ((group (octet-vector 1 2 3)))
    (with-mock-connection ('network.spread.wire-protocol.test::mock-server/leave)
        (mailbox)
      (finishes (client-leave mailbox group)))))
