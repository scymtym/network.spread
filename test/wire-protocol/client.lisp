;;;; client.lisp --- Unit tests for the wire-protocol client implementation.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol.test)
(in-suite :network.spread.wire-protocol)

;;; Connection management

(test client-connect/smoke
  "Smoke test for the `client-connect' function."

  (let ((server-version (list *major-version* *minor-version* *patch-version*))
        (private-group  (ascii-to-octets "foo")))

    (with-mock-server-stream
        (#'mock-server/connect :membership? nil :send-private-group private-group)
        (stream)
      (let+ (((&values received-private-group received-server-version)
              (client-connect stream nil nil 0 '())))
        (is (equalp private-group  received-private-group))
        (is (equal  server-version received-server-version))))

    (with-mock-server-stream
        (#'mock-server/connect :membership? nil :private-group private-group)
        (stream)
      (let+ (((&values received-private-group received-server-version)
              (client-connect stream private-group nil 0 '())))
        (is (equalp private-group  received-private-group))
        (is (equal  server-version received-server-version))))))

(test client-connect/incompatible-daemon-version
  "Test behavior of `client-connect' in case of an incompatible daemon
   version."

  (let ((private-group (ascii-to-octets "foo")))
    (with-mock-server-stream (#'mock-server/connect
                              :send-version       '(3 7 0)
                              :send-private-group private-group)
        (stream)
      (signals incompatible-daemon-error
        (client-connect stream nil t 0 '())))))

(test client-connect/reject
  "Test behavior of `client-connect' in case the daemon rejects the
   connection attempt."

  (let ((private-group (ascii-to-octets "foo")))
    (with-mock-server-stream (#'mock-server/connect
                              :send-private-group  private-group
                              :auth-methods-length -6)
        (stream)
      (signals failure-result-error
        (client-connect stream nil t 0 '())))))

(test client-disconnect/smoke
  "Smoke test for the `client-disconnect' function."

  (let ((private-group (ascii-to-octets "foo")))
    (with-mock-server-stream (#'mock-server/disconnect) (stream)
      (finishes (client-disconnect stream private-group)))))

;;; Client authentication

(test client-authenticate/smoke
  "Smoke test for the `client-authenticate' function."

  (let ((null-method (make-authentication-method :null))
        (mock-method (make-authentication-method :mock :name "FOO")))

    (with-mock-server-stream (#'mock-server/authentication) (stream)
      (finishes (client-authenticate stream '())))

    (with-mock-server-stream (#'mock-server/authentication) (stream)
      (signals method-rejected-error
        (client-authenticate stream (list null-method))))

    ;; TODO :result should trigger protocol-error for invalid codes and authentication-failed-error for valid codes /= 1
    (with-mock-server-stream
        (#'mock-server/authentication :methods '("NULL") :result 255)
        (stream)
      (signals authentication-failed-error
        (client-authenticate stream (list null-method))))

    #+no (do-it (lambda (stream)
                  (signals protocol-error
                    (client-authenticate stream (list null-method))))
           :methods-length 255)

    (with-mock-server-stream (#'mock-server/authentication) (stream)
      (signals method-rejected-error
        (client-authenticate stream (list null-method mock-method))))))

;;; Send and receiving messages

(test client-send/smoke
  "Smoke test for the `client-send' function."

  (map-product
   (lambda (private-group service-type groups message-type payload)
     (let ((private-group (ascii-to-octets private-group))
           (groups        (typecase groups
                            (string
                             (ascii-to-octets groups))
                            (sequence
                             (map (class-of groups) #'ascii-to-octets groups)))))
       (with-mock-server-stream (#'mock-server/send) (stream)
         (finishes (client-send stream private-group service-type groups
                                message-type payload)
                   (finish-output stream)))))
   ;; private-group
   '("foo")
   ;; service-type
   `(,+regular-message/unreliable+ ,+regular-message/fifo+)
   ;; groups
   `("bar"
     ,#("bar" "baz"))
   ;; message-type
   '(0)
   ;; payload
   `(,(octet-vector) ,(octet-vector 1 2 3))))

(test client-receive-into/smoke
  "Smoke test for the `client-receive-into' function."

  (map-product
   (lambda (service-type sender groups message-type payload
            return-sender? return-groups? short-buffer?)
     (let ((membership? (plusp (logand service-type +membership-message-mask+))))
       (with-mock-server-stream (#'mock-server/receive
                                 :service-type service-type
                                 :sender       sender
                                 :groups       groups
                                 :message-type message-type
                                 :payload      payload)
           (stream)
         (let+ ((buffer (if short-buffer?
                            (make-octet-vector 2)
                            (make-octet-vector (message-data-length-limit
                                                (length groups))))) ; TODO start, end
                (start  0)
                (end    (length buffer))
                ((&values received-service-type
                          received-sender received-groups
                          received-message-type received-payload-length)
                 (client-receive-into stream buffer start end
                                      return-sender? return-groups?)))
           (is (eql service-type received-service-type))
           (when (or (eq return-sender? t)
                     (and (eq return-sender? :when-membership) membership?))
             (is (equalp (coerce-to-group-name sender) received-sender)))
           (when (or (eq return-groups? t)
                     (and (eq return-groups? :when-membership) membership?))
             (is (equalp (coerce-to-group-names groups) received-groups)))
           (is (eql    message-type     received-message-type))
           (is (eql    (length payload) received-payload-length))
           (is (equalp (subseq payload 0 (min end (length payload)))
                       (subseq buffer  0 (min end received-payload-length))))))))
   ;; service-type
   `(,+regular-message/unreliable+ ,+regular-message/fifo+)
   ;; sender
   '("foo")
   ;; groups
   `(("bar")
     ("bar" "baz"))
   ;; message-type
   `(0 1 ,(1- (ash 1 16)))
   ;; payload
   `(,(octet-vector) ,(octet-vector 1 2 3)) ; TODO test maximum
   ;; return-sender?
   '(nil t :when-membership)
   ;; return-groups?
   '(nil t :when-membership)
   ;; short buffer
   '(nil t)))

;;; Group membership operations

(test client-join/smoke
  "Smoke test for the `client-join' function."

  (let ((private-group (ascii-to-octets "foo"))
        (group         (ascii-to-octets "bar")))
    (with-mock-server-stream (#'mock-server/join) (stream)
      (finishes (client-join stream private-group group)
                (finish-output stream)))))

(test client-leave/smoke
  "Smoke test for the `client-leave' function."

  (let ((private-group (ascii-to-octets "foo"))
        (group         (ascii-to-octets "bar")))
    (with-mock-server-stream (#'mock-server/leave) (stream)
      (finishes (client-leave stream private-group group)
                (finish-output stream)))))
