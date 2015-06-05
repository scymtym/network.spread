;;;; connection.lisp --- Unit tests for the connection class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(defun event-equal (event1 event2)
  (and (string=   (first  event1) (first  event2))
       (set-equal (second event1) (second event2) :test #'string=)))

(defun check-groups (connection expected context)
  (let ((groups (copy-seq (connection-groups connection))))
    (is (set-equal expected groups)
        "~@<~A, the connection ~A is a member of the
         groups ~{~S ~^, ~}, not the groups
         ~{~S~^, ~}.~@:>"
        context connection groups expected)))

(defun drain-messages (connection)
  "Drain all pending messages from CONNECTION."
  (sleep .2)
  (loop :repeat 10 :do (receive connection :block? nil)))

(defun check-membership-event (joins  expected-joins
                               leaves expected-leaves)
  "Verify JOINS and LEAVES against EXPECTED-JOINS and
   EXPECTED-LEAVES."
  (is (every #'event-equal expected-joins  joins))
  (is (every #'event-equal expected-leaves leaves)))

(def-suite :network.spread.connection
  :in :network.spread
  :description
  "Units test for the `connection' class and `connect' method.")
(in-suite :network.spread.connection)

(test connect/smoke
  "Smoke test for the `connect' method."

  ;; Connect and disconnect. Disconnecting twice has to signal an
  ;; error.
  (let ((connection (connect *daemon*)))
    (unwind-protect
         (progn
           (is-true connection)
           (is (stringp (connection-name connection)))
           (is (null (connection-groups connection))))
      (disconnect connection))

    (signals spread-error
      (disconnect connection)))

  ;; Illegal spread name.
  (signals spread-client-error
    (connect "no-such-daemon"))

  ;; Not cool enough to use that port.
  (signals spread-client-error
    (connect "31337")))

(test connect-restart
  "Make sure restarts are established correctly during `connect'
   call."

  (handler-bind
      ((spread-client-error (lambda (condition)
                              (declare (ignore condition))
                              (is-true (find-restart 'retry))
                              (is-true (find-restart 'use-daemon))
                              (invoke-restart (find-restart 'use-daemon) *daemon*))))
    (connect "no-such-daemon")))

(test membership/smoke
  "Smoke test for group membership functions."

  (with-connection (connection *daemon*)
    (check-groups connection '()
     "Initially")

    (join connection "a")
    (check-groups connection '("a")
     "After joining the group \"a\"")

    (join connection #("a"))
    (check-groups connection '("a")
     "After joining the group \"a\" twice")

    (leave connection "a")
    (check-groups connection '()
     "After leaving the group \"a\"")

    (join connection '("foo" "bar" "baz"))
    (check-groups connection '("foo" "bar" "baz")
     "After joining groups \"foo\", \"bar\" and \"baz\"")

    (leave connection #("foo" "bar"))
    (check-groups connection '("baz")
     "After leaving groups \"foo\" and \"bar\"")

    (leave connection t)
    (check-groups connection '()
     "After leaving all groups")))

(test membership-conditions
  "Tests for group membership-related conditions."

  (with-connection (connection *daemon*)
    (signals group-too-long-error
      (join connection (make-string (* 2 +max-group-name+)
                                    :initial-element #\a)))

    (signals group-too-long-error
      (leave connection (make-string (* 2 +max-group-name+)
                                     :initial-element #\a)))))

(test membership-hooks
  "Tests for membership hooks."

  (with-connection (connection *daemon*)
    (let ((self-name  (connection-name connection))
          (joins      '())
          (leaves     '()))
      (hooks:add-to-hook (hooks:object-hook connection 'join-hook)
                         (lambda (group members)
                           (push `(,group ,members) joins)))
      (hooks:add-to-hook (hooks:object-hook connection 'leave-hook)
                         (lambda (group members)
                           (push `(,group ,members) leaves)))

      (join connection "foo")
      (drain-messages connection)
      (check-membership-event joins  `(("foo" (,self-name)))
                              leaves '())

      (join connection "bar")
      (drain-messages connection)
      (check-membership-event joins  `(("bar" (,self-name))
                                       ("foo" (,self-name)))
                              leaves '())

      (with-connection (other *daemon*)
        (let ((other-name (connection-name other)))
          (join other "foo")
          (drain-messages connection)
          (check-membership-event joins  `(("foo" (,other-name ,self-name))
                                           ("bar" (,self-name))
                                           ("foo" (,self-name)))
                                  leaves '())

          (join other "bar")
          (drain-messages connection)
          (check-membership-event joins  `(("bar" (,other-name ,self-name))
                                           ("foo" (,other-name ,self-name))
                                           ("bar" (,self-name))
                                           ("foo" (,self-name)))
                                  leaves '())

          (join other "baz")
          (drain-messages connection)
          (check-membership-event joins  `(("bar" (,other-name ,self-name))
                                           ("foo" (,other-name ,self-name))
                                           ("bar" (,self-name))
                                           ("foo" (,self-name)))
                                  leaves '())

          (leave other "bar")
          (drain-messages connection)
          (check-membership-event joins  `(("bar" (,other-name ,self-name))
                                           ("foo" (,other-name ,self-name))
                                           ("bar" (,self-name))
                                           ("foo" (,self-name)))
                                  leaves `(("bar" (,self-name)))))))))

(test send/smoke
  "Smoke test for sending data."

  (with-connection (sender *daemon*)
    (mapc (lambda+ ((destination data))
            (send sender destination data))
          '(("foo"                "foo") ; Single destination
            (("group1")           "bar") ; Single destination, dispatched to broadcast method
            (#("group1" "group2") "bar") ; Multiple destinations as vector
            ))))

(test send/failure
  "Test error handling in case of failed send operations."

  (let ((connection (connect *daemon*)))
    ;; Too long group name should signal `group-too-long-error'.
    (signals group-too-long-error
      (send connection
            (make-string (* 2 +maximum-group-name-length+)
                         :initial-element #\a)
            "does-not-matter"))

    (disconnect connection)

    ;; Single destination.
    (signals spread-client-error
      (send connection "does-not-matter" "foo"))

    ;; Multiple destinations.
    (signals spread-client-error
      (send connection '("foo" "bar") "foo"))))

(test send/message-size-limit
  "Make sure that messages of the maximum allowable size can be send
   but larger messages signal an error."

  (flet ((make-message (size)
           (make-string size :initial-element #\a)))
    (with-connection (sender *daemon*)
      (with-connection (receiver *daemon*)
        (with-group (receiver "message-size-limit")
          ;; Longest possible message => has to work.
          (send sender "message-size-limit"
                (make-message +maximum-message-data-length+))
          (is (= +maximum-message-data-length+
                 (length (receive receiver))))

          ;; Longer message => has to signal an error.
          (signals message-too-long
            (send sender "message-size-limit"
                  (make-message (1+ +maximum-message-data-length+)))))))))

(test receive/failure
  "Test error handling in case of failed receive operations."

  (let ((connection (connect *daemon*)))
    (disconnect connection)
    (signals spread-client-error (receive connection))))

(defmacro with-receive-test-case-context ((&key (group "group")) &body body)
  (once-only (group)
    `(mapc (lambda+ ((expected-message sender? expected-group))
             (with-connection (sender *daemon*)
               (let ((sender-name (connection-name sender)))
                 (with-connection (receiver *daemon*)
                   (with-group (receiver ,group)
                     ,@body)))))
           `((,(octet-vector 98 97 114) t   ,,group)
             (,(octet-vector 98 97 114) nil ,,group)
             (,(octet-vector 98 97 114) t   nil)
             (,(octet-vector 98 97 114) nil nil)))))

(test send-receive/smoke
  "Smoke test for sending and receiving data."

  (with-receive-test-case-context (:group "group")
    ;; The receiver should not get this message.
    (send sender "some-other-group" "foo")

    ;; But this one
    (send sender "group" "bar")
    (is (equalp (multiple-value-list
                 (receive receiver
                          :block?         t
                          :return-groups? (when expected-group t)
                          :return-sender? sender?))
                (list expected-message
                      (when sender?
                        sender-name)
                      (when expected-group
                        (list expected-group)))))

    ;; Non-blocking receive should just return.
    (finishes (receive receiver
                       :block?         nil
                       :return-groups? (when expected-group t)
                       :return-sender? sender?))))

(test send-receive-into/smoke
  "Smoke test for sending and receiving data into a buffer."

  (with-receive-test-case-context (:group "group")
    ;; The receiver should not get this message.
    (send sender "some-other-group" "foo")

    (let ((buffer (make-octet-vector 100)))
      ;; But this one.
      (send sender "group" "bar")
      (is (equalp (multiple-value-list
                   (receive-into receiver buffer
                                 :block?         t
                                 :return-groups? (when expected-group t)
                                 :return-sender? sender?))
                  (list (length expected-message)
                        (when sender?
                          sender-name)
                        (when expected-group
                          (list expected-group)))))

      ;; Buffer too small => spread-client-error: buffer-too-short
      ;;
      ;; This cannot be tested due to a bug in the Spread client
      ;; library.
      #+spread-fixed (send sender "group" "bar")
      #+spread-fixed (signals spread-client-error
                       (receive-into receiver (make-octet-vector 2)
                                     :block?         t
                                     :return-groups? (when expected-group t)
                                     :return-sender? sender?))

      ;; Non-blocking receive should just return.
      (finishes (receive-into receiver buffer
                              :block?         nil
                              :return-groups? (when expected-group t)
                              :return-sender? sender?)))))

(test self-discard
  "Test that `connection' instances do not receive messages sent by
   themselves."

  (with-connection (connection *daemon*)
    (with-group (connection "group")
      (send connection "group" "foo")
      (loop :repeat 10 :do
         (is (null (receive connection :block? nil)))
         (sleep .1)))))

(test print
  "Test the `print-object' method on `connection'."

  (with-connection (connection *daemon*)
    (is-false (emptyp (with-output-to-string (stream)
                        (format stream "~A" connection))))))
