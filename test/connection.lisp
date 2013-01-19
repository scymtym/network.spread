;;;; connection.lisp --- Unit tests for the connection class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(defun event-equal (event1 event2)
  (and (string=   (first  event1) (first  event2))
       (set-equal (second event1) (second event2) :test #'string=)))

(defun check-groups (connection expected context)
  (let ((groups (copy-seq (connection-groups connection))))
    (ensure-same groups expected
		 :test      #'alexandria:set-equal
		 :report    "~@<~A, the connection ~A is a member of the ~
groups ~{~S ~^, ~}, not the groups ~{~S~^, ~}.~@:>"
		 :arguments (context connection groups expected))))

(defun drain-messages (connection)
  "Drain all pending messages from CONNECTION."
  (sleep .2)
  (iter:iter (iter:repeat 10) (receive connection :block? nil)))

(defun check-membership-event (joins  expected-joins
			       leaves expected-leaves)
  "Verify JOINS and LEAVES against EXPECTED-JOINS and
EXPECTED-LEAVES."
  (ensure-same joins  expected-joins  :test (curry #'every #'event-equal))
  (ensure-same leaves expected-leaves :test (curry #'every #'event-equal)))

(deftestsuite connection-root (root)
  ()
  (:documentation
   "Units test for the `connection' class and `connect' method."))

(addtest (connection-root
          :documentation
	  "Smoke test for the `connect' method.")
  connect/smoke

  ;; Connect and disconnect. Disconnecting twice has to signal an
  ;; error.
  (let ((connection (connect daemon)))
    (unwind-protect
	 (progn
	   (ensure      connection)
	   (ensure      (stringp (connection-name connection)))
	   (ensure-null (connection-groups connection)))
      (disconnect connection))

    (ensure-condition 'spread-error
      (disconnect connection)))

  ;; Illegal spread name.
  (ensure-condition 'spread-error
    (connect "no-such-daemon"))

  ;; Not cool enough to use that port.
  (ensure-condition 'spread-error
    (connect "31337")))

(addtest (connection-root
          :documentation
	  "Make sure restarts are established correctly during
`connect' call.")
  connect-restart

  (handler-bind
      ((spread-error
	#'(lambda (condition)
	    (ensure (find-restart 'retry))
	    (ensure (find-restart 'use-daemon))
	    (invoke-restart (find-restart 'use-daemon) daemon))))
    (connect "no-such-daemon")))

(addtest (connection-root
          :documentation
	  "Smoke test for group membership functions.")
  membership/smoke

  (with-connection (connection daemon)
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

(addtest (connection-root
          :documentation
	  "Tests for group membership-related conditions.")
  membership-conditions

  (with-connection (connection daemon)
    (ensure-condition 'spread-error
      (join connection (make-string (* 2 +max-group-name+)
				    :initial-element #\a)))

    (ensure-condition 'spread-error
      (leave connection (make-string (* 2 +max-group-name+)
				     :initial-element #\a)))))

(addtest (connection-root
          :documentation
	  "Tests for membership hooks.")
  membership-hooks

  (with-connection (connection daemon)
    (let ((self-name  (connection-name connection))
	  (joins      '())
	  (leaves     '()))
      (hooks:add-to-hook (hooks:object-hook connection 'join-hook)
			 #'(lambda (group members)
			     (push `(,group ,members) joins)))
      (hooks:add-to-hook (hooks:object-hook connection 'leave-hook)
			 #'(lambda (group members)
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

      (with-connection (other daemon)
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

(addtest (connection-root
          :documentation
	  "Smoke test for sending data.")
  send/smoke

  (with-connection (sender daemon)
    (ensure-cases (destination data)
	'(("foo"                "foo") ; Single destination
	  (("group1")           "bar") ; Single destination, dispatched to broadcast method
	  (#("group1" "group2") "bar") ; Multiple destinations as vector
	  )

      (send sender destination data))))

(addtest (connection-root
          :documentation
	  "Test error handling in case of failed send operations.")
  send/failure

  (let ((connection (connect daemon)))
    (disconnect connection)

    ;; Single destination.
    (ensure-condition 'spread-error
      (send connection "does-not-matter" "foo"))

    ;; Multiple destinations.
    (ensure-condition 'spread-error
      (send connection '("foo" "bar") "foo"))))

(addtest (connection-root
          :documentation
	  "Make sure that messages of the maximum allowable size can
be send but larger messages signal an error.")
  send/message-size-limit

  (flet ((make-message (size)
	   (make-string size :initial-element #\a)))
    (with-connection (sender daemon)
      (with-connection (receiver daemon)
	(with-group (receiver "message-size-limit")
	  ;; Longest possible message => has to work.
	  (send sender "message-size-limit"
		(make-message +maximum-message-data-length+))
	  (ensure-same (length (receive receiver))
		       +maximum-message-data-length+
		       :test #'=)

	  ;; Longer message => has to signal an error.
	  (ensure-condition 'message-too-long
	    (send sender "message-size-limit"
		  (make-message (1+ +maximum-message-data-length+)))))))))

(addtest (connection-root
          :documentation
	  "Test error handling in case of failed receive operations.")
  receive/failure

  (let ((connection (connect daemon)))
    (disconnect connection)

    (ensure-condition 'spread-error
      (receive connection))))

(addtest (connection-root
          :documentation
	  "Test sending and receiving data.")
  send-receive

  (with-connection (sender daemon)
    (let ((sender-name (connection-name sender))
	  (expected    (map 'octet-vector #'char-code "bar")))
      (with-connection (receiver daemon)
	(with-group (receiver "group")
	  ;; The receiver should not get this message.
	  (send sender "some-group" "foo")

	  ;; But this one
	  (send sender "group" "bar")
	  (ensure-same (receive receiver :block? t)
		       (values expected sender-name '("group"))
		       :test #'equalp)

	  ;; Non-blocking receive should just return.
	  (receive receiver :block? nil))))))

(addtest (connection-root
          :documentation
	  "Test the `print-object' method on `connection'.")
  print

  (with-connection (connection daemon)
    (ensure
     (not (emptyp
	   (with-output-to-string (stream)
	     (format stream "~A" connection)))))))
