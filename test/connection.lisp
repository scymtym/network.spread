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

(in-package :spread.test)

(deftestsuite connection-root (root)
  (joins
   leaves)
  (:function
   (check-groups (connection expected context)
     (let ((groups (copy-seq (connection-groups connection))))
       (ensure-same
	groups expected
	:test      #'alexandria:set-equal
	:report    "~@<~A, the connection ~A is a member of the groups ~{~S~
~^, ~}, not the groups ~{~S~^, ~}.~@:>"
	:arguments (context connection groups expected)))))
  (:function
   (event-equal (event1 event2)
     (and (string=   (first  event1) (first  event2))
	  (set-equal (second event1) (second event2)
		     :test #'string=))))
  (:function
   (check-membership-event (connection
			    expected-joins expected-leaves)
   (sleep .2)
   (iter:iter (iter:repeat 10) (receive connection :block? nil))
   (ensure-same
    joins expected-joins
    :test (curry #'every #'(lambda (e1 e2) (event-equal e1 e2))))
   (ensure-same
    leaves expected-leaves
    :test (curry #'every #'(lambda (e1 e2) (event-equal e1 e2))))))
  (:documentation
   "Units test for the `connection' class and `connect' method."))

(addtest (connection-root
          :documentation
	  "Smoke test for the `connect' method.")
  connect

  (let ((connection (connect daemon)))
    (unwind-protect
	 (progn
	   (ensure connection)
	   (ensure (stringp (connection-name connection)))
	   (ensure-null (connection-groups connection)))
      (disconnect connection))

    (ensure-condition 'spread-error
      (disconnect connection)))

  ;; Illegal spread name
  (ensure-condition 'spread-error
    (connect "no-such-daemon"))

  ;; Not cool enough to use that port
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
	    (ensure
	     (find-restart 'retry))
	    (ensure
	     (find-restart 'use-daemon))

	    (invoke-restart (find-restart 'use-daemon)
			    daemon))))
    (connect "no-such-daemon")))

(addtest (connection-root
          :documentation
	  "Tests for group membership functions.")
  membership

  (with-connection (connection daemon)
    (check-groups
     connection '()
     "Initially")

    (join connection "rsb://example/informer")
    (check-groups
     connection '("rsb://example/informer")
     "After joining the group \"rsb://example/informer\"")

    (join connection "rsb://example/informer")
    (check-groups
     connection '("rsb://example/informer")
     "After joining the group \"rsb://example/informer\" twice")

    (leave connection "rsb://example/informer")
    (check-groups
     connection '()
     "After leaving the group \"rsb://example/informer\"")

    (join connection '("foo" "bar" "baz"))
    (check-groups
     connection '("foo" "bar" "baz")
     "After joining groups \"foo\", \"bar\" and \"baz\"")

    (leave connection '("foo" "bar"))
    (check-groups
     connection '("baz")
     "After leaving groups \"foo\" and \"bar\"")

    (leave connection t)
    (check-groups
     connection '()
     "After leaving all groups")))

(addtest (connection-root
          :documentation
	  "Tests for group membership-related conditions.")
  membership-conditions

  (with-connection (connection daemon)
    (ensure-condition 'spread-error
      (join connection (make-string (* 2 spread::+max-group-name+)
				    :initial-element #\a)))

    (ensure-condition 'spread-error
      (leave connection (make-string (* 2 spread::+max-group-name+)
				     :initial-element #\a)))))

(addtest (connection-root
          :documentation
	  "Tests for membership hooks.")
  membership-hooks

  (with-connection (connection daemon)
    (let ((self-name  (connection-name connection)))
      (hooks:add-to-hook (hooks:object-hook connection 'join-hook)
			 #'(lambda (group members)
			     (push `(,group ,members) joins)))
      (hooks:add-to-hook (hooks:object-hook connection 'leave-hook)
			 #'(lambda (group members)
			     (push `(,group ,members) leaves)))
      (join connection "foo")
      (check-membership-event
       connection
       `(("foo" (,self-name)))
       ())

      (join connection "bar")
      (check-membership-event
       connection
       `(("bar" (,self-name))
	 ("foo" (,self-name)))
       ())

      (with-connection (other daemon)
	(let ((other-name (connection-name other)))
	  (join other "foo")
	  (check-membership-event
	   connection
	   `(("foo" (,other-name ,self-name))
	     ("bar" (,self-name))
	     ("foo" (,self-name)))
	   ())

	  (join other "bar")
	  (check-membership-event
	   connection
	   `(("bar" (,other-name ,self-name))
	     ("foo" (,other-name ,self-name))
	     ("bar" (,self-name))
	     ("foo" (,self-name)))
	   ())

	  (join other "baz")
	  (check-membership-event
	   connection
	   `(("bar" (,other-name ,self-name))
	     ("foo" (,other-name ,self-name))
	     ("bar" (,self-name))
	     ("foo" (,self-name)))
	   ())

	  (leave other "bar")
	  (check-membership-event
	   connection
	   `(("bar" (,other-name ,self-name))
	     ("foo" (,other-name ,self-name))
	     ("bar" (,self-name))
	     ("foo" (,self-name)))
	   `(("bar" (,self-name)))))))))

(addtest (connection-root
          :documentation
	  "Test sending data.")
  send

  (with-connection (sender daemon)
    (send sender "rsb://example/informer" "foo")

    (send sender '("group1") "bar")
    (send sender '("group1" "group2") "bar")))

(addtest (connection-root
          :documentation
	  "Test sending and receiving data.")
  send-receive

  (with-connection (sender daemon)
    (let ((sender-name (connection-name sender))
	  (expected    (map 'octet-vector #'char-code "bar")))
      (with-connection (receiver daemon)
	(with-group (receiver "rsb://example/informer")
	  ;; The receiver should not get this message
	  (send sender "rsb://example/some-group" "foo")

	  ;; But this one
	  (send sender "rsb://example/informer" "bar")
	  (ensure-same
	   (receive receiver :block? t)
	   (values expected sender-name '("rsb://example/informer"))
	   :test #'equalp))))))
