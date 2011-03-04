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
  ()
  (:function
   (check-groups (connection expected context)
     (let ((groups (copy-seq (connection-groups connection))))
       (ensure-same
	groups expected
	:test      #'alexandria:set-equal
	:report    "~@<~A, the connection ~A is a member of the groups ~{~S~
~^, ~}, not the groups ~{~S~^, ~}.~@:>"
	:arguments (context connection groups expected)))))
  (:documentation
   "Units test for the `connection' class and `connect' method."))

(addtest (connection-root
          :documentation
	  "Smoke test for the `connect' method.")
  connect

  (let ((connection (connect daemon)))
    (unwind-protect
	 (ensure connection)
      (disconnect connection)))

  ;; Illegal spread name
  (ensure-condition 'spread-error
    (connect "no-such-daemon"))

  ;; Not cool enough to used that port
  (ensure-condition 'spread-error
    (connect "31337")))

(addtest (connection-root
          :documentation
	  "Tests for group membership functions.")
  membership

  (with-connection (connection daemon)
    (join connection "rsb://example/informer")
    (check-groups
     connection '("rsb://example/informer")
     "After joining the group \"rsb://example/informer\"")

    (join connection "rsb://example/informer")
    (check-groups
     connection '("rsb://example/informer")
     "After joining a group twice \"rsb://example/informer\" twice")

    (join connection "rsb://example/informer")
    (check-groups
     connection '("rsb://example/informer")
     "After leaving the group \"rsb://example/informer\"")))

(addtest (connection-root
          :documentation
	  "Test sending data.")
  send

  (with-connection (sender daemon)
    (send sender "rsb://example/informer" "foo")

    (send sender '("group1" "group2") "bar")))

(addtest (connection-root
          :documentation
	  "Test sending and receiving data.")
  send-receive

  (with-connection (sender daemon)
    (let ((sender-name (connection-name sender)))
      (with-connection (receiver daemon)
	(with-group (receiver "rsb://example/informer")
	  ;; The receiver should not get this message
	  (send sender "rsb://example/some-group" "foo")

	  ;; But this one
	  (send sender "rsb://example/informer" "bar")
	  (ensure-same
	   (receive receiver :block? t)
	   (values "bar" sender-name '("rsb://example/informer"))))))))
