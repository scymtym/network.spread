;;;; variables.lisp --- Unit test for handling of special variables.
;;;;
;;;; Copyright (C) 2011, 2012 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(deftestsuite variables-root (root)
  ()
  (:documentation
   "Unit tests for the handling of special variables."))

(addtest (variables-root
          :documentation
	  "Test binding `*incoming-stream*'.")
  incoming-stream-smoke

  (let ((*incoming-stream* (make-string-output-stream)))
    (with-connection (receiver daemon)
      (with-group (receiver "incoming-stream-group")
	(with-connection (sender daemon)
	  (send sender "incoming-stream-group" "foo"))
	(receive receiver :block? t)))

    (let ((captured (get-output-stream-string *incoming-stream*))
	  (expected "66 6F 6F"))
      (ensure-same
       captured expected
       :test      #'string=
       :report    "~@<The string captured by binding `*incoming-stream*' ~
was ~S, not ~S ~@:>"
       :arguments (captured expected)))))

(addtest (variables-root
          :documentation
	  "Test binding `*outgoing-stream*'.")
  outgoing-stream-smoke

  (let ((*outgoing-stream* (make-string-output-stream)))
    (with-connection (sender daemon)
      (send sender "outgoing-stream-group" "foo"))

    (let ((captured (get-output-stream-string *outgoing-stream*))
	  (expected "66 6F 6F"))
      (ensure-same
       captured expected
       :test      #'string=
       :report    "~@<The string captured by binding `*outgoing-stream*' ~
was ~S, not ~S ~@:>"
       :arguments (captured expected)))))
