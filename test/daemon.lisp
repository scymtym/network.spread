;;;; daemon.lisp --- Unit tests for daemon-related functions.
;;;;
;;;; Copyright (C) 2011, 2012 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(deftestsuite daemon-suite (root)
  ()
  (:documentation
   "Test suite for daemon-related functions."))

(addtest (daemon-suite
          :documentation
	  "Smoke test for starting and stopping a spread daemon using
`start-daemon' and `stop-daemon' respectively.")
  smoke

  (let ((daemon (start-daemon :port (+ port 3))))
    (ensure (not (null daemon)))
    (stop-daemon daemon)))

(addtest (daemon-suite
          :documentation
	  "Check that errors such as starting two spread daemons with
identical ports are detected and reported.")
  startup-failure

  (with-daemon (:port (+ port 6))
    (ensure-condition 'failed-to-start-daemon
      (with-daemon (:port         (+ port 6)
		    :num-attempts 1)))))
