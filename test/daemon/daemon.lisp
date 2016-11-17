;;;; daemon.lisp --- Unit tests for daemon-related functions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.daemon.test)

(in-suite :network.spread.daemon)

(test smoke
  "Smoke test for starting and stopping a Spread daemon using
   `start-daemon' and `stop-daemon' respectively."

  (let ((daemon (start-daemon :port (+ *port* 3))))
    (is-true daemon)
    (stop-daemon daemon)))

(test startup-failure
  "Check that errors such as starting two Spread daemons with
   identical ports are detected and reported."

  (with-daemon (:port (+ *port* 6))
    (signals failed-to-start-daemon
      (with-daemon (:port         (+ *port* 6)
                    :num-attempts 1)))))
