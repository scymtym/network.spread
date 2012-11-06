;;;; macros.lisp --- Unit tests for macros provided by the network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(deftestsuite macros-root (root)
  ()
  (:documentation
   "Unit test for the convenience macros provided by the
network.spread system."))

(deftestsuite with-connection-root (macros-root)
  ()
  (:documentation
   "Unit tests for the `with-connection' macro."))

(addtest (with-connection-root
          :documentation
	  "Smoke test for the `with-connection' macro.")
  smoke

  (with-connection (connection daemon)
    (ensure connection)))

(deftestsuite with-group-root (macros-root)
  ()
  (:documentation
   "Unit tests for the `with-group' macro."))

(addtest (with-group-root
          :documentation
	  "Smoke test for the `with-group' macro.")
  smoke

  (with-connection (connection daemon)
    (with-group (connection "foo")
      (ensure-same
       (connection-groups connection) '("foo")
       :test #'equal))

    (ensure-null (connection-groups connection))))
