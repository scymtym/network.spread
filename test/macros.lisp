;;;; macros.lisp --- Unit tests for macros provided by the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(def-suite :network.spread.macros
  :in :network.spread
  :description
  "Unit test for the convenience macros provided by the network.spread
   system.")
(in-suite :network.spread.macros)

(test with-connection/smoke
  "Smoke test for the `with-connection' macro."

  (with-connection (connection *daemon*)
    (is-true connection)))

(test with-group/smoke
  "Smoke test for the `with-group' macro."

  (flet ((do-it (wait?)
           (with-connection (connection *daemon*)
             (with-group (connection "foo" :wait? wait?)
               (is (equal '("foo") (connection-groups connection))))

             (is (emptyp (connection-groups connection))))))
    (do-it t)
    (do-it nil)))
