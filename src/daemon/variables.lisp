;;;; variables.lisp --- Variables used by the daemon module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.daemon)

(defparameter *default-port* network.spread-system:*default-port*
  "The default port on which the Spread daemon should listen when it
   is started via `start-daemon' or `with-daemon'.")

(defparameter *default-daemon-program*
  network.spread-system:*default-daemon-program*
  "The default name of the program that should be executed when
   starting the Spread daemon.")
