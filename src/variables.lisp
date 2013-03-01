;;;; variables.lisp --- Global/special variables used in network.spread.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Network communication-related quantities

(defconstant +maximum-message-data-length+ +max-message+
  "The maximum length (in bytes) of the data in a spread message.")

;;; Daemon-related parameters

(defparameter *default-port* (or (asdf:component-property
				  (asdf:find-system :network.spread)
				  :default-port)
				 4803)
  "The default port on which the Spread should listen when it is
started via `start-daemon' or `with-daemon'.")

(defparameter *default-daemon-program* (or (asdf:component-property
					    (asdf:find-system :network.spread)
					    :default-daemon-program)
					   "spread")
  "The default name of the program that should be executed when
starting the Spread daemon.")
