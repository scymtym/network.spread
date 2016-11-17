;;;; variables.lisp --- Global/special variables used in network.spread.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defconstant +maximum-message-data-length+ +max-message+
  "The maximum length (in octets) of the data in a Spread message.")

(defconstant +maximum-group-name-length+ +max-group-name+
  "The maximum length (in octets) of a Spread group name.")
