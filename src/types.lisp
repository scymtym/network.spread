;;;; types.lisp --- Types for the network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype membership-event ()
  "The keywords which designate membership events."
  '(member :join :leave :other))
