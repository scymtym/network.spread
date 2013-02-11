;;;; types.lisp --- Types for the network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(deftype membership-event ()
  "The keywords which designate membership events."
  '(member :join :leave :other))
