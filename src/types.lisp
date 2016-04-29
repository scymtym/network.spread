;;;; types.lisp --- Types for the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Constants

(defconstant +max-message+    140000
  "The maximum size of a single message.")

(defconstant +max-group-name+ 32
  "The maximum size of a group name.")

(defconstant +max-groups+     1024
  "The maximum number of groups in a single API call.")

;;; Membership-related keywords

(deftype membership-event ()
  "The keywords which designate membership events."
  '(member :join :leave :other))

(deftype return-aspect-switch ()
  '(or boolean (eql :when-membership)))
