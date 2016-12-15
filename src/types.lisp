;;;; types.lisp --- Types for the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Membership-related keywords

(deftype membership-event ()
  "The keywords which designate membership events."
  '(member :join :leave :other))

(deftype return-aspect-switch ()
  '(or null                                       ; no returned
       (member t       :when-membership)          ; as `octet-vector'
       (member :string :when-membership/string))) ; as `string'
