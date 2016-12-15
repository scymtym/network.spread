;;;; types.lisp --- Types for the network.spread system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Membership-related keywords

(deftype membership-event ()
  "The keywords which designate membership events."
  '(member :join :leave :other))

(deftype return-aspect-switch ()
  '(or null                                       ; not returned
       (member t       :when-membership)          ; as `octet-vector'
       (member :string :when-membership/string))) ; as `string'

(defun return-aspect/high->return-aspect/low (return-aspect)
  (case return-aspect
    (:string                 t)
    (:when-membership/string :when-membership)
    (t                       return-aspect)))
