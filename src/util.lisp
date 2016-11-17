;;;; util.lisp --- Utilities used by the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defun parse-daemon-name (name)
  "Split NAME into a port and host components.

   Return two values: the host component as string or nil if it is not
   specified and the port component as string."
  (let ((@-index (position #\@ name)))
    (if @-index
        (values (subseq name (1+ @-index)) (subseq name 0 @-index))
        (values nil name))))
