;;;; util.lisp --- Utilities provided by the base module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.base)

;;; Daemon name syntax

(defun parse-daemon-name (name)
  "Split NAME into host and port components.

   Return two values: the host component as string or nil if it is not
   specified and the parsed port component as an integer."
  (with-condition-translation (((error daemon-name-syntax-error)
                                :string name))
    (let+ ((@-index (position #\@ name))
           ((&values host port)
            (if @-index
                (values (subseq name (1+ @-index)) (subseq name 0 @-index))
                (values nil name))))
      (values host (parse-integer port)))))

;;; String coding

(declaim (inline ascii-to-octets octets-to-ascii))

(defun ascii-to-octets (string &rest args &key start end)
  (declare (ignore start end))
  #+sbcl (apply #'sb-ext:string-to-octets string :external-format :ascii args)
  #-sbcl (error "not implemented"))

(defun octets-to-ascii (vector &rest args &key start end)
  (declare (ignore start end))
  #+sbcl (apply #' sb-ext:octets-to-string vector :external-format :ascii args)
  #-sbcl (error "not implemented"))
