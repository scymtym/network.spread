;;;; mock-server-coroutine.lisp --- Coroutine-based mock server.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.test)

(defmacro mock-server-coroutine (((read-function  read-sequence-name)
                                  (write-function write-sequence-name))
                                 &body body)
  "Execute BODY with local reader and writer functions that suspend execution.

   READ-SEQUENCE-NAME is bound to a function with
   lambda-list (sequence &key start end) that calls READ-FUNCTION to
   perform the actual reading, passing a continuation that allows
   continuing or suspending execution depending on whether the
   requested data can be read.

   WRITE-SEQUENCE-NAME is bound to a function with
   lambda-list (sequence &key start end) that calls WRITE-FUNCTION to
   perform the actual writing."
  `(cont:with-call/cc ()
     (flet ((,read-sequence-name (sequence
                                  &key (start 0) (end (length sequence)))
              (setf (subseq sequence start end)
                    (cont:let/cc k (funcall ,read-function (- end start) k))))
            (,write-sequence-name (sequence
                                   &key (start 0) (end (length sequence)))
              (funcall ,write-function (subseq sequence start end))))
       ,@body)))
