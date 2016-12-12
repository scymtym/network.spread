;;;; mock-server-streams.lisp --- Stream interface for mock servers.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol.test)

;;; `make-mock-server-output-stream'
;;;
;;; A stream that calls a continuation function after a given amount
;;; of data has been written to the stream.
;;;
;;; Limitations:
;;; + `write-{byte,sequence}' calls can not write more data to the
;;;   stream than it is currently waiting for.

(defclass continuation-vector-output-stream (flex::vector-output-stream)
  ((needed-count :initarg  :needed-count
                 :type     non-negative-integer
                 :accessor stream-needed-count
                 :initform 0
                 :documentation
                 "The number of octets needed before the continuation
                  function can be called.")
   (cont         :initarg  :cont
                 :accessor stream-cont
                 :documentation
                 "The continuation function that should be called
                  after receiving the required number of octets."))
  (:default-initargs
   :vector      (make-array 0 :fill-pointer 0)
   :transformer nil)
  (:documentation
   "Calls a continuation after accepting a given amount of data."))

(flet ((check (stream amount)
         (let+ (((&structure stream- needed-count cont) stream))
           (assert (<= amount needed-count))
           (when (zerop (decf needed-count amount))
             (funcall cont (flex:get-output-stream-sequence stream))))))

  (defmethod trivial-gray-streams:stream-write-byte :after
    ((stream continuation-vector-output-stream) (integer integer))
    (check stream 1))

  (defmethod trivial-gray-streams:stream-write-sequence :after
    ((stream continuation-vector-output-stream) (sequence sequence)
     start end &key)
    (check stream (- end start))))

(defun make-mock-server-output-stream ()
  (let+ ((stream (make-instance 'continuation-vector-output-stream))
         ((&flet read-from-server (amount k)
            (let+ (((&structure stream- needed-count cont) stream))
              (incf needed-count amount)
              (assert (plusp needed-count))
              (setf cont k)))))
    (values stream #'read-from-server)))

;;; `make-mock-server-input-stream'

(defun make-mock-server-input-stream ()
  (let+ ((stream (flex:make-in-memory-input-stream (octet-vector)))
         ((&flet write-to-server-output (sequence)
            (let+ (((&accessors (vector flex::vector-stream-vector)
                                (end    flex::vector-stream-end))
                    stream))
              (setf vector (concatenate 'vector vector sequence)
                    end    (length vector))))))
    (values stream #'write-to-server-output)))

;;; Utilities

(defun call-with-mock-server-stream
    (thunk mock-server-function &rest mock-server-args)
  (let+ (((&values server-input read-from-server)
          (make-mock-server-output-stream))
         ((&values server-output write-to-server)
          (make-mock-server-input-stream))
         (stream (make-two-way-stream server-output server-input)))
    (apply mock-server-function read-from-server write-to-server
           mock-server-args)

    (funcall thunk stream)))

(defmacro with-mock-server-stream ((mock-server-function &rest mock-server-args)
                                   (stream-var) &body body)
  `(call-with-mock-server-stream
    (lambda (,stream-var) ,@body)
    ,mock-server-function ,@mock-server-args))
