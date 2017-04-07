;;;; client.lisp --- Client role of the Spread wire protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol)

;;; Conversion

(declaim (inline endian-little? endian-mark-little endian-unmark-little))

(defun endian-little? (value)
  (logbitp 7 value))

(defun endian-mark-little (value)
  (logior value +little-endian-marker+))

(defun endian-unmark-little (value)
  (logxor value +little-endian-marker+))

(declaim (inline twos-complement))
(defun twos-complement (value)
  (declare (type octet value))
  (- (logand value (lognot (ash 1 +little-endian-marker-test-bit+)))
     (logand value (ash 1 +little-endian-marker-test-bit+))))

;;; Transmission

(define-constant +empty-octet-vector+
    (octet-vector)
  :test #'equalp)

(#+sbcl sb-ext:defglobal #-sbcl defvar **discard-buffer**
        (make-octet-vector (max (message-data-length-limit 0)
                                +group-data-length-limit+)))
(declaim (type (simple-octet-vector #.(max (message-data-length-limit 0)
                                           +group-data-length-limit+))
               **discard-buffer**)
         #+sbcl (sb-ext:always-bound **discard-buffer**))

(defun %short-read (context sequence start end position)
  (error 'short-read-error
         :context        context
         :expected-count (- end start)
         :received       (subseq sequence start position)))

(declaim (inline safe-read-sequence))
(defun safe-read-sequence (context sequence stream &key (start 0) end)
  (let* ((end      (or end (length sequence)))
         (position (read-sequence sequence stream :start start :end end)))
    (unless (= position end)
      (%short-read context sequence start end position))
    position))

(declaim (inline discard-bytes))
(defun discard-bytes (stream length)
  (safe-read-sequence "discarding input" **discard-buffer** stream
                      :end length))

(defun safe-write-sequence (context sequence stream)
  (log:debug "~@<Writing~@[ ~A~]~:@_~
              ~,,,16@:/utilities.binary-dump:print-binary-dump/~@:>"
             context sequence)
  (write-sequence sequence stream)
  (force-output stream))

(#+sbcl sb-ext:defglobal #-sbcl defvar **nul-buffer**
        (make-octet-vector (max (message-data-length-limit 0)
                                +group-data-length-limit+)))
(declaim (type (simple-octet-vector #.(max (message-data-length-limit 0)
                                           +group-data-length-limit+)))
         #+sbcl (sb-ext:always-bound **nul-buffer**))

(declaim (inline write-padded-sequence))
(defun write-padded-sequence (sequence stream length)
  (write-sequence sequence stream)
  (let ((remainder (- length (length sequence))))
    (unless (zerop remainder)
      (write-sequence **nul-buffer** stream :end remainder))))

(defun check-length (context length min max stream)
  (cond
    ((logbitp 7 length)
     (error 'failure-result-error
            :context context
            :stream  stream
            :code    (twos-complement length)))
    ((not (<= min length max))
     ;; TODO also protocol-error
     (error "~@<~:[Received a~;~:*When ~A, received a~] length of ~:D ~
             which is not between ~:D and ~:D.~@:>"
            context length min max)))
  length)

(defun read-length-delimited-sequence (context stream min-length max-length)
  (let* ((length (check-length context (read-byte stream)
                               min-length max-length stream))
         (buffer (make-octet-vector length)))
    (safe-read-sequence context buffer stream)
    (log:debug "~@<Read~@[ ~A~]~:@_~
                ~,,,16@:/utilities.binary-dump:print-binary-dump/~@:>"
               context buffer)
    (values buffer length)))

;;; Authentication methods

(declaim (ftype (function (simple-octet-vector array-index)
                          (values list &optional))
                unpack-auth-methods))
(defun unpack-auth-methods (buffer end)
  (loop :for offset :from 0 :below end
     :by +authentication-method-name-length-limit+
     :collect (let ((end (or (position #x20 buffer :start offset) end)))
                (octets-to-ascii buffer :start offset :end end))))

(declaim (ftype (function (list) (values simple-octet-vector &optional))
                pack-auth-methods))
(defun pack-auth-methods (methods)
  (let ((buffer (make-octet-vector +authentication-data-length-limit+)))
    (loop :for offset :from 0 :by +authentication-method-name-length-limit+
       :for method :in methods
       :for name = (network.spread.authentication:method-name method)
       :do (setf (subseq buffer offset (+ offset (length name)))
                 (ascii-to-octets name)))
    buffer))

;;; Error translation

(declaim (inline call-with-communication-error-translation))
(defun call-with-communication-error-translation (thunk stream)
  (with-condition-translation
      ((((and error (not network.spread.base:spread-error))
         generic-communication-error)
        :stream stream))
    (funcall thunk)))
(declaim (notinline call-with-communication-error-translation))

(defmacro with-communication-error-translation ((stream &key inline) &body body)
  `(locally ,@(when inline `((declare (inline call-with-communication-error-translation))))
     (call-with-communication-error-translation (lambda () ,@body) ,stream)))
