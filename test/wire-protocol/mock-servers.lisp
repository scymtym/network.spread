;;;; mock-servers.lisp --- Mock servers for testing clients.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol.test)

;;; Utilities

(defun maybe-ascii-to-octets (thing)
  (etypecase thing
    (octet-vector thing)
    (string       (ascii-to-octets thing))))

(defun write-padded-sequence* (writer sequence length)
  (funcall writer sequence)
  (let ((remainder (nth-value 1 (ceiling (length sequence) length))))
    (unless (zerop remainder)
      (funcall writer (make-octet-vector (- remainder))))))

;;;

(defmacro define-mock-server-function (name-and-options lambda-list &body body)
  (let+ (((name &optional
                (read-sequence-name  'read-sequence*)
                (write-sequence-name 'write-sequence*))
          (ensure-list name-and-options)))
    (with-unique-names (read-from-server-var write-to-server-var)
      `(cont:defun/cc ,name (,read-from-server-var ,write-to-server-var
                             ,@lambda-list)
         (mock-server-coroutine ((,read-from-server-var ,read-sequence-name)
                                 (,write-to-server-var  ,write-sequence-name))
           ,@body)))))

;;; Connection management

(declaim (ftype function
                mock-server/greeting mock-server/finalize-connect
                mock-server/authentication))

(defun default-server-version ()
  (list *major-version* *minor-version* *patch-version*))

(define-mock-server-function mock-server/greeting (&key
                                                   private-group
                                                   (membership? t)
                                                   (priority    0))
  (let* ((expected-fixed (octet-vector *major-version*
                                       *minor-version*
                                       *patch-version*
                                       (dpb priority (byte 1 4)
                                            (if membership? 1 0))
                                       (if private-group
                                           (length private-group)
                                           0)))
         (expected       (concatenate 'octet-vector
                                      expected-fixed private-group))
         (buffer         (make-octet-vector (length expected))))
    (read-sequence* buffer)
    (cont:without-call/cc
      (is (equalp expected buffer))
      (when private-group
        (is (equalp private-group (subseq buffer 5)))))))

(define-mock-server-function mock-server/finalize-connect
    (&key
     (send-version       (default-server-version))
     (send-private-group (missing-required-argument :send-private-group)))
  (write-sequence* send-version)
  (let ((private-group (maybe-ascii-to-octets send-private-group)))
    (write-sequence* (octet-vector (length private-group)))
    (write-sequence* private-group)))

(cont:defun/cc mock-server/connect
    (read-from-server write-to-server
                      &key
                      private-group
                      (membership?         t)
                      (priority            0)

                      (auth-methods        '("NULL"))
                      (auth-methods-length nil auth-methods-length-supplied?)
                      (auth-result         1)

                      (send-version       (default-server-version))
                      (send-private-group private-group))
  (mock-server-coroutine ((read-from-server read-sequence*)
                          (write-to-server  write-sequence*))
    (mock-server/greeting
     read-from-server write-to-server
     :private-group private-group :membership? membership? :priority priority)
    (apply #'mock-server/authentication
           read-from-server write-to-server
           :methods auth-methods  :result auth-result
           (when auth-methods-length-supplied?
             (list :methods-length auth-methods-length)))
    (mock-server/finalize-connect
     read-from-server write-to-server
     :send-version send-version :send-private-group send-private-group)))

(define-mock-server-function mock-server/disconnect ()
  (let ((buffer (make-octet-vector (+ 48 +group-name-length-limit+))))
    (read-sequence* buffer))) ; TODO

;;; Client authentication

(define-mock-server-function mock-server/authentication
    (&key
     (methods        '())
     (methods-length (reduce #'+ methods
                             :key           #'length
                             :initial-value (length methods)))
     (result         1))
  (when (minusp methods-length)
    (write-sequence* (octet-vector (+ methods-length 256)))
    (return-from mock-server/authentication))

  (write-sequence* (octet-vector methods-length))
  (write-sequence* (ascii-to-octets (format nil "~{~A ~}" methods)))
  (let ((buffer (make-octet-vector +authentication-data-length-limit+)))
    (read-sequence* buffer))
  (write-sequence* (octet-vector result)))

;;; Send and receiving messages

(define-mock-server-function mock-server/send ()
  (let ((header (make-octet-vector 48)))
    (read-sequence* header)
    (let* ((group-count    (ub32ref/le header 36))
           (payload-length (ub32ref/le header 44))
           (groups         (make-octet-vector
                            (* group-count +group-name-length-limit+)))
           (payload        (make-octet-vector payload-length)))
      (read-sequence* groups)
      (when (plusp payload-length)
        (read-sequence* payload)))))

(define-mock-server-function mock-server/receive
    (&key                               ; TODO both endians
     (sender       (missing-required-argument :sender))
     (service-type (missing-required-argument :service-type))
     (groups       (missing-required-argument :groups))
     (message-type 0)
     (payload      (octet-vector)))
  (let ((endian-marker (ecase :little ; endian
                         (:little 'network.spread.wire-protocol::endian-mark-little)
                         (:big    'identity)))
        (header        (make-octet-vector 48)))
    (setf (ub32ref/le header  0) (funcall endian-marker service-type)
          (subseq header 4)      (maybe-ascii-to-octets sender)
          (ub32ref/le header 36) (length groups)
          (ub32ref/le header 40) (funcall endian-marker (ash message-type 8))
          (ub32ref/le header 44) (length payload))
    (write-sequence* header))
  (map nil (lambda (group)
             (write-padded-sequence*
              #'write-sequence*
              (maybe-ascii-to-octets group)
              +group-name-length-limit+))
       groups)
  (write-sequence* payload))

;;; Group membership operations

(define-mock-server-function mock-server/join ()
  (let ((buffer (make-octet-vector (+ 48 +group-name-length-limit+))))
    (read-sequence* buffer)))

(define-mock-server-function mock-server/leave ()
  (let ((buffer (make-octet-vector (+ 48 +group-name-length-limit+))))
    (read-sequence* buffer)))
