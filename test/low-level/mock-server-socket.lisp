;;;; mock-server-socket.lisp --- Socket interface for mock servers.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.low-level.test)

(defun call-with-collecting-failures (thunk test)
  (locally (declare (sb-ext:disable-package-locks 5am::current-test 5am::result-list))
    (let ((5am::current-test test)
          (5am::result-list '()))
      (declare (special 5am::current-test 5am::result-list))
      (handler-bind ((5am::check-failure
                      (lambda (condition)
                        (declare (ignore condition))
                        (invoke-restart '5am::ignore-failure)))
                     (error (lambda (c) (log:info c 5am::result-list) (abort)) #+later #'abort))
        (funcall thunk))
      5am::result-list)))

(defun signal-failures (failures)
  (mapc #'signal failures))

(defmacro collecting-failures ((test) &body body)
  `(call-with-collecting-failures (lambda () ,@body) ,test))

(defun call-with-mock-server
    (thunk mock-server-function &rest mock-server-args)
  (let* ((port      nil)
         (lock      (bt:make-lock "port lock"))
         (condition (bt:make-condition-variable :name "port condition"))
         (test      (locally (declare (sb-ext:disable-package-locks 5am::current-test))
                      (locally (declare (special 5am::current-test))
                        5am::current-test)))
         (server                        ; TODO macro
          (bt:make-thread
           (lambda ()
             (collecting-failures (test)
               (usocket:with-socket-listener (socket "0.0.0.0" 0)
                 (bt:with-lock-held (lock)
                   (setf port (usocket:get-local-port socket))
                   (bt:condition-notify condition))
                 (let+ ((client-socket (usocket:socket-accept
                                        socket :element-type 'octet))
                        (stream        (usocket:socket-stream client-socket))
                        ((&flet read- (amount k)
                           (let ((buffer (make-octet-vector amount)))
                                        ; (log:info "about to read ~D octet~:P" amount)
                             (assert (= amount (read-sequence buffer stream)))
                                        ; (print :inp)
                                        ; (utilities.binary-dump:binary-dump buffer :base 16)
                                        ; (terpri)
                             (funcall k buffer))))
                        ((&flet write- (sequence)
                                        ; (print :out)
                                        ; (utilities.binary-dump:binary-dump (coerce sequence 'octet-vector) :base 16)
                                        ; (terpri)
                           (write-sequence sequence stream)
                           (force-output stream))))
                   (unwind-protect
                        (apply mock-server-function #'read- #'write- mock-server-args)
                     (usocket:socket-close client-socket)))))))))
    (unwind-protect
         (progn
           (bt:with-lock-held (lock)
             (loop :until port :do (bt:condition-wait condition lock)))
           (funcall thunk port))
      (locally (declare (sb-ext:disable-package-locks 5am::result-list))
        (locally (declare (special 5am::result-list))
          (appendf 5am::result-list (bt:join-thread server)))))))

(defmacro with-mock-server ((mock-server-function &rest mock-server-args)
                            (port-var) &body body)
  `(call-with-mock-server (lambda (,port-var) ,@body)
                          ,mock-server-function ,@mock-server-args))

(defun call-with-mock-connection
    (thunk mock-server-function &rest mock-server-args)
  (apply #'call-with-mock-server
         (lambda (port)
           (let* ((socket  (usocket:socket-connect "localhost" port :element-type 'octet))
                  (stream  (usocket:socket-stream socket))
                  (mailbox (make-mailbox socket stream (octet-vector 1 2 3)))) ; TODO priv group
             (funcall thunk mailbox)))
         mock-server-function mock-server-args))

(defmacro with-mock-connection ((mock-server-function &rest mock-server-args)
                                (mailbox-var) &body body)
  `(call-with-mock-connection (lambda (,mailbox-var) ,@body)
                              ,mock-server-function ,@mock-server-args))
