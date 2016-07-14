;;;; macros.lisp --- Macros provided by the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defun call-with-connection (daemon thunk)
  (let ((connection (connect daemon)))
    (unwind-protect
         (funcall thunk connection)
      (disconnect connection))))

(defmacro with-connection ((connection-var daemon) &body body)
  "Run BODY with CONNECTION-VAR bound to spread connection to DAEMON."
  (check-type connection-var symbol "a symbol")
  `(call-with-connection ,daemon (lambda (,connection-var) ,@body)))

(defun call-with-group (connection group thunk)
  (unwind-protect
       (progn
         (join connection group)
         (funcall thunk))
    (leave connection group)))

(defmacro with-group ((connection group) &body body)
  "Run BODY with CONNECTION being a member of the spread group named
   GROUP."
  `(call-with-group ,connection ,group (lambda () ,@body)))
