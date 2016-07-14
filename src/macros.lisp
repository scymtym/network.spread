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

(defun call-with-group (connection group thunk &key (wait? t))
  (if wait?
      (let+ ((joined? nil)
             (left?   nil)
             ((&flet comsume-until (thunk)
                (loop :until (funcall thunk)
                   :do (receive connection :block? nil)))))
        (hooks:with-handlers
            (((hooks:object-hook connection 'join-hook)
              (lambda (group* members)
                (declare (ignore members))
                (when (string= group* group)
                  (setf joined? t))))
             ((hooks:object-hook connection 'leave-hook)
              (lambda (group* members)
                (declare (ignore members))
                (when (string= group* group)
                  (setf left? t)))))
          (unwind-protect
               (progn
                 (join connection group)
                 (comsume-until (lambda () joined?))
                 (funcall thunk))
            (leave connection group)
            (comsume-until (lambda () left?)))))
      (unwind-protect
           (progn
             (join connection group)
             (funcall thunk))
        (leave connection group))))

(defmacro with-group ((connection group &key (wait? nil wait?-supplied?))
                      &body body)
  "Run BODY with CONNECTION being a member of the spread group named
   GROUP.

   WAIT? controls whether execution of BODY should start immediately
   or only after a join message indicates success of the requested
   join operation. Similarly for the leave operation after executing
   BODY."
  `(call-with-group ,connection ,group (lambda () ,@body)
                    ,@(when wait?-supplied? `(:wait? ,wait?))))
