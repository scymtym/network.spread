;;;; macros.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defmacro with-connection ((connection-var daemon) &body body)
  "Run BODY with CONNECTION-VAR bound to spread connection to DAEMON."
  (check-type connection-var symbol "a symbol")

  `(let ((,connection-var (connect ,daemon)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,connection-var))))

(defmacro with-group ((connection group) &body body)
  "Run BODY with CONNECTION being a member of the spread group named
GROUP."
  (once-only (connection group)
    `(unwind-protect
          (progn
            (join ,connection ,group)
            ,@body)
       (leave ,connection ,group))))
