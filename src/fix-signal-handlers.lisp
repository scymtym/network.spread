;;;; fix-signal-handlers.lisp --- Undo Spread's changes to SBCL's signal handlers.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

(defun fix-signal-handlers ()
  ;; Restore signal handlers for SIGINT and SIGPIPE to their original
  ;; states.
  (sb-unix::enable-interrupt sb-unix::sigint  #'sb-unix::sigint-handler)
  (sb-unix::enable-interrupt sb-unix::sigpipe #'sb-unix::sigpipe-handler))

(defmethod connect :around ((daemon t))
  ;; After the Spread connection to DAEMON has been created, restore
  ;; the signal handlers of the current thread to their original
  ;; state.
  (unwind-protect
       (call-next-method)
    (fix-signal-handlers)))
