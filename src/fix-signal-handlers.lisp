;;;; fix-signal-handlers.lisp --- Undo Spread's changes to SBCL's signal handlers.
;;;;
;;;; Copyright (C) 2011-2016, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; http://jcsu.jesus.cam.ac.uk/~csr21/papers/features.pdf
(eval-when (:compile-toplevel :execute)
  (when (and (find-package '#:sb-unix)
             (find-symbol (string '#:sigpipe-handler) '#:sb-unix))
    (pushnew 'sb-unix-sigpipe-handler *features*)))

(defun fix-signal-handlers ()
  ;; Restore signal handlers for SIGINT and SIGPIPE to their original
  ;; states.
  (sb-unix::enable-interrupt sb-unix::sigint  #'sb-unix::sigint-handler)
  #+network.spread::sb-unix-sigpipe-handler
  (sb-unix::enable-interrupt sb-unix::sigpipe #'sb-unix::sigpipe-handler))

(defmethod connect :around ((daemon t))
  ;; After the Spread connection to DAEMON has been created, restore
  ;; the signal handlers of the current thread to their original
  ;; state.
  (unwind-protect
       (call-next-method)
    (fix-signal-handlers)))
