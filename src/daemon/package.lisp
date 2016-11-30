;;;; package.lisp --- package definition for the daemon module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread.daemon
  (:use
   #:cl
   #:alexandria
   #:more-conditions)

  (:import-from #:network.spread.base
   #:retry
   #:use-daemon)

  ;; Conditions
  (:export
   #:failed-to-start-daemon
   #:failed-to-start-daemon-program
   #:failed-to-start-daemon-exit-code
   #:failed-to-start-daemon-options
   #:failed-to-start-daemon-output)

  ;; Restarts
  (:export
   #:retry
   #:use-daemon)

  ;; Variables
  (:export
   #:*default-port*
   #:*default-daemon-program*)

  ;; Spread daemon
  (:export
   #:start-daemon/no-restart #:start-daemon #:start-daemon/retry
   #:stop-daemon
   #:with-daemon)

  (:documentation
   "This package contains functions for configuring and executing the
    Spread daemon as a sub-process."))
