;;;; package.lisp --- Package definition network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus

   #:nibbles
   #:hooks)

  (:shadow
   #:leave)

  ;; Conditions
  (:export
   #:spread-error
   #:spread-error-code

   #:simple-spread-error

   #:connect-failed
   #:spread-error-name

   #:message-too-long
   #:message-too-long-data

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
   #:+maximum-message-data-length+

   #:*default-port*
   #:*default-daemon-program*)

  ;; Spread protocol
  (:export
   #:connect #:disconnect

   #:join    #:leave

   #:send    #:receive)

  ;; Protocol for sending raw bytes
  (:export
   #:send-bytes)

  ;; Class `connection'
  (:export
   #:connection
   #:connection-daemon-name
   #:connection-name
   #:connection-groups)

  ;; Hooks
  (:export
   #:join-hook #:leave-hook)

  ;; Convenience macros
  (:export
   #:with-connection
   #:with-group)

  ;; Spread daemon
  (:export
   #:start-daemon/no-restart #:start-daemon #:start-daemon/retry
   #:stop-daemon
   #:with-daemon

   #:parse-daemon-name)

  (:documentation
   "This package contains a Common Lisp interface to the spread group
communication system."))
