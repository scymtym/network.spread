;;;; package.lisp --- Package definition network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:nibbles
   #:hooks)

  (:import-from #:network.spread.base
   #:spread-error

   #:retry
   #:use-daemon

   #:ascii-to-octets
   #:octets-to-ascii)

  (:shadow
   #:leave)

  ;; Conditions
  (:export
   #:spread-error

   #:spread-client-error
   #:spread-error-code

   #:connect-failed
   #:spread-error-name

   #:message-too-long
   #:message-too-long-data

   #:group-too-long-error
   #:group-too-long-error-group)

  ;; Restarts
  (:export
   #:retry
   #:use-daemon)

  ;; Variables
  (:export
   #:+maximum-message-data-length+
   #:+maximum-group-name-length+)

  ;; Utilities
  (:export
   #:parse-daemon-name)

  ;; Spread protocol
  (:export
   #:connect #:disconnect

   #:join    #:leave

   #:send    #:receive #:receive-into)

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

  ;; Reloading of the Spread library
  (:export
   #:use-spread-library

   #:unload-spread-library
   #:reload-spread-library

   #:enable-reload-spread-library
   #:disable-reload-spread-library)

  (:documentation
   "This package contains a Common Lisp interface to the spread group
    communication system.

    The primary protocol consists of the following functions:

    * `connect'                         [generic function]
      Connect to a Spread daemon.

    * `disconnect'                      [generic function]
      Disconnect from a Spread daemon.

    * `join'                            [generic function]
      Join a Spread multicast group.

    * `leave'                           [generic function]
      Leave a Spread multicast group.

    * `send'                            [generic function]
      Send data to one or more Spread groups.

    * `receive'                         [generic function]
      Receive data into a freshly consed buffer.

    * `receive-into'                    [generic function]
      Receive data into an existing buffer."))
