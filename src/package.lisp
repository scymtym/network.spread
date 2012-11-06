;;; package.lisp --- Package definition network.spread system.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage #:network.spread
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:bind
   #:hooks)

  (:shadow
   #:leave)

  ;; Types
  (:export
   #:octet-vector)

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
   #:*default-daemon-program*

   #:*incoming-stream*
   #:*outgoing-stream*)

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
