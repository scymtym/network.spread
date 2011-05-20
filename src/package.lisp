;;; package.lisp --- Package definition cl-spread system.
;;
;; Copyright (C) 2011 Jan Moringen
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

(cl:in-package :cl-user)

(defpackage :spread
  (:shadow :leave)
  (:use
   :cl
   :alexandria
   :iterate
   :bind
   :hooks)

  ;; Types
  (:export
   :octet-vector)

  ;; Conditions
  (:export
   :spread-error
   :spread-error-code

   :simple-spread-error

   :connect-failed
   :spread-error-name

   :message-too-long
   :message-too-long-data)

  ;; Restarts
  (:export
   :retry
   :use-daemon)

  ;; Variables
  (:export
   :+maximum-message-data-length+

   :*default-port*
   :*default-daemon-program*

   :*incoming-stream*
   :*outgoing-stream*)

  (:export
   :connect :disconnect
   :join    :leave
   :send    :receive)

  ;; Class `connection'
  (:export
   :connection
   :connection-name
   :connection-groups)

  ;; Hooks
  (:export
   :join-hook :leave-hook)

  ;; Convenience macros
  (:export
   :with-connection
   :with-group)

  ;; Spread daemon
  (:export
   :start-daemon :stop-daemon
   :with-daemon)

  (:documentation
   "This package contains a Common Lisp interface to the spread group
communication system."))
