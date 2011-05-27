;;; conditions.lisp ---
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

(in-package :spread)

(define-condition spread-error (error)
  ((code :initarg  :code
	 :type     keyword
	 :reader   spread-error-code
	 :documentation
	 ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Spread error: ~A.~@:>"
	     (spread-error-code condition))))
  (:documentation
   "This class is used a superclass for all spread-related condition
classes."))

(define-condition simple-spread-error (spread-error
				       simple-error)
  ()
  (:report
   (lambda (condition stream)
     (apply #'format stream
	    (simple-condition-format-control   condition)
	    (simple-condition-format-arguments condition))
     (format stream ": ~A"
	     (spread-error-code condition))))
  (:documentation
   "Condition instances of this class contain a simple problem
description and a spread error code."))

(define-condition connect-failed (spread-error)
  ((name :initarg  :name
	 :type     string
	 :reader   spread-error-name
	 :documentation
	 ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to connect to the spread daemon ~
designated by ~S: ~A.~@:>"
	     (spread-error-name condition)
	     (spread-error-code condition))))
  (:documentation
   "This condition is signaled when establishing a connection to the
spread daemon fails."))

(define-condition message-too-long (spread-error)
  ((data :initarg  :data
	 :type     octet-vector
	 :reader   message-too-long-data
	 :documentation
	 "The data that caused the error."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Data is ~D octets long, which longer than the ~
maximum message length ~D.~@:>"
	     (length (message-too-long-data condition))
	     +maximum-message-data-length+)))
  (:documentation
   "This error is signaled when an attempt is made to send an
octet-vector which has more than `+maximum-message-data-length+'
octets."))


;;; Daemon-related conditions
;;

(define-condition failed-to-start-daemon (spread-error)
  ((program   :initarg  :program
	      :type     string
	      :reader   failed-to-start-daemon-program
	      :documentation
	      "The name of the Spread daemon executable that was
used.")
   (exit-code :initarg  :exit-code
	      :type     integer
	      :reader   failed-to-start-daemon-exit-code
	      :documentation
	      "The exit code returned by the Spread daemon.")
   (options   :initarg  :options
	      :type     list
	      :reader   failed-to-start-daemon-options
	      :documentation
	      "The configuration with which the Spread daemon was
started.")
   (output    :initarg  :output
	      :type     string
	      :reader failed-to-start-daemon-output
	      :documentation
	      "The output (concatenation of standard output and
standard error) of the Spread daemon."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Spread daemon (executable ~S) failed to start ~
with exit code ~D. Options were ~_~{~{~A = ~S~}~^, ~_~}. ~_Spread ~
said: ~&~A~@:>"
	     (failed-to-start-daemon-program   condition)
	     (failed-to-start-daemon-exit-code condition)
	     (failed-to-start-daemon-options   condition)
	     (failed-to-start-daemon-output    condition))))
  (:documentation
   "This error is signaled when an attempt to start the Spread daemon
fails."))
