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

(define-condition connect-failed (spread-error)
  ((name :initarg  :name
	 :type     string
	 :reader   spread-error-name
	 :documentation
	 ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to connect to spread daemon ~S: ~A.~@:>"
	     (spread-error-name condition)
	     (spread-error-code condition))))
  (:documentation
   "This condition is signaled when establishing a connection to the
spread daemon fails."))
