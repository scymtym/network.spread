;;; package.lisp --- Package definition for unit tests of the network.spread system.
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

(cl:defpackage #:network.spread.test
  (:use
   #:cl
   #:alexandria
   #:bind
   #:lift

   #:network.spread)

  (:import-from #:network.spread
   #:+max-group-name+)

  (:export
   #:root)

  (:documentation
   "This package contains unit tests for the network.spread system"))

(cl:in-package #:network.spread.test)

(deftestsuite root ()
  (port
   daemon)
  (:setup
   (setf port   (asdf:component-property
		 (asdf:find-system :network.spread-test) :port)
	 daemon (format nil "~D" port)))
  (:timeout 20)
  (:documentation
   "Root unit test suite for the network.spread system."))
