;;; variables.lisp --- Global/special variables used in network.spread.
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

(cl:in-package #:network.spread)


;;;
;;

(defconstant +maximum-message-data-length+ +max-message+
  "The maximum length (in bytes) of the data in a spread message.")


;;;
;;

(declaim (special *incoming-stream*))

(defvar *incoming-stream* nil
  "When this special variable is bound to an output-stream, incoming
data is printed onto that stream using a slightly hexdump-like
format.")

(declaim (special *outgoing-stream*))

(defvar *outgoing-stream* nil
  "When this special variable is bound to an output-stream, outgoing
data is printed onto that stream using a slightly hexdump-like
format.")


;;; Daemon-related parameters
;;

(defparameter *default-port* (or (asdf:component-property
				  (asdf:find-system :network.spread)
				  :default-port)
				 4803)
  "The default port on which the Spread should listen when it is
started via `start-daemon' or `with-daemon'.")

(defparameter *default-daemon-program* (or (asdf:component-property
					    (asdf:find-system :network.spread)
					    :default-daemon-program)
					   "spread")
  "The default name of the program that should be executed when
starting the Spread daemon.")
