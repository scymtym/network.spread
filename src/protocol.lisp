;;; protocol.lisp --- Protocol of the Common Lisp spread bindings.
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

(defgeneric connect (daemon)
  (:documentation
   "Connect to the spread daemon designated by DAEMON."))

(defgeneric disconnect (connection)
  (:documentation
   "Explicitly disconnect CONNECTION from the spread daemon to which
it is connected."))

(defgeneric join (connection group)
  (:documentation
   "Make CONNECTION a member of the spread group designated by
GROUP."))

(defgeneric leave (connection group)
  (:documentation
   "Remove CONNECTION from the spread group designated by GROUP."))

(defgeneric send (connection destination data)
  (:documentation
   "Send DATA to the spread group designated by DESTINATION within the
spread session in which CONNECTION participates."))

(defgeneric receive (connection
		     &key
		     block?)
  (:documentation
   "Receive and return data that is send to any spread group in which
CONNECTION is a member. If block? is non-nil the call blocks until a
message has been received. Otherwise, the call returns immediately and
may return nil if no message has been received."))
