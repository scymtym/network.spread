;;; macros.lisp --- Unit tests for macros provided by the cl-spread system.
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

(in-package :spread.test)

(deftestsuite macros-root (root)
  ()
  (:documentation
   "Unit test for the convenience macros provided by the cl-spread
system."))

(deftestsuite with-connection-root (macros-root)
  ()
  (:documentation
   "Unit tests for the `with-connection' macro."))

(addtest (with-connection-root
          :documentation
	  "Smoke test for the `with-connection' macro.")
  smoke

  (with-connection (connection daemon)
    (ensure connection)))

(deftestsuite with-group-root (macros-root)
  ()
  (:documentation
   "Unit tests for the `with-group' macro."))

(addtest (with-group-root
          :documentation
	  "Smoke test for the `with-group' macro.")
  smoke

  (with-connection (connection daemon)
    (with-group (connection "foo")
      (ensure-same
       (connection-groups connection) '("foo")
       :test #'equal))

    (ensure-null (connection-groups connection))))
