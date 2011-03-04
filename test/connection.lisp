;;; connection.lisp ---
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

(deftestsuite connection-root (root)
  ()
  (:documentation
   "Units test for the `connection' class and `connect' method."))

(addtest (connection-root
          :documentation
	  "Smoke test for the `connect' method.")
  smoke

  (let ((connection (spread:connect daemon)))
    (unwind-protect
	 (progn
	   )
      (disconnect connection))))
