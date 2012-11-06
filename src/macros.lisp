;;; macros.lisp ---
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

(defmacro with-connection ((connection-var daemon) &body body)
  "Run BODY with CONNECTION-VAR bound to spread connection to DAEMON."
  (check-type connection-var symbol "a symbol")

  `(let ((,connection-var (connect ,daemon)))
     (unwind-protect
	  (progn ,@body)
       (disconnect ,connection-var))))

(defmacro with-group ((connection group) &body body)
  "Run BODY with CONNECTION being a member of the spread group named
GROUP."
  (once-only (connection group)
    `(unwind-protect
	  (progn
	    (join ,connection ,group)
	    ,@body)
       (leave ,connection ,group))))
