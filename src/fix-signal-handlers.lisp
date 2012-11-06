;;; fix-signal-handlers.lisp --- Undo Spread's changes to SBCL's signal handlers.
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

(defun fix-signal-handlers ()
  "Restore signal handlers for SIGINT and SIGPIPE to their original
states."
  (sb-unix::enable-interrupt sb-unix::sigint  #'sb-unix::sigint-handler)
  (sb-unix::enable-interrupt sb-unix::sigpipe #'sb-unix::sigpipe-handler))

(defmethod connect :after ((daemon t))
  "After the Spread connection to DAEMON has been created, restore the
signal handlers of the current thread to their original state."
  (fix-signal-handlers))
