;;; daemon.lisp --- Unit tests for daemon-related functions.
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

(deftestsuite daemon-suite (root)
  ()
  (:documentation
   "Test suite for daemon-related functions."))

(addtest (daemon-suite
          :documentation
	  "Smoke test for starting and stopping a spread daemon using
`start-daemon' and `stop-daemon' respectively.")
  smoke

  (let ((daemon (start-daemon :port (+ port 3))))
    (ensure (not (null daemon)))
    (stop-daemon daemon)))

(addtest (daemon-suite
          :documentation
	  "Check that errors such as starting two spread daemons with
identical ports are detected and reported.")
  startup-failure

    (with-daemon (:port (+ port 6))
      (ensure-condition 'error
	(with-daemon (:port (+ port 6))))))
