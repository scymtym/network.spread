;;; cl-spread.asd --- System definition for the cl-spread system.
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

(cl:defpackage :cl-spread-system
  (:use
   :cl
   :asdf))

(cl:in-package :cl-spread-system)

#+sbcl (asdf:load-system :sb-posix)

(defsystem :cl-spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system provides a Common Lisp interface to the
spread group communication system."
  :depends-on  (:alexandria
		:iterate
		:metabang-bind ;; ok?
		:cffi
		:trivial-garbage
		:cl-hooks)
  :properties  ((:default-port           . #.(or #+sbcl (let ((value (sb-posix:getenv "SPREAD_PORT")))
							  (when value (read-from-string value)))
						 4803))
		(:default-daemon-program . #.(or #+sbcl (sb-posix:getenv "SPREAD_DAEMON_PROGRAM")
						 #+sbcl (let ((root (sb-posix:getenv "SPREAD_ROOT")))
							  (when root
							    (format nil "~A/sbin/spread" root)))
						 "spread")))
  :components  ((:module     "src"
		 :components ((:file       "package")
			      (:file       "types"
			       :depends-on ("package"))
			      (:file       "conditions"
			       :depends-on ("package" "variables"))
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "ffi"
			       :depends-on ("package" "types"))
			      (:file       "variables"
			       :depends-on ("package" "ffi"))
			      (:file       "connection"
			       :depends-on ("package" "types"
					    "conditions" "ffi"
					    "protocol" "variables"))
			      (:file       "macros"
			       :depends-on ("package" "connection"))

			      #+sbcl
			      (:file       "daemon"
			       :depends-on ("package" "conditions"
					    "variables"))

			      #+(and sbcl (not win32))
			      (:file       "fix-signal-handlers"
			       :depends-on ("protocol")))))

  :in-order-to ((test-op (test-op :cl-spread-test))))

(defsystem :cl-spread-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system provides unit tests for the cl-spread system."
  :depends-on  ((:version :cl-spread "0.1.0")
		(:version :lift      "1.7.1"))
  :properties  ((:port . 6789))
  :components  ((:module     "test"
		 :components ((:file       "package")
			      (:file       "connection"
			       :depends-on ("package"))
			      (:file       "macros"
			       :depends-on ("package"))
			      (:file       "variables"
			       :depends-on ("package"))
			      (:file       "daemon"
			       :depends-on ("package"))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-spread-test))))
  (eval (read-from-string
	 "(SPREAD:WITH-DAEMON (:PORT (ASDF:COMPONENT-PROPERTY
                                       (ASDF:FIND-SYSTEM :CL-SPREAD-TEST) :PORT))
            (LIFT:RUN-TESTS :CONFIG :GENERIC))")))
