;;;; network.spread.asd --- System definition for the network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:network.spread-system)

#+sbcl (asdf:load-system :sb-posix)

(defsystem :network.spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "LLGPLv3; see COPYING file for details."
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

  :in-order-to ((test-op (test-op :network.spread-test))))

(defsystem :network.spread-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides unit tests for the network.spread system."
  :depends-on  ((:version :network.spread "0.2.0")

		(:version :lift           "1.7.1"))
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

(defmethod perform ((op test-op) (system (eql (find-system :network.spread-test))))
  (eval (read-from-string
	 "(NETWORK.SPREAD:WITH-DAEMON
              (:PORT (ASDF:COMPONENT-PROPERTY
                       (ASDF:FIND-SYSTEM :NETWORK.SPREAD-TEST) :PORT))
            (LIFT:RUN-TESTS :CONFIG :GENERIC))")))
