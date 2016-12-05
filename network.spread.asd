;;;; network.spread.asd --- System definition for the network.spread system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread-system
  (:use
   #:cl
   #:asdf)

  ;; Configuration stuff
  (:export
   #:spread-library-pathname
   #:*spread-library-pathname*

   #:*default-port*
   #:*default-daemon-program*)

  ;; Test configuration stuff
  (:export
   #:*test-port*))

(cl:in-package #:network.spread-system)

;;; System definition

(defsystem :network.spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "This system provides a Common Lisp interface to the
spread group communication system."
  :depends-on  (:alexandria
                :let-plus
                :more-conditions
                :log4cl

                :nibbles
                :cffi
                :trivial-garbage

                (:version :architecture.service-provider "0.2")
                :cl-hooks)
  :components  ((:module     "base"
                 :pathname   "src/base"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "conditions")))

                (:module     "authentication"
                 :pathname   "src/authentication"
                 :depends-on ("base")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "method-null")))

                (:module     "src"
                 :depends-on ("base")
                 :serial     t
                 :components ((:file       "package")

                              (:file       "types")
                              (:file       "ffi")
                              (:file       "low-level")

                              (:file       "variables")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "connection")
                              (:file       "macros")

                              (:file       "reloading")

                              (:file       "fix-signal-handlers"
                                           :if-feature (:and :sbcl (:not :win32)))))

                (:module     "daemon"
                 :pathname   "src/daemon"
                 :depends-on ("base")
                 :serial     t
                 :components ((:file      "package")
                              (:file      "variables")
                              (:file      "conditions")
                              (:file      "daemon"))
                 :if-feature :sbcl))

  :in-order-to ((test-op (test-op :network.spread/test))))

(defsystem :network.spread/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "This system provides unit tests for the network.spread system."
  :depends-on  (:more-conditions

                :cl-cont

                (:version :network.spread (:read-file-form "version-string.sexp"))

                (:version :fiveam         "1.3"))
  :components  ((:module     "base"
                 :pathname   "test/base"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")))

                (:module     "test"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "mock-server-coroutine")

                              (:file       "connection")
                              (:file       "macros")))

                (:module     "authentication"
                 :pathname   "test/authentication"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")))

                (:module     "daemon"
                 :pathname   "test/daemon"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "daemon")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :network.spread/test))))
  (eval (read-from-string
         "(network.spread.daemon:with-daemon
              (:port network.spread-system:*test-port*)
            (network.spread.test:run-tests))")))

;;; Configuration stuff

(defun spread-library-pathname ()
  "Interpret hint as to where the Spread library can be found."
  (or (uiop:getenv "SPREAD_LIBRARY") nil))

(defparameter *spread-library-pathname* (spread-library-pathname)
  "The pathname of the Spread library. nil or relative pathnames
indicate that the operating system's facilities should be used to
locate the library.")

(defparameter *default-port*
  (or (let ((value (uiop:getenv "SPREAD_PORT")))
        (when value (read-from-string value)))
      4803)
  "The default port on which the Spread daemon should listen when it
is started via `network.spread:start-daemon' or
`network.spread:with-daemon'.")

(defparameter *default-daemon-program*
  (or (uiop:getenv "SPREAD_DAEMON_PROGRAM")
      (let ((root (uiop:getenv "SPREAD_ROOT")))
        (when root
          (format nil "~A/sbin/spread" root)))
      "spread")
  "The default name of the program that should be executed when
starting the Spread daemon.")

(defparameter *test-port* 6789
  "The port on which the Spread daemon should listen during tests.")
