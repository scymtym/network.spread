;;;; network.spread.asd --- System definition for the network.spread system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:network.spread-system
  (:use
   #:cl
   #:asdf)

  ;; Version stuff
  (:export
   #:version/list
   #:version/string)

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

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 2
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list (&key
                     (revision? t))
  "Return a version of the form (MAJOR MINOR [REVISION]) where
REVISION is optional.

REVISION? controls whether REVISION should be included. Default
behavior is to include REVISION."
  (append (list +version-major+ +version-minor+)
          (when revision? (list +version-revision+))))

(defun version/string (&rest args
                       &key
                       revision?)
  "Return a version string of the form
\"MAJOR.MINOR[.REVISION]\" where REVISION is optional.

See `version/list' for details on keyword parameters."
  (declare (ignore revision?))
  (format nil "~{~A.~A~^.~A~}" (apply #'version/list args)))

;;; System definition

(defsystem :network.spread
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides a Common Lisp interface to the
spread group communication system."
  #+sbcl :defsystem-depends-on #+sbcl (:sb-posix)
  :depends-on  (:alexandria
                :iterate
                :let-plus

                :nibbles
                :cffi
                :trivial-garbage

                :cl-hooks)
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

                              (:file       "reloading"
                               :depends-on ("ffi"))

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
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides unit tests for the network.spread system."
  :depends-on  ((:version :network.spread #.(version/string))

                (:version :lift           "1.7.1"))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "connection")
                              (:file       "macros")
                              (:file       "daemon")))))

(defmethod perform ((op test-op) (system (eql (find-system :network.spread-test))))
  (eval (read-from-string
         "(NETWORK.SPREAD:WITH-DAEMON
              (:PORT NETWORK.SPREAD-SYSTEM:*TEST-PORT*)
            (LIFT:RUN-TESTS :CONFIG :GENERIC))")))

;;; Configuration stuff

(defun spread-library-pathname ()
  "Interpret hint as to where the Spread library can be found."
  (or #+sbcl (sb-posix:getenv "SPREAD_LIBRARY")
      nil))

(defparameter *spread-library-pathname* (spread-library-pathname)
  "The pathname of the Spread library. nil or relative pathnames
indicate that the operating system's facilities should be used to
locate the library.")

(defparameter *default-port*
  (or #+sbcl (let ((value (sb-posix:getenv "SPREAD_PORT")))
               (when value (read-from-string value)))
      4803)
  "The default port on which the Spread daemon should listen when it
is started via `network.spread:start-daemon' or
`network.spread:with-daemon'.")

(defparameter *default-daemon-program*
  (or #+sbcl (sb-posix:getenv "SPREAD_DAEMON_PROGRAM")
      #+sbcl (let ((root (sb-posix:getenv "SPREAD_ROOT")))
               (when root
                 (format nil "~A/sbin/spread" root)))
      "spread")
  "The default name of the program that should be executed when
starting the Spread daemon.")

(defparameter *test-port* 6789
  "The port on which the Spread daemon should listen during tests.")
