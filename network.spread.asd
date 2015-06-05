;;;; network.spread.asd --- System definition for the network.spread system.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

(defparameter +version-revision+ 1
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
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "This system provides a Common Lisp interface to the
spread group communication system."
  :depends-on  (:alexandria
                :let-plus
                :more-conditions

                :nibbles
                :cffi
                :trivial-garbage

                :cl-hooks)
  :components  ((:module     "src"
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

                              (:file       "daemon"
                               :if-feature :sbcl)

                              (:file       "fix-signal-handlers"
                               :if-feature (:and :sbcl (:not :win32))))))

  :in-order-to ((test-op (test-op :network.spread-test))))

(defsystem :network.spread-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "This system provides unit tests for the network.spread system."
  :depends-on  ((:version :network.spread #.(version/string))

                (:version :fiveam         "1.3"))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "connection")
                              (:file       "macros")
                              (:file       "daemon")))))

(defmethod perform ((op     test-op)
                    (system (eql (find-system :network.spread-test))))
  (eval (read-from-string
         "(network.spread:with-daemon
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
