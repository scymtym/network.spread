;;;; constants.lisp --- Constants used in the Spread wire protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol)

;;; Spread version

(defparameter *major-version* 4)

(defparameter *minor-version* 4)

(defparameter *patch-version* 0)

(defparameter *acceptable-daemon-versions*
  '(or (cons (eql 3)     (cons (integer 8) (cons integer null)))
       (cons (integer 4) (cons integer     (cons integer null)))))

(defparameter *acceptable-daemon-versions-description*
  "~@<either a version 3.8.* or major version 4 and above~@:>")

;;; Error codes
;;;
;;; This is a subset of the error codes defined in the Spread header
;;; files because we are only interested in constants sent over the
;;; network as part of the wire protocol, not in constants used as
;;; return values to indicate errors of functions in the Spread C API.

(eval-when (:compile-toplevel :load-toplevel)
  (defstruct (result (:constructor make-result (code description))
                     (:predicate nil)
                     (:copier nil))
    (code        nil :type (signed-byte 8) :read-only t)
    (description nil :type string          :read-only t)))

(macrolet
    ((define-result-constants ()
       (labels
           ((expand-constants (info)
              (let+ (((&flet+ make-constant ((code description &optional name))
                        (when name
                          (let ((name (symbolicate '#:+result- name'#:+)))
                            `((define-constant ,name
                                  ,code
                                :test #'equalp
                                :documentation ,description)))))))
                (mappend #'make-constant info)))
            (expand-results (info)
              `(define-constant +results+
                   (vector
                    ,@(mapcar
                       (lambda+ ((code description &optional &ign))
                         `(make-result ,code ,description))
                       info))
                 :test #'equalp))
            (expand (info)
              `(progn
                 ,@(expand-constants info)
                 ,(expand-results info))))
         (expand
          `(( 0 "No error"         #:no-error)
            ( 1 "Session accepted" #:session-accepted)

            (-3 "Connection rejected, too many users")
            (-4 "Connection rejected, no name was supplied")
            (-5 "Connection rejected, illegal name")
            (-6 "Connection rejected, name not unique")
            (-7 "Connection rejected, library does not fit daemon")
            (-8 "Connection closed by Spread")
            (-9 "Connection rejected, authentication failed"))))))

  (define-result-constants))

;;; Sizes of message parts

(defconstant +group-name-length-limit+ 32
  "The maximum length (in octets) of a Spread group name.")

(defconstant +group-count-limit+ 1024
  "The maximum number of groups in a messages.")

(defconstant +group-data-length-limit+
  (* +group-name-length-limit+ +group-count-limit+)
  "The maximum length (in octets) of a group name list, i.e. a
   consecutively stored padded group names, in a message.")

(defconstant +authentication-method-name-length-limit+ 30
  "The maximum length (in octets) of a authentication method name.")

(defconstant +authentication-method-count-limit+ 3
  "The maximum number of authentication method names in a message.")

(defconstant +authentication-data-length-limit+
  (* +authentication-method-name-length-limit+
     +authentication-method-count-limit+)
  "The maximum length (in octets) of a authentication method name
   list, i.e. a consecutively stored padded authentication method
   names, in an authentication-related message.")

(defconstant +message-length-limit+
  ;; (* max-scatter-elements (- packet-size packet-header-size))
  ;; where supposedly
  ;;
  ;;   max-scatter-elements = 100
  ;;   packet-size          = 1472
  ;;   packet-header-size   = 32
  ;;
  ;; but using (* 100 (- 1472 32)) permits message sizes that crash
  ;; the daemon (likely due to an off-by-one error in an assertion).
  (* 99 (- 1472 32))
  "The maximum length (in octets) of a Spread message.

   This limits the combined length of the payload and all destination
   groups in a message.")

(declaim (inline message-data-length-limit))
(defun message-data-length-limit (group-count)
  "The maximum length of a message payload given GROUP-COUNT groups."
  (- +message-length-limit+ (* group-count +group-name-length-limit+)))

;;; Endian marker

(defconstant +little-endian-marker+ #x80000080)

(defconstant +little-endian-marker-test-bit+ 7)

;;; Message header slots

(define-constant +header-slot-lengths+
    `(4 ,+group-name-length-limit+ 4 4 4)
  :test #'equal)

(defmacro header-slot-offset (index)
  (reduce #'+ +header-slot-lengths+ :end index :initial-value 0))

;;; Message types

(defmacro define-bitfield ((prefix) &body values)
  (let+ ((current-index 0)
         (min-index     most-positive-fixnum)
         (max-index     0)
         ((&flet make-name (&rest parts)
            (apply #'symbolicate '#:+ prefix (append parts '(#:+)))))
         ((&flet+ make-constant ((name &optional (index current-index)))
            (setf min-index     (min min-index index)
                  max-index     (max max-index index)
                  current-index (1+ index))
            (let ((name  (apply #'make-name (when name (list '#:/ name))))
                  (value (ash 1 index)))
              `(defconstant ,name ,value)))))
    `(progn
       ,@(mapcar (compose #'make-constant #'ensure-list) values)
       (defconstant ,(make-name '#:-mask)
         ,(logxor (1- (ash 1 (1+ max-index)))
                  (1- (ash 1 min-index)))))))

;;; Regular messages
;;;
;;; Bits 0..6, bit 7 for self-discard

(define-bitfield (regular-message)
  unreliable reliable fifo causal agreed safe)

(defconstant +self-discard+ #x00000040
  "Allow the sender to hint whether it wants the message back or
   not.")

;;; Membership messages
;;;
;;; Bits 8..12

(define-bitfield (membership-message)
  (nil 12)
  (caused-by-join 8) caused-by-leave caused-by-disconnect caused-by-network)

;;; Group and connection commands
;;;
;;; Bits 16..20, bit 21 for reject-message

(define-bitfield (command-message)
  (join 16) leave kill groups)

(defconstant +reject-message+ #x00400000
  "Message was rejected by authentication module.")
