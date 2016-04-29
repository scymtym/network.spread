;;;; ffi.lisp --- Spread foreign library and functions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Foreign library

(defun use-spread-library
    (&key
     (pathname network.spread-system:*spread-library-pathname*))
 (cffi::register-foreign-library
  'libspread
  (if pathname
      `((t (:default ,pathname)))
      `((:darwin (:or "libspread-without-signal-blocking.dylib"
                      "libspread.dylib"
                      "libspread.2.dylib"
                      "libspread.2.0.dylib"
                      "libspread.1.dylib"))
        (:unix   (:or "libspread-without-signal-blocking.so"
                      "libspread.so"
                      "libspread.so.2"
                      "libspread.so.2.0"
                      "libspread.so.1"))
        (t       (:default "libspread")))))
  (cffi:load-foreign-library 'libspread))

(use-spread-library)

;;; Spread constants

(cffi:defcenum return-value
  (:ok                     0)

  (:accept-session         1)

  (:illegal-spread        -1)
  (:could-not-connect     -2)
  (:reject-quota          -3)
  (:reject-no-name        -4)
  (:reject-illegal-name   -5)
  (:reject-not-unique     -6)
  (:reject-version        -7)
  (:connection-closed     -8)
  (:reject-auth           -9)

  (:illegal-session      -11)
  (:illegal-service      -12)
  (:illegal-message      -13)
  (:illegal-group        -14)
  (:buffer-too-short     -15)
  (:groups-too-short     -16)
  (:message-too-long     -17)
  (:net-error-on-session -18))

(cffi:defbitfield (service-type :int)
  (:unreliable-mess      #x00000001)
  (:reliable-mess        #x00000002)
  (:fifo-mess            #x00000004)
  (:causal-mess          #x00000008)
  (:agreed-mess          #x00000010)
  (:safe-mess            #x00000020)
  (:regular-mess         #x0000003f)

  (:self-discard         #x00000040)

  (:caused-by-join       #x00000100)
  (:caused-by-leave      #x00000200)
  (:caused-by-disconnect #x00000400)
  (:caused-by-network    #x00000800)
  (:reg-memb-mess        #x00001000)
  (:transition-mess      #x00002000)
  ;;(:membership-mess      #x00003f00)

  ;;(:reserved             #x003fc000)

  (:reject-mess          #x00400000)
  (:drop-recv            #x01000000)

  (:endian-reserved      #x80000080))

;;; Spread functions

(cffi:defcfun (spread-connect "SP_connect") return-value
  (daemon        :string)
  (name          :string)
  (priority      :int)
  (membership    :int)
  (handle        (:pointer :int))
  (private-group :string))

(cffi:defcfun (spread-disconnect "SP_disconnect") return-value
  (handle :int))

(cffi:defcfun (spread-join "SP_join") return-value
  (handle :int)
  (group  :string))

(cffi:defcfun (spread-leave "SP_leave") return-value
  (handle :int)
  (group  :string))

;; Performance-critical functions

(declaim (inline spread-poll
                 spread-receive
                 spread-multicast spread-multigroup-multicast))

(cffi:defcfun (spread-poll "SP_poll") :int
  (handle :int))

(cffi:defcfun (spread-receive "SP_receive") :int
  (handle             :int)
  (service-type       (:pointer service-type))
  (sender             (:pointer :uchar)) ; actually :string
  (max-groups         :int)
  (num-groups         (:pointer :int))
  (groups             (:pointer :string))
  (message-type       (:pointer :int16))
  (endian-mismatch    (:pointer :int))
  (max-message-length :int)
  (message            (:pointer :uchar)))

(cffi:defcfun (spread-multicast "SP_multicast") :int
  (handle         :int)
  (service-type   service-type)
  (group          (:pointer :uchar))
  (message-type   :int16)
  (message-length :int)
  (message        (:pointer :uchar)))

(cffi:defcfun (spread-multigroup-multicast "SP_multigroup_multicast") :int
  (handle         :int)
  (service-type   service-type)
  (num-groups     :int)
  (groups         (:pointer :uchar))
  (message-type   :int16)
  (message-length :int)
  (message        (:pointer :uchar)))
