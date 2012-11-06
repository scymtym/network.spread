;;; ffi.lisp --- Spread foreign library and functions.
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


;;;
;;

(cffi:define-foreign-library libspread
  (:darwin (:or "libspread-without-signal-blocking.dylib"
		"libspread.dylib"
		"libspread.2.dylib"
		"libspread.2.0.dylib"
		"libspread.1.dylib"))
  (:unix   (:or "libspread-without-signal-blocking.so"
		"libspread.so"
		"libspread.so.2"
		"libspread.so.2.0"
		"libspread.so.1"))
  (t       (:default "libspread")))

(cffi:use-foreign-library libspread)


;;; Spread constants
;;

(cffi:defcenum return-value
  (:ok                     0)

  (:accept-session         1)

  (:illegal-spread        -1)
  (:could-not-connect	  -2)
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
  (:buffer-too-short	 -15)
  (:groups-too-short     -16)
  (:message-too-long     -17)
  (:net-error-on-session -18))

(cffi:defbitfield (message-type :int16)
  (:unreliable-mess      #x00000001)
  (:reliable-mess        #x00000002)
  (:fifo-mess            #x00000004)
  (:causal-mess          #x00000008)
  (:agreed-mess          #x00000010)
  (:safe-mess            #x00000020)
  ;(:regular-mess         #x0000003f)

  (:self-discard         #x00000040)

  (:caused-by-join       #x00000100)
  (:caused-by-leave      #x00000200)
  (:caused-by-disconnect #x00000400)
  (:caused-by-network    #x00000800)
  (:reg-memb-mess        #x00001000)
  (:transition-mess      #x00002000)
  ;(:membership-mess      #x00003f00)

  ;(:reserved             #x003fc000)

  (:reject-mess          #x00400000)
  (:drop-recv            #x01000000)

  (:endian-reserved      #x80000080))

(defconstant +max-message+    140000
  "The maximum size of a single message.")

(defconstant +max-group-name+ 32
  "The maximum size of a group name.")

(defconstant +max-groups+     32
  "The maximum number of groups in a single API call.")


;;; Spread functions
;;

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

(cffi:defcfun (spread-poll "SP_poll") :int
  (handle :int))

(cffi:defcfun (spread-receive "SP_receive") :int
  (handle             :int)
  (service-type       (:pointer :int))
  (sender             :string)
  (max-groups         :int)
  (num-groups         (:pointer :int))
  (groups             (:pointer :string))
  (message-type       (:pointer :int16))
  (endian-mismatch    (:pointer :int))
  (max-message-length :int)
  (message            (:pointer :uchar)))

(cffi:defcfun (spread-multicast "SP_multicast") :int
  (handle         :int)
  (service-type   :int) ;; type?
  (group          :string)
  (message-type   :int16)
  (message-length :int)
  (message        (:pointer :uchar)))

(cffi:defcfun (spread-multigroup-multicast "SP_multigroup_multicast") :int
  (handle         :int)
  (service-type   :int)
  (num-groups     :int)
  (groups         (:pointer :string))
  (message-type   :int16)
  (message-length :int)
  (message        (:pointer :uchar)))


;;; Convenience wrappers
;;

(defun %connect (daemon
		 &key
		 ;; name
		 ;; priority?
		 membership?)
  (cffi:with-foreign-objects ((handle '(:pointer :int)))
    (cffi:with-foreign-string (private-group (make-string +max-group-name+))
      (let ((result (spread-connect
		     daemon
		     (cffi:null-pointer) ;;(if name (cffi:
		     0 ;; (if priority 1 0)
		     (if membership? 1 0)
		     handle
		     private-group)))
	(case result
	  (:accept-session
	   (values
	    (cffi:mem-ref handle :int)
	    (cffi:convert-from-foreign private-group :string)))
	  (t
	   (error 'connect-failed
		  :name daemon
		  :code result)))))))

(defun %disconnect (handle)
  (let ((result (spread-disconnect handle)))
    (case result
      (:ok (values))
      (t   (%signal-error "Disconnect error" result)))))

(defun %join (handle group)
  (let ((result (spread-join handle group)))
    (case result
      (:ok (values))
      (t   (%signal-error "Error joining group ~S" result group)))))

(defun %leave (handle group)
  (let ((result (spread-leave handle group)))
    (case result
      (:ok (values))
      (t   (%signal-error "Error leaving group ~S" result group)))))

(declaim (ftype (function (fixnum) (or null fixnum)) %poll)
	 (inline %poll))

(defun %poll (handle)
  (let ((result (spread-poll handle)))
    (cond
      ((plusp result)
       result)
      ((zerop result)
       nil)
      (t
       (%signal-error "Error polling" result)))))

(defun %receive (handle)
  (declare (optimize (speed 3) (safety 0) (debug 0))) ;; SBCL won't do stack allocation otherwise
  (let ((buffer  (make-array +max-message+
			    :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (cffi:with-pointer-to-vector-data (buffer1 buffer)
      (cffi:with-foreign-objects ((service-type    '(:pointer message-type))
				  (num-groups      '(:pointer :int))
				  (message-type1   '(:pointer :int16))
				  (endian-mismatch '(:pointer :int)))
	(cffi:with-foreign-strings ((groups (make-string (* +max-groups+ +max-group-name+)))
				    (sender (make-string +max-group-name+)))
	  (setf (cffi:mem-ref service-type :int) 0)
	  (let ((result (spread-receive handle
					service-type ;; for input: 0 or DROP-RECV
					sender
					+max-groups+ num-groups groups
					message-type1
					endian-mismatch
					+max-message+ buffer1)))
	    (cond
	      ;; Positive result -> process message
	      ((not (minusp result))
	       (funcall (cond
			  ((plusp (logand (cffi:mem-ref service-type :int16) #x3f)) ;; TODO ugly
			   #'%process-regular-message)
			  (t
			   #'%process-membership-message))
			service-type sender num-groups groups result buffer))

	      ;; Negative result -> error
	      (t
	       (%signal-error "Error receiving" result)))))))))

(declaim (ftype (function (t t t t fixnum octet-vector) list)
		%process-regular-message
		%process-membership-message)
	 (inline %process-regular-message
		 %process-membership-message))

(defun %process-regular-message (service-type sender num-groups groups result buffer) ;; TODO message-type
  (declare (ignore service-type))

  (let ((num-groups (cffi:mem-ref num-groups :int)))
    (list
     :regular
     (subseq buffer 0 result)
     ;; TODO do not convert these if they are not required
     (cffi:convert-from-foreign sender :string)
     (if (not (minusp num-groups))
	 (%extract-groups num-groups groups)
	 :group-buffer-too-small))))

(defun %process-membership-message (service-type sender num-groups groups result buffer)
  (declare (ignore result buffer))

  (let ((type       (cffi:mem-ref service-type 'message-type))
	(num-groups (cffi:mem-ref num-groups :int)))
    (list
     (%type->tag type)
     (cffi:convert-from-foreign sender :string) ;; means group
     (if (not (minusp num-groups)) ;; group members
	 (%extract-groups num-groups groups)
	 :group-buffer-too-small))))

(declaim (ftype (function (fixnum string octet-vector)
			  (values)) %send-one))

(defun %send-one (handle destination data)
  (cffi:with-pointer-to-vector-data (message data)
    (let ((result (spread-multicast handle
				    2          ;; service-type
				    destination
				    0          ;; message-type
				    (length data) message)))
      (cond
	((not (minusp result))
	 (values))

	(t
	 (%signal-error "Sending failed" result))))))

(declaim (ftype (function (fixnum list octet-vector)
			  (values)) %send-multiple))

(defun %send-multiple (handle destinations data)
  (cffi:with-foreign-string
      (groups  (apply #'concatenate 'string
		      (iter (for destination in destinations)
			    (collect destination)
			    (collect (make-list
				      (- +max-group-name+ (length destination))
				      :initial-element #\Nul)))))
    (cffi:with-pointer-to-vector-data (message data)
      (let ((result (spread-multigroup-multicast handle
						 2          ;; service-type
						 (length destinations) groups
						 0          ;; message-type
						 (length data) message)))
	(cond
	  ((not (minusp result))
	   (values))

	  (t
	   (%signal-error "Multigroup send failed" result)))))))


;;; Utility functions
;;

(declaim (ftype (function (list) membership-event) %type->tag)
	 (inline %type->tag))

(defun %type->tag (type)
  (cond
    ((member :caused-by-join type)
     :join)
    ((or (member :caused-by-leave type)
	 (member :caused-by-disconnect type))
     :leave)
    (t
     :other)))

(declaim (ftype (function (non-negative-fixnum t) list) %extract-groups)
	 (inline %extract-groups))

(defun %extract-groups (num-groups groups)
  (iter (repeat num-groups)
	(for offset :from 0 :by +max-group-name+)
	(collect (cffi:foreign-string-to-lisp groups
					      :encoding  :ascii
					      :offset    offset
					      :max-chars +max-group-name+))))

(declaim (ftype (function (string (or fixnum keyword) &rest t) *) %signal-error)
	 (inline %signal-error))

(defun %signal-error (format value &rest args)
  (error 'simple-spread-error
	 :format-control   format
	 :format-arguments args
	 :code
	 (etypecase value
	   (fixnum
	    (cffi::%foreign-enum-keyword
	     (funcall (cffi::find-type-parser 'return-value))
	     value :errorp t))
	   (keyword
	    value))))
