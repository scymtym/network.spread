;;;; low-level.lisp --- Low-level (but not ffi) functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Convenience wrappers

(defmacro with-pointers-to-vector-data (bindings &body body)
  `(cffi:with-pointer-to-vector-data ,(first bindings)
     ,@(if (rest bindings)
           `((with-pointers-to-vector-data ,(rest bindings)
               ,@body))
           body)))

;;; The following functions are not performance-critical.
;;;
;;; Consequently, they are not inlined and let cffi translate return
;;; values, enum values, etc.

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

;;; The following function are performance-critical.
;;;
;;; Consequently, some of them are inlined and cffi's automatic
;;; translation of return values, enum values, etc. is mostly avoided.

(declaim (ftype (function (fixnum) (or null fixnum)) %poll)
         (inline %poll))

(defun %poll (handle)
  (let ((result (spread-poll handle)))
    (cond
      ((plusp result) result)
      ((zerop result) nil)
      (t              (%signal-error "Error polling" result)))))

(declaim (ftype (function (t) (values membership-event &rest nil)) %extract-type)
         (inline %extract-type))

(defun %extract-type (service-type)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((type (cffi:mem-ref service-type 'service-type)))
    (cond
      ((member :caused-by-join type)            :join)
      ((or (member :caused-by-leave type)
           (member :caused-by-disconnect type)) :leave)
      (t                                        :other))))

(declaim (ftype (function (simple-octet-vector) (values string &rest nil)) %extract-sender)
         (inline %extract-sender))

(defun %extract-sender (sender)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  #+sbcl
  (sb-ext:octets-to-string
   sender :external-format :ascii
   :end (or (position 0 sender) (length sender)))
  #-sbcl
  (cffi:foreign-string-to-lisp
   sender
   :encoding  :ascii
   :max-chars (or (position 0 sender) (length sender))))

(declaim (ftype (function (t octet-vector)
                          (values (or list (eql :group-buffer-too-small))))
                %extract-groups)
         (inline %extract-groups))

(defun %extract-groups (num-groups groups)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((num-groups (cffi:mem-ref num-groups :int)))
    (declare (type (and fixnum (integer * #.+max-groups+)) num-groups))
    (when (minusp num-groups)
      (return-from %extract-groups :group-buffer-too-small))

    (loop :repeat num-groups
       :for offset :of-type fixnum :from 0 :by +max-group-name+
       :collect
       #+sbcl
       (sb-ext:octets-to-string
        groups :external-format :ascii
        :start offset
        :end   (or (position 0 groups
                             :start offset
                             :end   (+ offset +max-group-name+))
                   (+ offset +max-group-name+)))
       #-sbcl
       (cffi:foreign-string-to-lisp groups
                                    :encoding  :ascii
                                    :offset    offset
                                    :max-chars +max-group-name+))))

(declaim (inline %%receive-into))

(defun %%receive-into (handle buffer start end)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type octet-vector buffer)
           (type fixnum start end))
  (let ((sender (cffi:make-shareable-byte-vector +max-group-name+)))
    (declare (dynamic-extent sender))
    (cffi:with-foreign-objects ((service-type    'service-type)
                                (num-groups      :int)
                                (message-type    :int16)
                                (endian-mismatch :int))
      (setf (cffi:mem-ref service-type :int)
            #.(cffi:foreign-bitfield-value 'service-type '(:drop-recv)))
      (let ((result
             (with-pointers-to-vector-data ((sender-ptr sender)
                                            (buffer-ptr buffer))
               (spread-receive handle service-type
                               sender-ptr
                               0 num-groups (cffi:null-pointer)
                               message-type endian-mismatch
                               end (cffi:inc-pointer buffer-ptr start)))))
        (declare (type fixnum result))
        (cond
          ;; Negative result => error.
          ((minusp result)
           (%signal-error "Error receiving" result))

          ;; Positive result and service type indicates regular
          ;; message => return regular message.
          ((plusp (logand
                   (cffi:mem-ref service-type :int)
                   #.(cffi:foreign-bitfield-value 'service-type '(:regular-mess))))
           (values :regular result))

          ;; Positive result and service type does not indicate
          ;; regular message => return membership message.
          (t
           (values (%extract-type service-type) nil)))))))

(declaim (inline %%receive-into/sender+groups))

(defun %%receive-into/sender+groups (handle buffer start end sender? groups?)
  ;; SBCL won't do stack allocation otherwise
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type octet-vector buffer)
           (type fixnum start end))
  (let ((sender (cffi:make-shareable-byte-vector +max-group-name+))
        (groups (cffi:make-shareable-byte-vector
                 (* +max-groups+ +max-group-name+))))
    (declare (dynamic-extent sender groups))
    (cffi:with-foreign-objects ((service-type    'service-type)
                                (num-groups      :int)
                                (message-type    :int16)
                                (endian-mismatch :int))
      (setf (cffi:mem-ref service-type 'service-type) 0)
      (let ((result
             (with-pointers-to-vector-data ((sender-ptr sender)
                                            (groups-ptr groups)
                                            (buffer-ptr buffer))
               (spread-receive handle service-type
                               sender-ptr
                               +max-groups+ num-groups groups-ptr
                               message-type endian-mismatch
                               end (cffi:inc-pointer buffer-ptr start)))))
        (declare (type fixnum result))
        (cond
          ;; Negative result => error.
          ((minusp result)
           (%signal-error "Error receiving" result))

          ;; Positive result and service type indicates regular
          ;; message => return regular message.
          ((plusp (logand
                   (cffi:mem-ref service-type :int)
                   #.(cffi:foreign-bitfield-value 'service-type '(:regular-mess))))
           (values :regular result
                   (when sender? (%extract-sender sender))
                   (when groups? (%extract-groups num-groups groups))))

          ;; Positive result and service type does not indicate
          ;; regular message => return membership message.
          (t
           (values (%extract-type service-type) nil
                   (when sender? (%extract-sender sender))
                   (when groups? (%extract-groups num-groups groups)))))))))

(declaim (ftype (function (integer octet-vector non-negative-fixnum non-negative-fixnum boolean boolean)
                          (values &rest t))
                %receive-into))

(defun %receive-into (handle buffer start end return-sender? return-groups?)
  (cond
    ((not (or return-sender? return-groups?))
     (%%receive-into handle buffer start end))
    (t
     (%%receive-into/sender+groups
      handle buffer start end return-sender? return-groups?))))

(declaim (ftype (function (fixnum simple-octet-vector simple-octet-vector) (values))
                %send-one))

(defun %send-one (handle destination data)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((length (length destination)))
    (when (< length +max-group-name+)
      (setf (aref destination length) 0)))
  (cffi:with-pointer-to-vector-data (group-ptr destination)
    (cffi:with-pointer-to-vector-data (message data)
      (let ((result (spread-multicast handle
                                      2 ; service-type
                                      group-ptr
                                      0 ; message-type
                                      (length data) message)))
        (declare (type fixnum result))
        (cond
          ((not (minusp result))
           (values))

          (t
           (%signal-error "Sending failed" result)))))))

(declaim (ftype (function (fixnum sequence simple-octet-vector) (values))
                %send-multiple))

(defun %send-multiple (handle destinations data)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((num-groups (length destinations))
        (groups     (cffi:make-shareable-byte-vector (* +max-groups+ +max-group-name+))))
    (declare (dynamic-extent groups))
    (loop :for destination :of-type octet-vector :being :the :elements :in destinations
       :for offset :from 0 :by +max-group-name+
       :for length = (length destination) :do
       (setf (subseq groups offset (+ offset length)) destination)
       (when (< length)
         (setf (aref groups (+ offset length)) 0)))
    (cffi:with-pointer-to-vector-data (groups-ptr groups)
      (cffi:with-pointer-to-vector-data (message data)
        (let ((result (spread-multigroup-multicast handle
                                                   2          ; service-type
                                                   num-groups groups-ptr
                                                   0          ; message-type
                                                   (length data) message)))
          (declare (type fixnum result))
          (cond
            ((not (minusp result))
             (values))

            (t
             (%signal-error "Multigroup send failed" result))))))))

;;; Utility functions

(declaim (ftype (function (string (or fixnum keyword) &rest t) *) %signal-error))

(defun %signal-error (format value &rest args)
  (error 'simple-spread-client-error
         :format-control   format
         :format-arguments args
         :code
         (etypecase value
           (fixnum  (cffi:foreign-enum-keyword 'return-value value :errorp t))
           (keyword value))))
