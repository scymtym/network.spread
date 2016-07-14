;;;; low-level.lisp --- Low-level (but not ffi) functions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread)

;;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro with-pointers-to-vector-data (bindings &body body)
    `(cffi:with-pointer-to-vector-data ,(first bindings)
       ,@(if (rest bindings)
             `((with-pointers-to-vector-data ,(rest bindings)
                 ,@body))
             body)))

  (defmacro service-type (&rest values &environment env)
    (assert (every (rcurry #'constantp env) values))
    `(cffi:foreign-bitfield-value 'service-type '(,@values)))

  (defmacro set-service-type (service-type &rest values)
    (let ((mask (cffi:foreign-bitfield-value 'service-type values)))
      `(setf (cffi:mem-ref ,service-type :int) ,mask)))

  (defmacro %service-type-matches? (service-type &rest values)
    (let ((mask (cffi:foreign-bitfield-value 'service-type values)))
      `(plusp (logand ,service-type ,mask))))

  (defmacro service-type-matches? (service-type &rest values)
    `(%service-type-matches? (cffi:mem-ref ,service-type :int) ,@values))

  (defmacro return-value-matches? (return-value &rest values)
    (once-only (return-value)
      `(or ,@(mapcar (lambda (value)
                       `(= ,return-value ,(cffi:foreign-enum-value
                                           'return-value value)))
                     values))))

) ; eval-when

;;; Convenience wrappers

;;; The following functions are not performance-critical.
;;;
;;; Consequently, they are not inlined and let cffi translate return
;;; values, enum values, etc.

(defun %connect (daemon
                 &key
                 ;; name
                 priority?
                 membership?)
  (declare (type string daemon))
  (cffi:with-foreign-objects ((handle        :int)
                              (private-group :char +max-group-name+))
    (let ((result (spread-connect daemon (cffi:null-pointer)  ;;(if name (cffi:
                                  (if priority? 1 0) (if membership? 1 0)
                                  handle private-group)))
      (case result
        (:accept-session
         (values (cffi:mem-ref handle :int)
                 (cffi:convert-from-foreign private-group :string)))
        (t
         (error 'connect-failed
                :name daemon
                :code result))))))

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

(declaim (ftype (function (fixnum) (values (or null fixnum) &optional)) %poll)
         (inline %poll))

(defun %poll (handle)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((result (spread-poll handle)))
    (declare (type fixnum result))
    (cond
      ((plusp result) result)
      ((zerop result) nil)
      (t              (%signal-error "Error polling" result)))))

(declaim (ftype (function (t) (values membership-event &optional)) %extract-type)
         (inline %extract-type))

(defun %extract-type (service-type)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((type (cffi:mem-ref service-type :int)))
    (cond
      ((%service-type-matches? type :caused-by-join)
       :join)
      ((%service-type-matches? type :caused-by-leave :caused-by-disconnect)
       :leave)
      (t
       :other))))

(declaim (ftype (function (octet-vector) (values string &optional)) %extract-sender)
         (inline %extract-sender))

(defun %extract-sender (sender)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  #+sbcl (sb-ext:octets-to-string sender
                                  :external-format :ascii
                                  :end             (or (position 0 sender)
                                                       +max-group-name+))
  #-sbcl (error "not implemented"))

(declaim (ftype (function (t octet-vector)
                          (values (or list (eql :group-buffer-too-small))))
                %extract-groups))

(defun %extract-groups (num-groups groups)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((num-groups (cffi:mem-ref num-groups :int)))
    (declare (type (and fixnum (integer * #.+max-groups+)) num-groups))
    (when (minusp num-groups)
      (return-from %extract-groups :group-buffer-too-small))

    (loop :repeat num-groups
       :for offset :of-type fixnum :from 0 :by +max-group-name+
       :collect (let* ((end (+ offset +max-group-name+))
                       (end (or (position 0 groups :start offset :end end)
                                end)))
                  #+sbcl (sb-ext:octets-to-string
                          groups
                          :external-format :ascii
                          :start           offset
                          :end             end))
       #-sbcl (error "not implemented"))))

(declaim (ftype (function (fixnum
                           simple-octet-vector non-negative-fixnum non-negative-fixnum
                           return-aspect-switch return-aspect-switch)
                          (values &rest t))
                %receive-into))

(defun %receive-into (handle buffer start end return-sender? return-groups?)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type octet-vector buffer)
           (type fixnum       start end))
  (let ((sender (cffi:make-shareable-byte-vector +max-group-name+))
        (groups (cffi:make-shareable-byte-vector (* +max-groups+ +max-group-name+))))
    (declare (dynamic-extent sender groups))
    (cffi:with-foreign-objects ((service-type    'service-type)
                                (num-groups      :int)
                                (message-type    :int16)
                                (endian-mismatch :int))
      (with-pointers-to-vector-data
          ((buffer-ptr buffer) (sender-ptr sender) (groups-ptr groups))
        (labels ((receive (max-groups buffer-ptr buffer-size)
                   (spread-receive handle service-type
                                   sender-ptr
                                   max-groups num-groups groups-ptr
                                   message-type endian-mismatch
                                   buffer-size buffer-ptr))
                 (receive/with-data (max-groups)
                   (let* ((buffer-ptr  (cffi:inc-pointer buffer-ptr start))
                          (buffer-size (- end start))
                          (result      (receive max-groups buffer-ptr buffer-size)))
                     (when (minusp result)
                       (%signal-error "Error receiving" result))
                     result))
                 (receive/probe ()
                   (set-service-type service-type) ; detect too short buffers
                   (let ((result (receive 0 buffer-ptr 0)))
                     (cond
                       ((return-value-matches?
                         result :buffer-too-short :groups-too-short)
                        t)
                       ((minusp result)
                        (%signal-error "Error receiving" result)))))
                 (return/full (type size)
                   (values (or type (%extract-type service-type)) size
                           (when return-sender?
                             (%extract-sender sender))
                           (when return-groups?
                             (%extract-groups num-groups groups)))))
          (declare (dynamic-extent #'receive #'receive/with-data #'receive/probe
                                   #'return/full))

          (cond
            ;; Returning sender and/or group list has definitely not
            ;; been requested.
            ((not (or return-sender? return-groups?))
             (set-service-type service-type :drop-recv)
             (let ((result (receive/with-data 0)))
               (declare (type fixnum result))
               (cond
                 ;; Service type indicates regular message => return
                 ;; regular message.
                 ((service-type-matches? service-type :regular-mess)
                  (values :regular result))
                 ;; Service type does not indicate regular message =>
                 ;; return membership message.
                 (t
                  (values (%extract-type service-type) nil)))))

            ;; At least one of sender and group list should be
            ;; returned iff the message is a membership message.
            ((not (or (eq return-sender? t) (eq return-groups? t)))
             (cond
               ;; When probing receive does not detect too short data
               ;; or group buffer, this has to be a membership message
               ;; with empty group list.
               ;;
               ;; Note that this relies on Spread returning
               ;; :buffer-too-short when buffer size and payload size
               ;; are both 0.
               ((not (receive/probe))
                (return/full nil nil))
               ;; Service type indicates regular message => receive
               ;; and return regular message.
               ((service-type-matches? service-type :regular-mess)
                (set-service-type service-type :drop-recv)
                (let ((result (receive/with-data 0)))
                  (values :regular result)))
               ;; Service type does not indicate regular message =>
               ;; receive and return membership message.
               (t
                (receive/with-data +max-groups+)
                (return/full nil nil))))

            ;; Both, sender and group list, should definitely be
            ;; returned.
            (t
             (set-service-type service-type)
             (let ((result (receive/with-data +max-groups+)))
               (declare (type fixnum result))
               (cond
                 ;; Service type indicates regular message => return
                 ;; regular message.
                 ((service-type-matches? service-type :regular-mess)
                  (return/full :regular result))
                 ;; Service type does not indicate regular message =>
                 ;; return membership message.
                 (t
                  (return/full nil nil)))))))))))

(declaim (ftype (function (fixnum simple-octet-vector simple-octet-vector) (values))
                %send-one))

(defun %send-one (handle destination data)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((length (length destination)))
    (when (< length +max-group-name+)
      (setf (aref destination length) 0)))
  (with-pointers-to-vector-data ((group-ptr destination) (data-ptr data))
    (let ((result (spread-multicast
                   handle
                   (service-type :reliable-mess :self-discard)
                   group-ptr
                   0 ; message-type
                   (length data) data-ptr)))
      (declare (type fixnum result))
      (cond
        ((not (minusp result))
         (values))

        (t
         (%signal-error "Sending failed" result))))))

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
       (when (< length +max-group-name+)
         (setf (aref groups (+ offset length)) 0)))
    (with-pointers-to-vector-data ((groups-ptr groups) (data-ptr data))
      (let ((result (spread-multigroup-multicast
                     handle
                     (service-type :reliable-mess :self-discard)
                     num-groups groups-ptr
                     0 ; message-type
                     (length data) data-ptr)))
        (declare (type fixnum result))
        (cond
          ((not (minusp result))
           (values))

          (t
           (%signal-error "Multigroup send failed" result)))))))

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
