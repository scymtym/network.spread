;;;; client.lisp --- Client role of the Spread wire protocol.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.wire-protocol)

;;; Connection management

(defun client-connect (stream name membership? priority authentication-methods)
  (declare (notinline client-send))
  (with-communication-error-translation (stream)
    ;; Send initial greeting message with client version and,
    ;; optionally, requested private name.
    (let* ((name-length (if name (length name) 0))
           (buffer      (make-octet-vector (+ 5 name-length))))
      (declare (type group-name-length name-length))
      (setf (aref buffer 0) *major-version*
            (aref buffer 1) *minor-version*
            (aref buffer 2) *patch-version*
            (aref buffer 3) (dpb priority (byte 4 4)
                                 (if membership? 1 0))
            (aref buffer 4) name-length)
      (when name
        (setf (subseq buffer 5 (+ 5 name-length)) name))
      (safe-write-sequence "sending the initial message" buffer stream))

    ;; Authenticate using the supplied AUTHENTICATION-METHODS.
    (client-authenticate stream authentication-methods)

    ;; Receive daemon version and private group name.
    (let ((version (make-list 3)))
      (safe-read-sequence "receiving the daemon version" version stream)
      (log:debug "~@<Daemon reported version ~{~D~^.~}~@:>" version)
      (unless (typep version *acceptable-daemon-versions*)
        (error 'incompatible-daemon-error
               :context            "connecting"
               :stream             stream
               :actual-version     version
               :supported-versions *acceptable-daemon-versions-description*))

      (values (read-length-delimited-sequence
               "receiving the private group name"
               stream 0 +group-name-length-limit+)
              version))))

(defun client-disconnect (stream private-group)
  (declare (notinline client-send))
  (log:debug "~@<Sending kill message~@:>")
  (with-communication-error-translation (stream)
    (client-send stream private-group +command-message/kill+ private-group
                 0 +empty-octet-vector+)
    (force-output stream))
  (values))

;;; Client authentication

(defun client-authenticate (stream methods)
  (log:debug "~@<Trying to authenticate with method~P~
              ~@:_~
              ~{+ ~A~^~@:_~}~
              ~@:>"
             (length methods) methods)

  (with-communication-error-translation (stream)
    (let+ (((&values auth-list auth-list-length)
            (read-length-delimited-sequence
             "receiving the authentication method list"
             stream 0 +authentication-data-length-limit+))
           (accepted-methods (unpack-auth-methods auth-list auth-list-length))
           (methods          (network.spread.authentication:validate
                              methods accepted-methods)))

      ;; Send list of authentication methods to server.
      (safe-write-sequence "sending authentication method list"
                           (pack-auth-methods methods) stream)

      ;; Perform authentication using METHODS.
      (network.spread.authentication:authenticate methods stream)

      ;; Receive authentication result.
      (let ((result (let ((byte (read-byte stream)))
                      (declare (type octet byte))
                      (twos-complement byte))))
        (if (= result +result-session-accepted+)
            (log:debug "~@<Authenticated successfully~@:>")
            (with-condition-translation
                (((error network.spread.authentication:authentication-failed-error)
                  :method methods))
              (error "Server replied with failure code ~D" result))))))
  (values))

;;; Sending and receiving messages

(declaim (inline client-send))
(defun client-send (stream private-group service-type groups message-type payload)
  (with-communication-error-translation (stream :inline t)
    (let+ ((groups-flat?   (typep groups 'simple-octet-vector))
           ((&values group-count group-padding)
            (if groups-flat?
                (ceiling (length groups) +group-name-length-limit+)
                (length groups)))
           (payload-length (length payload))
           (hint           (ash message-type 8))
           (header         (make-octet-vector (header-slot-offset 5))))
      (declare (dynamic-extent header))  ; TODO broken by using let+
      ;; Populate and send header. Note that, when sending, we always
      ;; use little endian encoding and the corresponding markers.
      (setf (ub32ref/le header (header-slot-offset 0)) (endian-mark-little service-type)
            (subseq header
                    (header-slot-offset 1)
                    (+ (header-slot-offset 1) (length private-group)))
            private-group
            (ub32ref/le header (header-slot-offset 2)) group-count
            (ub32ref/le header (header-slot-offset 3)) (endian-mark-little hint)
            (ub32ref/le header (header-slot-offset 4)) payload-length)
      (log:debug "~@<Writing header~@:_~
                  ~,,,16:@/utilities.binary-dump:print-binary-dump/~@:>"
                 header)
      (write-sequence header stream)

      ;; Write groups: if GROUPS is a `simple-octet-vector', it contains
      ;; one or more concatenated group names with the necessary
      ;; padding. Otherwise it is a vector of `simple-octet-vector's
      ;; each containing one group name.
      (if groups-flat?
          (progn
            (log:debug "Writing groups~&~
                        ~,,,16/utilities.binary-dump:print-binary-dump/"
                       groups)
            (write-sequence groups stream)
            (when (minusp group-padding)
              (write-sequence **nul-buffer** stream :end (- group-padding))))
          (locally (declare (type (array simple-octet-vector 1) groups))
            (loop :for group :across groups :do
               (write-padded-sequence group stream +group-name-length-limit+))))

      ;; Write payload.
      (unless (zerop (length payload))
        (log:debug "Writing payload~&~
                    ~,,,16/utilities.binary-dump:print-binary-dump/"
                   payload)
        (write-sequence payload stream))
      (values))))
(declaim (notinline client-send))

(declaim (inline client-receive-into))
(defun client-receive-into (stream buffer start end return-sender? return-groups?)
  (with-communication-error-translation (stream :inline t)
    (let* ((start            (or start 0))
           (end              (or end (length buffer)))
           (available-length (- end start))
           (header           (make-octet-vector (header-slot-offset 5))))
      (declare (type (message-data-length 0) available-length)
               (dynamic-extent header))
      (safe-read-sequence "receiving the message header" header stream)
      (log:debug "~@<Received header~:@_~
                  ~,,,16:@/utilities.binary-dump:print-binary-dump/~@:>"
                 header)
      (let+ (((&values service-type group-count hint payload-length)
              (if (endian-little? (aref header 0))
                  (values (endian-unmark-little
                           (ub32ref/le header (header-slot-offset 0)))
                          (ub32ref/le header (header-slot-offset 2))
                          (endian-unmark-little
                           (ub32ref/le header (header-slot-offset 3)))
                          (ub32ref/le header (header-slot-offset 4)))
                  (values (ub32ref/be header (header-slot-offset 0))
                          (ub32ref/be header (header-slot-offset 2))
                          (ub32ref/be header (header-slot-offset 3))
                          (ub32ref/be header (header-slot-offset 4)))))
             (private-group  (when (or (eq return-sender? t)
                                       (and (eq return-sender? :when-membership)
                                            (service-type-membership-message?
                                             service-type)))
                               (subseq header
                                       (header-slot-offset 1)
                                       (header-slot-offset 2))))
             (group-length   (* group-count +group-name-length-limit+))
             (message-type   (ash hint -8))
             (return-groups? (or (eq return-groups? t)
                                 (and (eq return-groups? :when-membership)
                                      (service-type-membership-message?
                                       service-type))))
             (groups         (if return-groups?
                                 (make-octet-vector group-length)
                                 **discard-buffer**)))
        (locally
            (declare (type (message-data-length 0) payload-length)
                     (type group-count             group-count))
          (log:debug "service-type ~X membership? ~A payload-length ~:D"
                     service-type membership? payload-length)

          ;; Depending on RETURN-GROUPS?, read or discard groups.
          (safe-read-sequence "receiving the groups list" groups stream
                              :end group-length)
          (log:debug "~@<Received groups~:@_~
                      ~,,,16:@/utilities.binary-dump:print-binary-dump/~@:>"
                     groups)

          ;; Check payload length vs. available space in BUFFER and
          ;; discard any leftover data.
          (let+ (((&values read-length remainder)
                  (if (<= payload-length available-length)
                      (values payload-length   0)
                      (values available-length (- payload-length available-length)))))
            (safe-read-sequence "receiving the payload" buffer stream
                                :start start :end (+ start read-length))
            (when remainder (discard-bytes stream remainder)))
          (log:debug "~@<Received payload~:@_~
                      ~,,V,16:@/utilities.binary-dump:print-binary-dump/~@:>"
                     payload-length buffer)
          (values service-type private-group (when return-groups? groups)
                  message-type payload-length))))))
(declaim (notinline client-receive-into))

;;; Group membership operations

(macrolet
    ((define-membership-operation (name service-type description)
       `(defun ,name (stream private-group group)
          (log:debug ,(format nil "~~@<Writing ~A message for group~~@:_~
                                   ~~,,,16:@/utilities.binary-dump:print-binary-dump/~~@:>"
                              description)
                     group)
          (client-send stream private-group ,service-type group
                       0 +empty-octet-vector+))))

  (define-membership-operation client-join  +command-message/join+  "join")
  (define-membership-operation client-leave +command-message/leave+ "leave"))
