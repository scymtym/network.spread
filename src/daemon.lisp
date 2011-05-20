;;; daemon.lisp --- Function for running the spread daemon.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :spread)

(defun start-daemon (&key
		     (port              4803)
		     (program           "spread")
		     (host              "localhost")
		     (host-address      "127.0.0.1")
		     (broadcast-address "127.0.0.255"))
  "Start a spread daemon with the specified parameters and return the process."
  ;; Generate a randomized configuration file name.
  (let ((config-filename (format nil "/tmp/spread-~8,'0X.conf"
				 (random (ash 1 (* 8 4))))))
    ;; Write the specified configuration to that file.
    (with-output-to-file (stream config-filename)
      (format stream
	      "Spread_Segment ~A:~A {~%    ~A ~A~%}"
	      broadcast-address port host host-address))
    ;; Start a spread daemon that uses the configuration file.
    (let ((process (sb-ext:run-program
		    program `("-n" ,host "-c" ,config-filename)
		    :search t
		    :wait   nil)))
      ;; Store the port number and configuration file name in the
      ;; process object.
      (setf (getf (sb-ext:process-plist process) :port)
	    port
	    (getf (sb-ext:process-plist process) :config-file)
	    config-filename)
      ;; Give the spread daemon five seconds to start up then check
      ;; whether it died in the meantime.
      (sleep 5)
      (unless (sb-ext:process-alive-p process)
	(error "Spread daemon failed to start."))
      ;; If everything looks good, return the process object.
      process)))

(defun stop-daemon (process)
  "Stop the spread daemon PROCESS and clean the mess it leaves
behind."
  (let ((pid             (sb-ext:process-pid process))
	(port            (getf (sb-ext:process-plist process) :port))
	(config-filename (getf (sb-ext:process-plist process) :config-file)))
    ;; Send a SIGINT signal to the process and wait for it to die.
    (sb-posix:kill pid sb-posix:SIGINT)
    (sb-ext:process-wait process)
    ;; Clean up the socket and the configuration file.
    (ignore-errors
      (delete-file config-filename))
    (ignore-errors
      (delete-file (format nil "/tmp/~A" port)))
    (values)))

(defmacro with-daemon ((&key
			(port              4803)
			(program           "spread")
			(host              "localhost")
			(host-address      "127.0.0.1")
			(broadcast-address "127.0.0.255"))
		       &body body)
  "Execute BODY with a spread using the specified parameters running."
  (with-unique-names (process-var)
    `(let ((,process-var (start-daemon
			  :port              ,port
			  :program           ,program
			  :host              ,host
			  :host-address      ,host-address
			  :broadcast-address ,broadcast-address)))
       (declare (ignorable ,process-var))
       (unwind-protect
	    (progn ,@body)
	 (stop-daemon ,process-var)))))
