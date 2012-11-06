;;; variables.lisp --- Unit test for handling of special variables.
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

(cl:in-package #:network.spread.test)

(deftestsuite variables-root (root)
  ()
  (:documentation
   "Unit tests for the handling of special variables."))

(addtest (variables-root
          :documentation
	  "Test binding `*incoming-stream*'.")
  incoming-stream-smoke

  (let ((*incoming-stream* (make-string-output-stream)))
    (with-connection (receiver daemon)
      (with-group (receiver "incoming-stream-group")
	(with-connection (sender daemon)
	  (send sender "incoming-stream-group" "foo"))
	(receive receiver :block? t)))

    (let ((captured (get-output-stream-string *incoming-stream*))
	  (expected "66 6F 6F"))
      (ensure-same
       captured expected
       :test      #'string=
       :report    "~@<The string captured by binding `*incoming-stream*' ~
was ~S, not ~S ~@:>"
       :arguments (captured expected)))))

(addtest (variables-root
          :documentation
	  "Test binding `*outgoing-stream*'.")
  outgoing-stream-smoke

  (let ((*outgoing-stream* (make-string-output-stream)))
    (with-connection (sender daemon)
      (send sender "outgoing-stream-group" "foo"))

    (let ((captured (get-output-stream-string *outgoing-stream*))
	  (expected "66 6F 6F"))
      (ensure-same
       captured expected
       :test      #'string=
       :report    "~@<The string captured by binding `*outgoing-stream*' ~
was ~S, not ~S ~@:>"
       :arguments (captured expected)))))
