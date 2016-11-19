;;;; protocol.lisp --- Unit tests for protocol functions of the authentication module.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.authentication.test)
(in-suite :network.spread.authentication)

(test validate.smoke
  "Smoke test for the `validate' generic function."

  (let+ ((name   "MOCK")
         (method (make-authentication-method :mock :name name))
         ((&flet do-it (accepted list? &rest args)
            (apply #'validate (if list? (list method) method) accepted
                   args))))
    ;; Test rejected method lists.
    (signals method-rejected-error (do-it '() nil))
    (signals method-rejected-error (do-it '() t))

    ;; Test accepted method lists.
    (is (equal (list method) (do-it `(,name) nil)))
    (is (equal (list method) (do-it `(,name) t)))

    ;; Test :if-invalid.
    (is (eq :foo (do-it '() nil :if-invalid :foo)))
    (is (eq :foo (do-it '() t   :if-invalid :foo)))))

(test authenticate.smoke
  "Smoke test for the `authenticate' generic function."

  (let+ ((name    "MOCK")
         (succeed (make-authentication-method
                   :mock :name name :behavior :succeed))
         (fail    (make-authentication-method
                   :mock :name name :behavior :fail))
         (stream  (make-broadcast-stream))
         ((&flet do-it (method)
            (authenticate method stream))))
    ;; Test authentication failures.
    (signals authentication-error (do-it fail))
    (signals authentication-error (do-it (list fail)))
    (signals authentication-error (do-it (list fail fail)))

    ;; Test authentication successes.
    (is (equal :success             (do-it succeed)))
    (is (equal '(:success)          (do-it (list succeed))))
    (is (equal '(:success :success) (do-it (list succeed succeed))))))

(test authenticate.retry
  "Test the `retry' restart established by the `authenticate' generic
   function."

  (let ((method  (make-authentication-method
                  :mock :name "MOCK" :behavior :fail-once))
        (stream  (make-broadcast-stream))
        (failed? nil)
        (report  nil))
    (handler-bind ((authentication-error
                    (lambda (condition)
                      (declare (ignore condition))
                      (setf failed? t
                            report  (princ-to-string (find-restart 'retry)))
                      (invoke-restart 'retry))))
      (is (eq :success (authenticate method stream)))
      (is-true failed?)
      (is-false (emptyp report)))))
