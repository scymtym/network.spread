;;;; util.lisp --- Unit test for utilities in the base module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:network.spread.base.test)

(in-suite :network.spread.base)

(test parse-daemon-name.smoke
  "Smoke test for the `parse-daemon-name' function."

  (mapc (lambda+ ((input expected))
          (flet ((do-it ()
                   (parse-daemon-name input)))
            (case expected
              (daemon-name-syntax-error
               (signals daemon-name-syntax-error (do-it)))
              (t
               (is (equal expected (multiple-value-list (do-it))))))))
        '(;; A few invalid cases.
          ("not-a-port"     daemon-name-syntax-error)
          ("foo@bar"        daemon-name-syntax-error)

          ;; These are valid.
          ("4803@localhost" ("localhost" 4803))
          ("4803"           (nil         4803)))))
