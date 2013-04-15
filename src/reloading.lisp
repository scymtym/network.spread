;;;; reloading.lisp --- Reloading of the Spread library after re-init.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; This whole file is a kludge to provide the equivalents of linking
;;; with and without RPATH respectively. We could almost just use
;;; SBCL's :dont-save parameter to `sb-ext:load-shared-object' (if
;;; cffi supported it) but that would not allow us to continue with a
;;; warning in case the shared object could not be reloaded.

(cl:in-package #:network.spread)

(defun unload-spread-library ()
  "Unload the Spread library.

Can be used in conjunction with `reload-spread-library'.

This is mainly intended to be used before saving an image when the
filesystem location of the Spread library is expected to be different
when reloading image."
  (cffi:close-foreign-library 'libspread))

(defun reload-spread-library (&key (if-fails #'error))
  "Reload the Spread library.

Can be used in conjunction with `unload-spread-library'."
  (handler-case
      (cffi:use-foreign-library libspread)
    (cffi:load-foreign-library-error (condition)
      (cond
        ((member if-fails `(,#'warn warn) :test #'eq)
         (warn "~@<Failed to load Spread library: ~A. Spread functionality ~
                will not be available.~@:>"
               condition))
        (t
         (funcall if-fails condition))))))

(defun enable-reload-spread-library (&key (if-fails #'error))
  "Maybe arrange for the Spread library to be reloaded after
saving/reloading the Lisp image.

This may be unnecessary or not even make sense for some Lisps. Do
nothing in these cases.

Currently, this action is irreversible."
  #+sbcl (progn
           (push 'unload-spread-library sb-ext:*save-hooks*)
           (push (lambda () (reload-spread-library :if-fails if-fails))
                 sb-ext:*init-hooks*))
  #-sbcl (error "Reloading libraries is not implemented for this Lisp."))
