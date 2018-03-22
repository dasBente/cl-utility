;;;; cl-utility.asd

(asdf:defsystem #:cl-utility
  :description "A collection of utility functions for Common Lisp."
  :author "Tobias Möhring <dasbente@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:let-over-lambda)
  :components ((:file "package")
               (:file "threading")
               (:file "cl-utility"))
  :in-order-to ((test-op (test-op :cl-utility/tests))))

(asdf:defsystem #:cl-utility/tests
  :depends-on (#:cl-utility
               #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "threading")
               (:file "core"))
  :perform (test-op :after (o c)
                    (funcall (intern #.(string :run) :prove) c)))
