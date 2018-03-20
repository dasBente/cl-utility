;;;; cl-utility.asd

(asdf:defsystem #:cl-utility
  :description "Describe cl-utility here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:let-over-lambda)
  :components ((:file "package")
               (:file "cl-utility")))
