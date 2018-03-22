;;;; package.lisp

(defpackage #:cl-utility/threading
  (:use #:cl)
  (:export #:-> #:->> #:<- #:<<- #:as-> #:<-as))

(defpackage #:cl-utility
  (:use #:cl)
  (:export #:\\ #:destructuring-lambda #:f-and #:f-or #:multi-compare #:destructuring-let))
