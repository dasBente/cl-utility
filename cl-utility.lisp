;;;; cl-utility.lisp

(in-package #:cl-utility)

(defun atom-to-list (arg)
  "Make a list from any given atom or return the argument if it's not a atom"
  (if (atom arg) (list arg) arg))

(defmacro -> (init-arg &body body)
  "Function composition macro chaining into first argument from left to right"
  (if body
      (destructuring-bind (fn &rest args) (atom-to-list (car body))
        `(-> (,fn ,init-arg ,@args) ,@(cdr body)))
      init-arg))

(defmacro ->> (init-arg &body body)
  "Function composition macro chaining into last argument from left to right"
  (if body
      (destructuring-bind (fn &rest args) (atom-to-list (car body))
        `(->> (,fn ,@args ,init-arg) ,@(cdr body)))
      init-arg))

(defmacro <- (&body body)
  "Function composition macro chaining into first argument from right to left"
  (destructuring-bind (init &rest rest) (reverse body)
    `(-> ,init ,@rest)))

(defmacro <<- (&body body)
  "Function composition macro chaining into last argument from right to left"
  (destructuring-bind (init &rest rest) (reverse body)
    `(->> ,init ,@rest)))

