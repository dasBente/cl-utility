;;;; threading.lisp

(in-package #:cl-utility/threading)


(defun atom-to-list (arg)
  "Make a list from any given atom or return the argument if it's not a atom"
  (if (atom arg) (list arg) arg))


(defmacro thread (init-arg symb form)
  "Helper macro that threads init-arg into form at the point designated by _"
  (if (atom form) 
      `(,form ,init-arg)
      (let* ((count 0)
             (new-form (loop for l in form collect
                            (if (eql l symb) (progn (incf count) init-arg) l))))
        (cond ((= 0 count) (error "Form ~a should contain symbol ~a but does not." form symb))
              ((> count 1) (error "Form ~a contains more than one instance of ~a." form symb))
              (t new-form)))))


(defmacro as-> (symb init-arg &body body)
  "Function composition macro where the threading can be controlled using a chosen symbol.
Left to right."
  (if body
      `(as-> ,symb (thread ,init-arg ,symb ,(car body)) ,@(cdr body))
      init-arg))


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


(defmacro <-as (symb &body body)
  "Function composition macro where the threading can be controlled using a chosen symbol. 
Right to left"
  `(as-> ,symb ,@(reverse body)))


(defmacro <- (&body body)
  "Function composition macro chaining into first argument from right to left"
  (destructuring-bind (init &rest rest) (reverse body)
    `(-> ,init ,@rest)))


(defmacro <<- (&body body)
  "Function composition macro chaining into last argument from right to left"
  (destructuring-bind (init &rest rest) (reverse body)
    `(->> ,init ,@rest)))
