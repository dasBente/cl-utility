;;;; cl-utility.lisp

(in-package #:cl-utility/tests)

(plan 8)

#|
The f-or function is meant to be a variant of the or macro to be used in higher order functions.
  (f-or (zerop 2) (plusp -1) 5) ;; => 5
|#
(subtest "Testing f-or"
  ;; Empty arg list
  (is (f-or) nil)

  ;; Some args
  (is (f-or t) t)
  (is (f-or (zerop 2) (plusp -1) 5) 5)
  (is (f-or 1 2 3) 1)
  (is (f-or nil) nil))

#|
The f-and function is meant to be a variant of the and macro to be used in higher order functions.
  (f-and (zerop 0) (plusp 1) 5) ;; => 5
|#
(subtest "Testing f-and"
  ;; Empty arg list
  (is (f-and) t)
  
  ;; Some args 
  (is (f-and t) t)
  (is (f-and (zerop 0) (plusp 1) 5) 5)
  (is (f-and (zerop 0) (plusp -1) 5) nil)
  (is (f-and nil) nil))

#|
The multi-compare function compares it's reference value to a list of other possible values using
a specified test function (implicitly set to #'eql) and accumulating the result (implicitly set to
#'f-or).
  (multi-compare 'a '(a b c d) :test #'eql :acc #'f-or) ;; => 'a 
|#
(subtest "Testing multi-compare"
  (is (multi-compare 'a '(a b c d) :test #'eql :acc #'f-or) t)
  (is (multi-compare 'a '(a b c d) :test #'eql :acc #'f-and) nil)
  (is (multi-compare 5 '(6 7 8 9) :test #'< :acc #'f-or) t)
  (is (multi-compare 5 '(6 7 8 9) :test #'> :acc #'f-or) nil)
  (is (multi-compare 0 '(1 2 3 4) :test #'+ :acc #'+) 10))

#|
The destructuring-lambda macro enables using list destructuring in lambda function definitions.
  (destructuring-lambda (a (b c) &rest d) (concatenate 'list (list a b c) d))
|#
(subtest "Testing destructuring-lambda - Non-destructuring calls (normal lambda)"
  ;; 0-arity
  (is-expand (destructuring-lambda () 5) (lambda () 5))
  ;(ok (equal (macroexpand '(destructuring-lambda () 5)) '#'(lambda () 5)))
  (is (funcall (destructuring-lambda () 5)) 5)

  ;; multiple-arity
  (is-expand (destructuring-lambda (a b) (+ a b)) (lambda (a b) (+ a b)))
  (is (funcall (destructuring-lambda (a b) (+ a b)) 3 5) 8)

  ;; keys
  (let ((f (destructuring-lambda (&key a) a)))
    (is (funcall f) nil)
    (is (funcall f :a 'hello) 'hello))
  
  ;; keys with default value (TODO)
  (let ((f (destructuring-lambda (&key (a 0)) a)))
    (is (funcall f) 0)
    (is (funcall f :a 1) 1))

  ;; optionals
  (let ((f (destructuring-lambda (&optional a) a)))
    (is (funcall f) nil)
    (is (funcall f 'hello) 'hello))

  ;; optionals with default values (TODO)
  (let ((f (destructuring-lambda (&optional (a 0)) a)))
    (is (funcall f) 0)
    (is (funcall f 1) 1))

  ;; rest 
  (let ((f (destructuring-lambda (&rest a) a)))
    (is (funcall f) nil)
    (is (funcall f 'hello) '(hello)))

  ;; insufficient argument count
  ;(is-error (funcall (destructuring-lambda (a) a)) 'error)
  )

(subtest "Testing destructuring-lambda - Destructuring calls"
  ;; gensyms are used as substitutes for destructuring lambda lists, so expansion testing won't work
  (is (funcall (destructuring-lambda (a (b c)) (list a b c)) 'a '(b c)) '(a b c))
  (is (funcall (destructuring-lambda (a &rest b) (list a b)) 'a 'b 'c) '(a (b c)))
  (is (funcall (destructuring-lambda (&optional a) a)) nil)
  (is (funcall (destructuring-lambda (&optional a) a) 42) 42)
  
  ;; errors for insufficient arguments being supplied to a destructured argument
  ;;(is-error (funcall (destructuring-lambda ((a b)) (+ a b)) '(1)) 'error)
  )

(subtest "Testing destructuring-lambda - Nested destructuring calls"
  (is (funcall (destructuring-lambda (a (b (c))) (list a b c)) 'a '(b (c))) '(a b c))
  (is (funcall (destructuring-lambda (a (b &rest c)) (list a b c)) 'a '(b c)) '(a b (c)))
  (let ((f (destructuring-lambda (a (b &optional c) d) (list a b c d))))
    (is (funcall f 'a '(b) 'd) '(a b nil d))
    (is (funcall f 'a '(b c) 'd) '(a b c d))))

#|
The destructuring-let macro employs destructuring directly within a let* form.
  (destructuring-let (((a &rest b) '(1 2 3))) (list a b)) => '(1 (2 3))
|#
(subtest "Testing destructuring-let - No destructuring"
  (is (destructuring-let (a) a) 'nil)
  (is (destructuring-let ((a 1)) a) 1)
  (is (destructuring-let ((a '(1)) (b (car a))) b) 1))

(subtest "Testing destructuring-let - Destructuring"
  (is (destructuring-let ((&rest a)) a) nil)
  (let ((lst '(1 (2 3) 4 5)))
    (is (destructuring-let (((a (b c) d e) lst)) (list a b c d e)) '(1 2 3 4 5))
    (is (destructuring-let (((a b &rest c) lst)) (list a b c)) '(1 (2 3) (4 5))))
  
  (let ((lst '(1 2 3)))
    (is (destructuring-let (((a b c &optional d) lst)) (list a b c d)) '(1 2 3 nil))
    (is (destructuring-let (((a b &optional c d) lst)) (list a b c d)) '(1 2 3 nil))
    (is (destructuring-let (((a b c &optional (d 4)) lst)) (list a b c d)) '(1 2 3 4))
    (is (destructuring-let (((a b &optional (c t)) lst)) (list a b c)) '(1 2 3))
    (is (destructuring-let (((a b &optional (c t) (d 4)) lst)) (list a b c d)) '(1 2 3 4))))

(finalize)
