;;;; cl-utility.lisp

(in-package #:cl-utility/tests)

(plan 6)

#| 
The <- macro allows for chaining multiple "partial" functions after another by passing the following 
function as the first argument of it's predecessor. The resulting expression is thus built right
to left.
  (<- f (g 2) (h 3) x) is meant to represent f(g(h(x, 3), 2)). 
|#
(subtest "Testing <- macro"
  ;; Simple function chaining using only unary functions
  (is 3 (<- 1+ 1+ 1+ 0))

  ;; Some tests using higher arity expressions
  (is 3 (<- (/ 2) (* 3) (+ 2) 0))
  (is '((1 . 2) . 3) (<- (cons 3) (cons 2) 1)))

#|
The <<- macro allows for chaining multiple "partial" functions after another by passing the following
function as the last argument of it's predecessor. The resulting expression is thus built right 
to left.
  (<<- f (g 2) (h 3) x) is meant to represent f(g(2, h(3, x))).
|#
(subtest "Testing <<- macro"
  ;; Simple function chaining using only unary functions
  (is 3 (<<- 1+ 1+ 1+ 0))

  ;; Some tests using higher arity expressions
  (is 2/6 (<<- (/ 2) (* 3) (+ 2) 0))
  (is '(1 2 . 3) (<<- (cons 1) (cons 2) 3))
  (is '(1 2 3) (<<- (concatenate 'list '(1)) (cons 2) (cons 3) 'nil)))

#|
Analogously to Clojures -> threading macro. Like <- but reads from left to right.
  (-> x f (g 2) (h 3)) is meant to represent h(g(f(x), 2), 3).
|#
(subtest "Testing -> macro"
  ;; Simple function chaining using only unary functions
  (is 3 (-> 0 1+ 1+ 1+))

  ;; Some tests using higher arity expressions
  (is 5 (-> 0 (+ 2) (/ 2) (* 5)))
  (is '((1 . 2) . 3) (-> 1 (cons 2) (cons 3))))

#|
Analogously to Clojures ->> threading macro. Like <<- but reads from left to right.
|#
(subtest "Testing ->> macro"
  ;; Simple function chaining using only unary functions
  (is 3 (->> 0 1+ 1+ 1+))
 
  ;; Some tests using higher arity expressions
  (is 3/4 (->> 0 (+ 2) (* 2) (/ 3)))
  (is '(1 2 3) (->> '() (cons 3) (cons 2) (cons 1))))

#|
Analogously to Clojures as-> threading macro. A symbol can be chosen which will then be substituted
by the already assembled expression starting from the left. A function atom will be treated as a
unary function and does not require the substitution symbol.
The substitution symbol has to be explicitly given for any non-atom expression.
  (as-> x _ f (g _ 2) (h 3 _)) is meant to represent h(3, g(f(x), 2)).
|#
(subtest "Testing as-> macro"
  ;; Simple function chaining using only unary functions
  (is 3 (as-> 0 _ 1+ 1+ 1+))

  ;; Some tests using higher arity expressions
  (is -5 (as-> 0 _ (- _ 10) (* 2 _) (/ _ 4)))
  (is '((2 1) . 3) (as-> '() _ (cons 1 _) (cons 2 _) (cons _ 3)))

  ;; Error if at least one form does not contain a substitution symbol
  ;; (is-error (as-> 0 _ (+ 1 _) (+ 2)) 'error)

  ;; Error if a form contains more than one instance of _
  ;; (is-error (as-> 0 _ (+ 1 _) (+ 2 _ _)) 'error)
  )

#|
Like as-> but read right to left (except the substitution symbol).
  (<-as _ f (g _ 2) (h 3 _) x) is meant to represent f(g(h(3, x), 2)).
|#
(subtest "Testing <-as macro"
  ;; Simple function chaining using only unary functions
  (is 3 (<-as _ 1+ 1+ 1+ 0))

  ;; Some tests using higher arity expressions
  (is -5 (<-as _ (/ _ 4) (* 2 _) (- _ 10) 0))
  (is '(1 2 nil . 3) (<-as _ (cons 1 _) (cons 2 _) (cons _ 3) '()))
  
  ;; Error if at least one form does not contain a substitution symbol
  ;; (is-error (<-as _ (+ 1 _) (+ 2) 0) 'error)

  ;; Error if a form contains more than one instance of _
  ;; (is-error (<-as _ (+ 1 _ _) (+ 2 _) 0) 'error)
  )

(finalize)
