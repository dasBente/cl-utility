;;;; cl-utility.lisp

(in-package #:cl-utility/tests)

(plan 4)

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
 
  (is 3/4 (->> 0 (+ 2) (* 2) (/ 3)))
  (is '(1 2 3) (->> '() (cons 3) (cons 2) (cons 1))))

(finalize)
