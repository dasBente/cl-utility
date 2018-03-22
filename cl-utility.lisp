;;;; cl-utility.lisp

(in-package #:cl-utility)


(defmacro destructuring-helper (to-destruct body)
  "Helps destructure the argument list of the destructuring-lambda."
  (destructuring-bind ((symb arg-list) &rest rest) to-destruct
    (if rest
        `(destructuring-bind ,arg-list ,symb (destructuring-helper ,rest ,body))
        `(destructuring-bind ,arg-list ,symb ,@body))))


(defun f-or (&rest args)
  "Functional or. Evaluates parameters beforehand!"
  (when args (if (car args) (car args) (apply #'f-or (cdr args)))))


(defun f-and (&rest args)
  "Functional and. Evaluates parameters beforehand!"
  (if args
      (if (cdr args) 
          (when (car args) 
            (apply #'f-and (cdr args))) 
          (car args))
      t))


(defun multi-compare (reference compare-to &key (test #'eql) (acc #'f-or))
  "Compares multiple values to a reference value using test then accumulating the result using ac."
  (let ((comparator (lambda (x) (funcall test reference x))))
    (apply acc (mapcar comparator compare-to))))


(defun special-arg-p (arg)
  (multi-compare arg '(&optional &key &rest &body)))


(defun get-destruct-arglist (args)
  "Helper function that processes a arg-list as required by destructuring-lambda"
  (let ((destruct t)) 
    (reduce (lambda (res arg) 
              (if (atom arg) 
                  (progn 
                    ;; Don't destructure key and optional args (yet?)
                    (when (special-arg-p arg)
                      (setf destruct nil))
                    (cons (list arg) res))
                  (if destruct 
                      (cons (list (gensym) arg) res)
                      (cons (list arg) res))))
            args :initial-value nil)))


(defmacro destructuring-lambda (args &body body)
  "Lambda macro employing list destructuring in it's arg list."
  (let* ((arg-list (nreverse (get-destruct-arglist args)))
         (symbs (mapcar #'car arg-list))
         (to-destruct (remove-if (lambda (i) (= 1 (length i))) arg-list)))
    (if to-destruct
        `(lambda ,symbs (destructuring-helper ,to-destruct ,body))
        `(lambda ,symbs ,@body))))


(defun split-list (lst delimiter)
  "Split a list at a given delimiter"
  (let (split left right)
    (loop for l in lst do
         (cond ((eql l delimiter) (setf split t))
               (split (setf right (cons l right)))
               (t (setf left (cons l left))))
       finally (return (list (nreverse left) (nreverse right))))))


(defmacro \\ (&body body)
  "Haskell style lambda. Shorthand for destructuring-lambda, with or without Haskell style syntax."
  (if (find '-> body) 
      (destructuring-bind (vars forms) (split-list body '->) ; Hakell lambda
        `(destructuring-lambda ,vars ,@forms)) 
      `(destructuring-lambda ,(car body) ,@(cadr body)))) ; Lispy lambda definition)

(defmacro destructuring-let (vars &body body)
  "A variant of let* directly supporting destructuring binds."
  'false)
