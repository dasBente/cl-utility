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


(defun split-until (lst until)
  "Split a list into all elements found until and after a condition is met"
  (let (split left right)
    (loop for l in lst do
         (cond ((funcall until l) (progn (setf split t)
                                         (setf right (cons l right))))
               (split (setf right (cons l right)))
               (t (setf left (cons l left))))
       finally (return (list (nreverse left) (nreverse right))))))


(defun split-delimited (lst delimiter)
  "Split a list at a given delimiter"
  (let ((res (split-until lst (lambda (l) (eql l delimiter)))))
    (list (car res) (cdadr res))))


(defmacro \\ (&body body)
  "Haskell style lambda. Shorthand for destructuring-lambda, with or without Haskell style syntax."
  (if (find '-> body) 
      (destructuring-bind (vars forms) (split-delimited body '->) ; Hakell lambda
        `(destructuring-lambda ,vars ,@forms)) 
      `(destructuring-lambda ,(car body) ,@(cadr body)))) ; Lispy lambda definition)


(defmacro destructure* (to-destruct &body body)
  "Takes a list to-destruct and destructures all of it's elements."
  (if to-destruct
      (destructuring-bind ((pattern &optional form) &rest rest) to-destruct
        `(destructuring-bind ,pattern ,form (destructure* ,rest ,@body)))
      `(progn ,@body)))


(defmacro destructuring-let (vars &body body)
  "A variant of let* directly supporting destructuring binds."
  (let* ((arg-split (split-until vars (lambda (l) (and (not (atom l)) (listp (car l)))))))
    (destructuring-bind (simple-args rest) arg-split
      (if simple-args
          `(let* ,simple-args (destructuring-let ,rest ,@body))
          (let* ((rest-split (split-until rest (lambda (l) (or (atom l) (not (listp (car l))))))))
            (destructuring-bind (to-dest new-rest) rest-split
              (if to-dest
                  `(destructure* ,to-dest (destructuring-let ,new-rest ,@body))
                  `(progn ,@body))))))))
