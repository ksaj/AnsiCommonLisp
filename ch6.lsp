;;; Global Functions
;; new tool: fboundp, symbol-function
(setf (symbol-function 'add2)
      #'(lambda (x) (+ x 2)))

;; By making the first argument to `defun` a list of form (setf f),
;; you define what happens when the first argument to setf is call to f.
(defun primo (lst) (car lst))
(defun (setf primo) (val lst)
  (setf (car lst) val))
(let ((x '(a b c)))
  (setf (primo x) 480)
  x)
;;=>(480 B C)

;; new tool: documentation
(defun foo (x)
  "identity function"
  x)
(documentation 'foo 'function)		;=> identity function

;;; Local Functions
;; new tool: labels
(labels ((inc (x) (+ x 1))
	 (square (x) (* x x)))
  (square (inc 3)))
;;=> 16

;; `labels` is like rather `let*` than `let`.
;; Functions defined in the labels block can refer each other.

;;; Parameter Lists
;; new tool: &rest
(defun our-funcall (fn &rest args)
  (apply fn args))

;; new tool: &optional
(defun philosoph (thing &optional property)
  (list thing 'is property))
(philosoph 'death)			;=> (DEATH IS NIL)

;; we can give an explicit default for the optional parameter.
(defun philosoph (thing &optional (property 'fun))
  (list thing 'is property))
(philosoph 'death)			;=> (DEATH IS FUN)
;; The default for an optional parameter need not be a constant. It
;; can be any Lisp expression.

;; new tool: key
(defun cmp (a b &key (test #'<))
  (funcall test a b))
(cmp 1 2)				;=> T
(cmp 1 2 :test #'>) 			;=> NIL

;; keywords and their associated arguments can be controlled in the
;; `rest` parameters and apssed on to other functions that are
;; expecting them. for example:
(defun our-adjoin (obj lst &res args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

;; destructuring-bind can hold also these complex parameters
(destructuring-bind ((&key w x) &rest y) '((:w 3 :x 1) a b c)
  (list w x y))
;;=> (3 1 (A B C))

;;; Closures

(defun combiner (x)
  (typecase x
    (number #'+)
    (list   #'append)
    (t      #'list)))

(mapcar (complement #'oddp)
	'(1 2 3 4 5 6))
;;=> (NIL T NIL T NIL T)

(defun our-complement (f)
  #'(lambda (&rest args)
      (not (apply f args))))

;;; Dynamic Scope
;; special variable
(setf x 5)
(let ((x 10))
  (defun foo()
    (declare (special x))
    x))
(foo)					;=> 5
;; A `declare` can begin any body of code where new variables are created.
(let ((x 20))
  (declare (special x))
  (foo)) 				;=> 20

;; example of special variable
(let ((*print-base* 16))
  (princ 32))
;; 20     # 10-base decimal 32 in 16-base.
;; => 32

;;; Compilation
;; Common lisp functions can be compiled either individual or by the file.

;; new tool: compiled-function-p, compile
(defun foo(x) (+ x 1))
(compiled-function-p #'foo)		;=> NIL
(compile 'foo)
(compiled-function-p #'foo)		;=> T

;; There is one kind of function you can't give as an argument to
;; compile: a function that was typed into the toplevel within a
;; distinct lexical context(e.g. `let`). But it would be ok to define
;; these functions within a file, and then compile and load the file.

;; new tool: compile-file

;; When one function occurs within another, and the containing
;; function is compiled, the inner function should also be compiled.

(defun make-adder (x)
  #'(lambda (n) (+ n x)))
(compile 'make-adder)
(compiled-function-p (make-adder 2))	;=> T


;;; Exercise 1

...
