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
;; The name "closure" is left over from earlier Lisp dialects. It
;; derives from the way closures have to be implemented under dynamic
;; scope.
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

;;; utils
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (acc fn) (funcall fn acc))
		rest :initial-value (apply fn1 args)))))

(defun disjoin (&rest fns)
  #'(lambda (&rest args)
      (dolist (fn fns nil)
	(when (apply fn args) (return t)))))

(defun conjoin (&rest fns)
  #'(lambda (&rest args)
      (dolist (fn fns t)
	(unless (apply fn args) (return nil)))))

;;; Exercise 1
(defun constituent (x) "x가 토큰을 구성하는 문자열인가?"
  (funcall (conjoin #'graphic-char-p
		    (complement #'(lambda (x) (char= x #\ ))))
	   x))
(defun token (str &key test (start 0) end)
  "str[start, end) 구획에서 토큰을 이루는 substring과 그 위치 반환"
  (let* ((endidx (if (null end) (length str) end))
	 (i (position-if test str :start start :end endidx))
	 (j (let ((endpos (position-if (complement test)
				       str :start i :end endidx)))
	      (if (null endpos) endidx endpos))))
    (values (substring str i j) i j)))
(defun tokens (str &key (test #'constituent) (start 0) end)
  (multiple-value-bind
	(substr startpos endpos) (token str :start start :end end :test test)
    (cons substr (if (or (= endpos (length str))
			 (and end (>= endpos end)))
		     nil
		     (tokens str :test test :start endpos :end end)))))
      
(tokens "I have a dream") 		;=> ("I" "have" "a" "dream")

;;; Exercise 2
(defun bin-search (obj vec &key (key #'eql) (test #'<) (start 0) end)
  (let ((endidx (if (null end) (length vec) end)))
    (if (>= start endidx) nil
	(let* ((mid (floor (/ (+ start endidx) 2)))
	       (midobj (svref vec mid)))
	  (cond ((funcall key obj midobj) mid)
		((funcall test obj midobj)
		 (bin-search obj vec :key key :test test
			     :start start :end mid))
		(t
		 (bin-search obj vec :key key :test test
			     :start (1+ mid) :end endidx)))))))

;;; Exercise 3
(defun argc (&rest rest) (length rest))

;;; Exercise 4
(defun most (fn lst &key (< #'<))
  (let ((max (funcall fn (car lst)))
	(maxidx 0)
	(min (funcall fn (car lst)))
	(minidx 0)
	(idx 1))
    (dolist (item (cdr lst) (values maxidx minidx))
      (let ((res (funcall fn item)))
	(when (funcall < res max) (setf max res maxidx idx))
	(when (funcall < min res) (setf min res minidx idx))
	(incf idx)))))

#|
(most #'(lambda (theta) (sin theta)) 
	       (list 0 (/ pi 2)  pi (* pi (/ 3 2)) (* 2 pi)))
3
1
|#

;;; Exercise 5
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push x acc))))
    (nreverse acc)))
(defun new-remove-if (test lst)
  (filter (complement test) lst))

;;; Exercise 6
(let ((greatest nil))
  (defun max-sofar (x)
    (if (null greatest) (setf greatest x)
	(progn
	  (when (> x greatest)
	    (setf greatest x))
	  greatest))))

;;; Exercise 7
;;; Exercise 8
;;; Exercise 9
;; not interested
