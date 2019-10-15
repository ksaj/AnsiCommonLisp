;;;; Section 6.2 Function Builders

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  "returns a predicate that returns true when any of the predicates
  return true"
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  "returns a predicate that returns true when all of the predicates
  return true"
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
	#'(lambda (&rest args)
	    (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x) #'(lambda (&rest args) x))

#|
 cddr  = (compose #'cdr #'cdr)
  nth  = (compose #'car #'nthcdr)
 atom  = (compose #'not #'consp)
       = (rcurry #'typep 'atom)
  <=   = (disjoin #'< #'=)
 listp = (disjoin #'null #'consp)
       = (rcurry #'typep 'list)
  1+   = (curry #'+ 1)
       = (rcurry #'+ 1)
  1-   = (rcurry #'- 1)
mapcan = (compose (curry #'apply #'nconc) #'mapcar)
complement = (curry #'compose #'not)
|#
