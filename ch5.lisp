;;; Blocks
;; new tool: block(a progn with name and emergency exit), return
(block myblk
  (return 27)
  kkkkkkkkkkkkkkkkkkkkkk) ;; will be evaluated never
;; => 27

;; new tool: return-from
(block head
  (format t "head~%")
  (block body
    (format t "body~%")
    (return-from head 'body)
    (format t "/body~%"))
  (return-from head 'head)
  (format t "/head~%"))
;; head
;; body
;; => BODY

;; Many Common lisp operators that take a body of expression
;; implicitly enclose the body in a `block` named `nil`.
;; All iteration constructs do.
(dolist (x '(a b c d e))
  (format t "~A " x)
  (if (eql x 'c)
      (return 'done)))
;; A B C
;; => DONE

;; The body of a function also a implicit block, whose name is same as
;; the function's name
(defun foo ()
  (return-from foo 27))

;; new tool: tagbody
;; tagbody has goto
(tagbody
   (setf x 0)
 top
   (setf x (+ x 1))
   (format t "~A " x)
   (if (< x 10) (go top)))
;; 1 2 3 4 5 6 7 8 9 10

;;; Context
;; tools: let, let*

;; In both let and let*, initial values default to nil. Such variables
;; need not be enclosed within lists:
(let (x y)
  (list x y))				;=> (NIL NIL)

;; new tool: destructuring-bind(Generalized let)
;; it takes a pattern and binds them to the corresponding parts of
;; some actual tree.
(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))
;; => (A B C (D E))

;;; Conditional
;; In `cond` : If there are no expressions after the successful condition
(cond (99))				;=> 99
;; the value of the condition itself is returned.

;; new tool: case
;; the keys in case will never be evaluated. keys are treated as constants
(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-year) 29 28))
    (otherwise "unknown month")))

;; new tool: typecase
;; typecase is similar to case, except that the keys in each clause
;; should be type specifiers

;;; Iteration
;; `do` contins both an implicit `block` and an implicit `tagbody`, it
;; is possible to use `return`, `return-from`, and `go` in its body.

#| basic structure of do(or do*)

(do ((variable1 initial update-expr) (variable2 ...) ...)
     (stop-expr resultform)
  body)

if `initial` is omitted, default value is nil
if `update-expr` is ommitted, variable will not be updated automatically.
|#

;; tools: dolist, dotimes
;; new tool: mapc (similar to mapcar, but does not cons up)
;; mapc is more flexible than dolist, because it can traverse multiple
;; lists in parallel.

(mapc #'(lambda (x y)
	  (format t "~A ~A " x y))
      '(hip flip slip)
      '(hop flop slop))
;; HIP HOP FLLIP FLOP SLIP SLOP
;; => (HIP FLIP SLIP)

;; It *always* returns its second argument.

;;; Multiple Values

;; new tool: values, multiple-value-bind,call,list
(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))
;;=> (1 2 3)

(multiple-value-call #'+ (values 1 2 3)) ;=> 6
(multiple-value-list (values 'a 'b 'c))	 ;=> (A B C)

;;; Aborts
;; new tool: catch, abort
;; new tool: error, unwind-protect

;;; Exercise 1 (a)
((lambda (x)
   (cons x x))
 (car y))
;;; Exercise 1 (b)
((lambda (w)
   ((lambda (y)
      (cons w y))
    (+ w z)))
 (car x))

;;; Exercise 2
(defun mystery (x y)
  (cond ((null y) nil)
	((eql (car y) x) 0)
	(t (let ((z (mystery x (cdr y))))
	     (and z (+ z 1))))))

;;; Exercise 3
(defun solve3 (n)
  (if (and (< 0 n) (<= n 5)) n
      (* n n)))

;;; Exercise 4
;; not interested.

;;; Exercise 5
;; iterative
(defun precedes (chr str)
  (let ((len (length str)))
    (if (< len 2) (return nil))
    (let ((res nil))
      (dotimes (i (- len 1) res)
	(when (char= chr (char str (1+ i)))
	  (pushnew (char str i) res))))))

;; recursive
(defun precedes-rec (chr str)
  (let ((len (length str)))
    (if (< len 2) nil
	(prec-iter chr str 0 len nil))))

(defun prec-iter (chr str idx len res)
  (let ((nextidx (1+ idx)))
    (if (not (< nextidx len)) res
	(prec-iter chr str nextidx len
		   (if (char= chr (char str nextidx))
		       (adjoin (char str idx) res) res)))))
;;; Exercise 6
;; recursive
(defun intersperse-rec (salt lst)
  (if (null lst) nil
      (cons (car lst)
	    (if (cdr lst) (cons salt (intersperse-rec salt (cdr lst)))
		(intersperse-rec salt (cdr lst))))))

;; tail-recursive
(defun intersperse-tc (salt lst)
  (if (null lst) nil
      (intersperse-tc-iter salt lst nil)))

(defun intersperse-tc-iter (salt lst res)
  (if (null lst) (reverse (cdr res))
      (let ((tail (cdr lst)))
	(intersperse-tc-iter salt tail
			     (cons salt (cons (car lst) res))))))

;; iterative
(defun intersperse-iter (salt lst)
  (if (null lst) nil
      (let ((res nil))
	(dolist (item lst (reverse (cdr res)))
	  (push item res)
	  (push salt res)))))

;; reduce
(defun intersperse-reduce (salt lst)
  (reverse
   (cdr
    (reduce #'(lambda (acc item)
		`(,salt ,item . ,acc))
	    lst :initial-value nil))))

;;; Exercise 7 (a)
(defun diff1 (a b)
  (= 1 (abs (- a b))))
(defun succp-rec (numlst)
  (if (null (cdr numlst)) nil
      (succp-rec-iter numlst nil)))
(defun succp-rec-iter (numlst res)
  (if (null (cdr numlst)) res
      (if (diff1 (first numlst) (second numlst))
	  (succp-rec-iter (cdr numlst) t)
	  nil)))
;;; Exercise 7 (b)
;; It'll be a bit hard to understand.
(defun succp-do (numlst)
  (if (null (cdr numlst)) nil
      (do ((fst (first numlst) (first numlst))
	   (snd (second numlst) (second numlst))
	   (res t (and res (diff1 fst snd)))
	   (numlst (cdr numlst) (cdr numlst)))
	  ((null numlst) res))))
;;; Exercise 7 (c)
(defun succp-mapc (numlst)
  (if (null (cdr numlst)) nil
      (let ((prev (car numlst)))
	(mapc #'(lambda (curr)
		  (unless (diff1 prev curr)
		    (return-from succp-mapc nil))
		  (setf prev curr))
	      (cdr numlst))
	t)))

;;; Exercise 8
(defun minmax (vec)
  (case (length vec)
    (0 nil)
    (1 (let ((e (svref vec 0)))
	 (values e e)))
    (2 (let ((e0 (svref vec 0))
	     (e1 (svref vec 1)))
	 (if (< e0 e1) (values e0 e1)
	     (values e1 e0))))
    (otherwise
     (let ((e0 (svref vec 0))
	   (e1 (svref vec 1)))
       (multiple-value-bind (min max)
	   (minmax  (subseq vec 2))
	 (multiple-value-bind (local-min local-max)
	     (if (< e0 e1) (values e0 e1)
		 (values e1 e0))
	   (values (if (< local-min min) local-min min)
		   (if (> local-max max) local-max max))))))))

;;; Exercise 9
;; not interested.
