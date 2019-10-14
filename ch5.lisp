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

...
