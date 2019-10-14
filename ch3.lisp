#| eq vs eql vs equal
[[http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node74.html]
 [Equality Predicates]]

eq : identity test 
(ex. (eq 3 3) ;=> t)
(ex. (eq 3.0 3.0) ;=> nil)

eql : eq or objects have same numeric type or char type and same value
(ex. (eq 3.0 3.0) ;=> t)
(ex. (eql '(a b) '(a b)) ;=> nil)
(ex. (eq 3 3.0) ;=> nil)

equal : returns true if its arguments would print the same
(ex. (equal '(a b) '(a b)) ;=> t)
(ex. (eq 3 3.0) ;=> nil)
|#
(defun our-atom (item)
  (not (consp item)))
(defun our-listp (item)
  (or (null item) (consp item)))

;; new tool: copy-list
(setf x '(a b c)
      y (copy-list x))
#|
memory representation
x -> [ ] [ ] [ ]
      v   v   v
      a   b   c
      ^   ^   ^
y -> [ ] [ ] [ ]
This implies that 
  `x` and `(copy-list x)` are always `equal`.
|#

;; new tool: mapcar and maplist
;; and there are other mapping functions `mapc`, `mapcan`
(mapcar #'(lambda (x) x)
	'(a b c)) 			; => '(a b c)
(maplist #'(lambda (x) x)
	 '(a b c))			; => '((a b c) (b c) c)

;;; new tool: copy-tree
;; 리스프의 tree는 말그대로의 트리 구조를 나타낸다기 보다는, nested
;; list의 원소에 대해 연산을 적용하고 싶을 때 쓰인다. 예를 들면,
;; substitute는 리스트 연산인데 반해, subst는 트리 연산이다.
(substitute 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; => (AND (INTEGERP X) (ZEROP (MOD X 2)))
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))
;; => (AND (INTEGERP Y) (ZEROP (MOD Y 2)))

;;; Keyword arguments

;; new tool: member
;; member compares elements with `eql`
;; if you want to change this, :test keyword-argument will help.
(member '(a) '((a) (z))) 		;=> nil
(member '(a) '((a) (z)) :test #'equal)	;=> ((A) (Z))
;; the :key argument
(member 'a '((a b) (c d)) :key #'car) 	;=> ((A B) (C D))

;; new tool: member-if
(member-if #'oddp '(2 3 4))		;=> (3 4)

;;; Set

;; new tool: adjoin and union, intersection, set-difference
(adjoin 'b '(a b c))			;=> (A B C)
(adjoin 'z '(a b c))			;=> (Z A B C)
(union '(a b c) '(c b s))		;=> (A C B S)
(intersection '(a b c) '(b b c))	;=> (B C)
(set-difference '(a b c d e) '(b e))	;=> (A C D)

;;; Sequence(lists and vectors)

;; new tool: length
(length '(a b c))			;=> 3
;; (subseq  lst n &optional m) => lst[n:m)
(subseq '(a b c d) 1 2)			;=> (B)
(subseq '(a b c d) 1)			;=> (B C D)
;; reverse
(reverse '(a b c))			;=> (C B A)
;; sort (CAUTION: sort is destructive(has side-effect))
(sort '(0 2 1 3 8) #'>)			;=> (8 3 2 1 0)
;; every and some
(every #'oddp '(1 3 5))			;=> T
(some #'evenp '(1 2 3))			;=> T
;; example of every #'> for multiple sequence
(every #'> '(1 3 5) '(0 2 4))		;=> T

;;; Stacks
;; new tool: push and pop
;; new tool: pushnew(using adjoin instead of cons for pushing)

;;; Assoc-lists
;; new tool: assoc
(setf trans '((+ . "add") (- . "subtract")))
(assoc '+ trans)			;=> (+ . "add")
(assoc '* trans)			;=> NIL


;;; 가비지 컬렉션과 리스프 프로그램을 짜는 전략
;; 리스프의 가비지는 대개 consing과 관련되어 있다. consing은 매우
;; 유용하지만, 성능문제를 야기하기 마련이다. 따라서 처음 프로그램은
;; 많은 리스트를 사용해 순수함수형으로 개발하고, 프로그램을
;; 발전시켜나감에 따라 점차 리스트의 사용을 줄여 나가는 것이
;; 일반적으로 유효한 전략이다.

;;; Exercise 2
;; using push
(defun new-union (lst1 lst2)
  (let ((r (copy-list lst1)))
    (dolist (item lst2 r)
      (if (not (member item lst1))
	  (push item (cdr (last r)))
	  nil))))

;;; Exercise 3
(defun occurrences (lst)
  (let ((dict nil))
    (dolist (item lst (sort dict #'> :key #'cadr))
      (setf dict (inc-assoc item dict)))))
(defun inc-assoc (key plist)
  (let ((pair (assoc key plist)))
    (if (eq pair nil)
	(set-assoc key 1 plist)
	(set-assoc key (1+ (cadr pair)) plist))))
(defun set-assoc (key value plist)
  (if (null (assoc key plist))
      (cons (list key value) plist)
      (change-assoc key value plist)))
(defun change-assoc (key value plist)
  (mapcar #'(lambda (pair)
	      (if (eq key (car pair))
		  (list (car pair) value)
		  pair))
	  plist))

;;; Exercise 5 (a)
(defun pos+rec (lst)
  (reverse (pos+rec-iter 0 lst nil)))
(defun pos+rec-iter (idx lst res)
  (if (null lst) res
      (pos+rec-iter (+ idx 1)
		(cdr lst)
		(cons (+ idx (car lst)) res))))
;;; Exercise 5 (b)
(defun pos+iter (lst)
  (let ((idx 0)
	(res nil))
    (dolist (item lst (reverse res))
      (push (+ idx item) res)
      (incf idx))))
;;; Exercise 5 (c)
(defun pos+mapcar (lst)
  (let ((idx -1))
    (mapcar #'(lambda (x)
		(+ (incf idx) x))
	    lst)))

;;; Exercise 6
(defun new-cons (cdr car)
  (cons cdr car))
(defun new-list (&rest items)
  (let ((s NIL))
    (dolist (item (reverse items) s)
      (setf s (new-cons s item)))))
(defun new-length (new-lst)
  (if (null new-lst) 0
      (1+ (new-length (car new-lst)))))
(defun new-member (item new-lst)
  (if (null new-lst) nil
      (if (eql item (cdr new-lst)) new-lst
	  (new-member item (car new-lst)))))

;;; Exercise 7
;; We can use fewer cons cells by just modifying `n-elts` function
(defun compress (x)
  (if (atom x) x
      (compr (car x) 1 (cdr x))))
(defun compr (elt n lst)
  "elt: atom, n: integer>0, lst: list"
  (if (null lst) (list (n-elts elt n))
      (if (eql (first lst) elt)
	  (compr elt (+ n 1) (cdr lst))
	  (cons (n-elts elt n)
		(compr (car lst) 1 (cdr lst))))))
(defun n-elts (elt n)
  (if (<= n 1) elt
      (cons n elt))) ;; changed: list->cons

;;; Exercise 8
(defun showdots (x)
  (if (atom x) (format t "~A" x)
      (let ((len (length x)))
	(dolist (item x)
	  (format t "(~A . " item))
	(format t "NIL")
	(dotimes (i len)
	  (format t ")")))))

;;; Exercise 9
;; adjacent list (refer. network.bmp)
(setf net '((1 2 3 7) (2 1 3 4) (3 1 2 5)
	    (4 2 5 7) (5 3 4 6) (6 5 7) (7 1 6)))
(defun longest-path (start end net)
  (reverse (dfs start end net (list start))))
(defun dfs (start end net visit)
  "visit: '(start ...)"
  (if (eql start end) visit
      (let ((next (remove-if #'(lambda (node) (member node visit))
			     (assoc start net))))
	(if (null next) nil
	    (let ((paths (mapcar #'(lambda (node)
				     (dfs node end net (cons node visit)))
				 next))
		  (longest nil)    ;; mutable
		  (longest-len 0)) ;; mutable
	      (dolist (path paths longest)
		(let ((path-len (length path)))
		  (if (> path-len longest-len)
		      (setf longest path
			    longest-len path-len)
		      nil))))))))
