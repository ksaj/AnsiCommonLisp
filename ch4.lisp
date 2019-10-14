;;;; Chapter 4

;;; Array
;; new tool: make-array, aref
;; to make a 2x3 array. :initial-element is optional
;; [CAUTION] retrieving an element from uninitialized array is UB.
(setf arr (make-array '(2 3) :initial-element nil))
(setf vec (make-array 4 :initial-element nil))
(setf (aref arr 1 2) 'END)

;; in some implementation, printing array in #(...) form is not default
;; to make sure for the repl to print array:
(setf *print-array t)

;; to denote a literal array, we use the #<n>a syntax
(setf arr2 #2a((b nil nil) (nil nil nil)))

;;; Vector: one-dimensional array
;; new tool: vector, svref (sv stands for "simple vector")
(vector 'a 'b 'c 1 2 3)			;=> #(A B C 1 2 3)
(setf vec2 #(a b c 1 2 3))		;=> #(A B C 1 2 3)
(svref vec2 0)				;=> A

;;; Strings(:vectors of characters) and Characters
;; new tool: char-code, char<,<=,>,>=,/=,=
(sort "elbow" #'char<)			;=> "below"
;; new tool: char
(eql (aref "abc" 1) (char "abc" 1))	;=> T
;; new tool: string-equal(similar to equal, but ignore case.)
(equal "fred" "fred")			;=> T
(equal "fred" "Fred")			;=> NIL
(string-equal "fred" "Fred")		;=> T
;; Calling format with nil as the first argument return as string.
(format nil "~A or ~A" "truth" "dare")	;=> "truth or dare"
;; new tool: concatenate
(concatenate 'string "not " "to worry")	;=> "not to worry"

;;; Sequences
;; Common lisp type sequence includes both lists and vectors(surely, string)

;; new tool: elt
;; elt: same as `nth` for lists, as `aref` and `svref` for vectors, and as
;; `char` for strings

#||
standard set of keyword parameters for sequence funcitons
| parameter | purpose                             | default  |
|-----------+-------------------------------------+----------|
| :key      | a function to apply to each element | identity |
| :test     | the function for comparison         | eql      |
| :from-end | if true, work backwards             | nil      |
| :start    | position at which to start          | 0        |
| :end      | position, if any, at which to stop  | nil      |
||#

;; new tool: position
;; `position` takes the full set of standard keyword parameters
(position #\a "fantasia")		  ;=> 1
(position #\a "fantasia" :start 3 :end 5) ;=> 4
(position #\a "fantasia" :from-end t)	  ;=> 7
(position 'a '((a b) (c d)))		  ;=> NIL
(position 'a '((a b) (c d)) :key #'car)	  ;=> 0
(position '(a b) '((a b) (c d)))	  ;=> NIL
(position '(a b) '((a b) (c d)) :test #'equal) ;=> 0
(position 3 '(1 0 7 5) :test #'<)	  ;=> 2

;; new tool: subseq[n:m)
(defun second-word (str)
  (let ((p1 (1+ (position #\  str))))
    (subseq str p1 (position #\  str :start p1))))
(second-word "From follows function.")	;=> "follows"

;; new tool: position-if
;; It takes all the keyword arguments *except* `:test`
(position-if #'oddp '(2 3 4 5))		;=> 1

;; new tool: find, find-if
;; `find` takes all the keryword arguments, `find-if` takes all except :test.

;; new tool: remove-duplicates
(remove-duplicates "abracadabra")	;=> "cdbra"

;;; Structures
;; new tool: defstruct
(defstruct point
  x
  y)
;; implicitly defines:
;; - make-point
;; - point-p
;; - copy-point
;; - point-x
;; - point-y
(setf p (make-point :x 0 :y 0))		;=> #S(POINT :X 0 :Y 0)
(point-x p)				;=> 0
(point-p p)				;=> T
(setf (point-y p) 2)			;=> 2
p					;=> #S(POINT :X 0 :Y 2)

;; defining a structre also defines a type of the name
;; point -> structure -> atom -> t
(typep p 'point)			;=> T

;; specifying defulat values for structure
(defstruct update
  (current-version (progn
		     (format t "Current Version: ")
		     (read)))
  (up-to-date-version nil))

(make-update)
;; > Current Version: "3.7.6"
;; => #S(UPDATE :CURRENT-VERSION "3.7.6" :UP-TO-DATE-VERSION NIL)

;; We can also control things like the way a structure is displayed,
;; and the prefix used in the names of the access functions is
;; created.

(defstruct (point (:conc-name p)
		  (:print-function print-point))
  (x 0) (y 0))
(defun print-point (p stream depth)
  (format stream "#<~A, ~A>"
	  ;; here are the difference that :conc-name made.
	  (px p) (py p)))

(make-point) 				;=> #<0, 0>

;;; Hash Tables

;; new tool: make-hash-table, gethash
(setf ht (make-hash-table))
(setf (gethash 'color ht) 'red)

;; new tool: remhash
(setf (gethash 'bug ht) t)
(remhash 'bug ht)

;; new tool: maphash
(setf (gethash 'size ht) 'big)
(maphash #'(lambda (k v)
	     (format t "~A = ~A~%" k v))
	 ht)
;; > SIZE = BIG
;; > COLOR = RED

;; Setting hashtable's size: (1) Because you know the hash table is
;; going to be huge, and you want to avoid expanding it. OR, (2)
;; because you know the hash table is going to be small, and you don't
;; want to waste memory.
(make-hash-table :size 5)

;; By default hash tables lookup the key by `eql`. You can change this
;; setting with :test argument.
(setf writers (make-hash-table :test #'equal))
(setf (gethash '(ralph waldo emerson) writers) t)

;;; Exercise 1
(defun quarter-turn (square)
  (if (not (squarep square)) nil
      (let ((n (car (array-dimensions square))))
	(let ((s (make-array (list n n))))
	  (dotimes (i n s)
	    (dotimes (j n)
	      (setf (aref s i j) (aref square (- n j 1) i))))))))

(defun squarep (x)
  (and (arrayp x)
       (let ((dim (array-dimensions x)))
	 (and (= (length dim) 2)
	      (= (first dim) (second dim))))))

;;; Exercise 2
(defun our-copy-list (x)
  (reverse (our-reverse x)))
(defun our-reverse (x)
  (reduce #'(lambda (acc item) (cons item acc))
	  x :initial-value nil))

;;; trinode impl. for Exercise 3
(defstruct (trinode (:print-function
		     (lambda (n s d)
		       (format s "#<~A>" (trinode-elt n)))))
  elt
  (first nil) (second nil) (third nil))

;; Exercise 3 (a)
(defun tree-copy (tree)
  (if (null tree) nil
      (make-trinode :elt (trinode-elt tree)
		    :first (tree-copy (trinode-first tree))
		    :second (tree-copy (trinode-second tree))
		    :third (tree-copy (trinode-third tree)))))

;; Exercise 3 (b)
(defun tree-member (obj tree)
  (if (null tree) nil
      (or (tree-member obj (trinode-first tree))
	  (tree-member obj (trinode-second tree))
	  (tree-member obj (trinode-third tree)))))

;;; BST impl. for Exercise 4, 5
(defstruct (node (:print-function
		 (lambda (n s d)
		     (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst) (make-node :elt obj)
      (if (eql obj (node-elt bst)) bst
	  (if (funcall < obj (node-elt bst))
	      (make-node
	       :elt (node-elt bst)
	       :l (bst-insert obj (node-l bst) <)
	       :r (node-r bst))
	      (make-node
	       :elt (node-elt bst)
	       :l (node-l bst)
	       :r (bst-insert obj (node-r bst) <))))))

(defun bst-find (obj bst <)
  (if (null bst) nil
      (if (eql obj (node-elt bst)) bst
	  (if (funcall < obj (node-elt bst))
	      (bst-find obj (node-l bst) <)
	      (bst-find obj (node-r bst) <)))))

(defun bst-remove (obj bst <)
  (if (null bst) nil
      (if (eql obj (node-elt bst)) (percolate bst)
	  (if (funcall < obj (node-elt bst))
	      (make-node
	       :elt (node-elt bst)
	       :l (bst-remove obj (node-l bst) <)
	       :r (node-r bst))
	      (make-node
	       :elt (node-elt bst)
	       :l (node-l bst)
	       :r (bst-remove obj (node-r bst) <))))))

(defun percolate (bst)
  (if (null bst) nil
      (cond ((null (node-l bst))
	     (if (null (node-r bst)) nil
		 (node-r bst)))
	    ((null (node-r bst)) (node-l bst))
	    (t
	     (if (zerop (random 2)) (lperc bst)
		 (rperc bst))))))
(defun lperc (bst)
  (let ((head (node-l bst)))
    (make-node
     :elt (node-elt head)
     :l (percolate head)
     :r (node-r bst))))
(defun rperc (bst)
  (let ((head (node-r bst)))
    (make-node
     :elt (node-elt head)
     :l (node-l bst)
     :r (percolate head))))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

(defun bst-min (bst)
  (if (null bst) nil
      (or (bst-min (node-l bst))
	  (node-elt bst))))

(defun bst-max (bst)
  (if (null bst) nil
      (or (bst-max (node-r bst))
	  (node-elt bst))))

(defun bst-copy (bst)
  (if (null bst) nil
      (make-node :elt (node-elt bst)
		 :l (bst-copy (node-l bst))
		 :r (bst-copy (node-r bst)))))

;;; Exercise 4
;; just for fun
(defun bst-traverse-inverse (fn bst)
  (when bst
    (bst-traverse-inverse fn (node-r bst))
    (funcall fn (node-elt bst))
    (bst-traverse-inverse fn (node-l bst))))
;; solution
(defun bst-reverse-list (bst)
  (let ((s NIL))
    (bst-traverse #'(lambda (item) (push item s)) bst)
    s))

;;; Exercise 5
;; bst-adjoin and bst-insert are same exactly.
;; See Errata(http://www.paulgraham.com/ancomliser.html)

;;; Exercise 6
(setf color '((R . 255) (G . 192) (B . 0)))

(defun assoc->hash (alist)
  (let ((ht (make-hash-table)))
    (dolist (pair alist ht)
      (let ((key (car pair)) (value (cdr pair)))
	(setf (gethash key ht) value)))))

(setf hcolor (assoc->hash color))
;;> #S(HASH-TABLE :TEST FASTHASH-EQL (B . 0) (G . 192) (R . 255))

(defun hash->assoc (hash)
  (let ((r nil))
    (maphash #'(lambda (key value)
		 (push (cons key value) r))
	     hash)
    r))

(hash->assoc hcolor)
;;> ((R . 255) (G . 192) (B . 0))
