(defun gugu()
  (dotimes (n 3)
    (let ((m1 (+ n 1))
	  (m2 (+ n 4))
	  (m3 (+ n 7)))
      (dotimes (i 9)
	(let ((x (+ i 1)))
	  (format t "~Dx~D=~2D . " m1 x (* m1 x))
	  (format t "~Dx~D=~2D . " m2 x (* m2 x))
	  (format t "~Dx~D=~2D~%" m3 x (* m3 x))))
      (format t "------------------------~%"))))
(defun quiz (n m)
  (format t "~Ax~A=" n m)
  (format t
	  (if (= (read) (* n m)) "correct.~%"
	      "wrong.~%")))
;; Exercise 7
(defun contains-listp (ll)
  (let ((s nil))
    (dolist (x ll s)
      (setf s (or s (listp x))))))
;; Exercise 8's (a)
(defun pokedot-do (n)
  (do ((i 1 (+ i 1)))
      ((> i n))
    (format t "."))
  (format t "~%"))
(defun pokedot-dotimes (n)
  (dotimes (i n)
    (format t "."))
  (format t "~%"))
(defun pokedot-rec (n)
  (if (= n 0) (format t "~%")
      (progn
	(format t ".")
	(pokedot-rec (- n 1)))))
;; Exercise 8's (b)
(defun count-a-dolist (ll)
  (let ((r 0))
    (dolist (x ll r)
      (if (eq x 'a) (incf r 1) nil))))
(defun count-a-do (ll)
  (let ((r 0)
	(l ll))
    (do ((a (car l) (setf l (cdr l)
			  a (car l))))
	((null l) r)
      (if (eq a 'a) (incf r 1) nil))))
(defun count-a-rec (ll)
  (if (null ll) 0
      (+ (if (eq (car ll) 'a) 1 0)
	 (count-a-rec (cdr ll)))))
