;;;; Utilities for operations on sorted vectors.

;;; Finds an element in a sorted vector

(defun bin-search (obj vec)
  (let ((len (length vec)))
    ;; if a real vector, send it to finder
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start)) obj nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let ((obj2 (aref vec mid)))
	    (if (< obj obj2)
		(finder obj vec start (- mid 1))
		(if (> obj obj2)
		    (finder obj vec (+ mid 1) end)
		    obj)))))))
