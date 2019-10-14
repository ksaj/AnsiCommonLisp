(setf min '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;; queue contains not nodes, but paths
(defun bfs (end queue net)
  (if (null queue) nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n) (cons n path))
	  (cdr (assoc node net))))

(shortest-path 'a 'd min)
