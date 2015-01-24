(defun string-split-by (string char)
	   (let* ((p1 0)
		  (p2 (position char string :start (+ p1 1)))
		  (lst nil))
	     (progn
	       (loop while p2 do
		    (progn
		      (push (subseq string p1 p2) lst)
		      (setf p1 (+ 1 p2))
		      (setf p2 (position char string :start (+ 1 p1)))))
	       (push (subseq string p1) lst)
	       (reverse lst))))

(defun output (string-list)
	   (if (null (cdr string-list))
	       (format t "~a" (car string-list))
	       (progn
		 (format t "~a " (car string-list))
		 (output (cdr string-list)))))

(output (reverse (string-split-by (read-line) #\ )))