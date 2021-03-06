;;;;//this is the practice for the Programming Ability Test
;;;;//for this question

;;;;//Given a sequence of K integers { N1, N2, ..., NK }. 
;;;;//A continuous subsequence is defined to be { Ni, Ni+1, ..., Nj } where 1 <= i <= j <= K. 
;;;;//The Maximum Subsequence is the continuous subsequence which has the largest sum of its elements. 
;;;;//For example, given sequence { -2, 11, -4, 13, -5, -2 }, 
;;;;//its maximum subsequence is { 11, -4, 13 } with the largest sum being 20.

;;;;//Each case occupies two lines. 
;;;;//The first line contains a positive integer K (<= 10000). 
;;;;//The second line contains K numbers, separated by a space.

(defun main () 
	   (let ((len (read))
		 (array (read-line)))
	     (format t "~a" (max-subseq-sum (mapcar #'parse-integer (string-split-by array #\  )) len ))))

(defun max-subseq-sum (lst n)
	   (let ((this-sum 0)
		 (max-sum 0))
	     (dotimes (i n  max-sum)
	       (progn
		 (setf this-sum (+ this-sum (nth i lst)))
		 (if (> this-sum max-sum)
		     (setf max-sum this-sum))
		 (if (< this-sum 0)
		     (setf this-sum 0))))))

;;(defun string-split-by (string char)
;;	    (let ((p (position char string)))
;;	      (if p
;;		  (cons (subseq string 0 p)
;;			(string-split-by (subseq string (+ p 1)) char))
;;		  (cons string nil))))

(defun string-split-by (string char)
	   (let* ((p1 (position char string :from-end t))
		  (p2 (position char string :from-end t :end (- p1 1)))
		  (lst nil))
	     (progn
	       (push (subseq string (1+ p1)) lst)
	       (loop while p2 do
		    (progn
		      (push (subseq string (1+ p2) p1) lst)
		      (setf p1 p2)
		      (setf p2 (position char string :from-end t :end p1))))
	       (push (subseq string 0 p1) lst)
	       lst)))

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


(defun main ()
	   (let ((this-sum 0)(max-sum 0)(n (read)))
	     (dotimes (i n max-sum)
	       (progn
		 (setf this-sum (+ this-sum (read)))
		 (if (> this-sum max-sum)
		     (setf max-sum this-sum))
		 (if (< this-sum 0)
		     (setf this-sum 0)))))
(format t "~a" (main))