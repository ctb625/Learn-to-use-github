;;;;OJ题目的解答。题目为http://www.patest.cn/contests/pat-b-practise/1031 查验身份证。
;;check-id-number函数还可以增加一个输入位数验证。有机会的吧。


;;这个函数可以把一个字符串拆分成一个列表。
(defun listlize (string)
  (let ((res nil)(temp (reverse string)))    
    (dotimes (p (length string) res)
      (push (subseq temp p (+ p 1)) res))))

;;拆分出来的列表的每一项都是只有一个字符的字符串。
;;所以总是和下面的函数连用，将每一项的单个字符串转换成数字和符号。  
(defun unstring (string-list)
  (mapcar #'read-from-string string-list))

;;身份证的验证方法是将每一位乘以某个系数然后相加起来然后除以11得的余数。
(defun get-id-z (number-list)
  (mod (apply #'+ (map 'list #'*  '(7 9 10 5 8 4 2 1 6 3 7 9 10 5 8 4 2) number-list)) 11))

;;相应的余数会对应相应的数字（或X）
(defun get-id-m (id-z)
  (elt '(1 0 X 9 8 7 6 5 4 3 2) id-z))

;;检测一个数字是否符合身份证的格式。（只算验证码）
;;如果符合格式没问题，则返回nil，否则返回传入的参数id-number-string。
(defun check-id-number (id-number-string)
  (let ((num (subseq id-number-string 0 (- (length id-number-string) 1)))   ;从开头取到倒数第二个。
	(check (subseq id-number-string (- (length id-number-string) 1))))  ;最后一位。
    (if (every #'numberp (unstring (listlize num)))                         ;检测前面的是否全是数字。如果全是数字进行下一步，否则跳出。
	(if (eql (get-id-m (get-id-z (unstring (listlize num)))) (read-from-string check))  ;对计算出来的验证码和数字里的验证码进行比对。
	    nil                                                             ;一致则说明没问题。
	    id-number-string)                                               ;不一致说明号码有问题。
	id-number-string)))                                                 ;如果前17位不为全数字也是出错的。


;;;针对OJ系统的输入输出。
(let ((count 0))
  (dotimes (i (read))
    (let (temp)
      (if (setf temp (check-id-number (read-line)))
	  (progn 
	    (format t "~a~%" temp)
	    (incf count)))))
  (if (eql count 0)
      (format t "All passed")))