(defun main ()
  (let ((in (open "input.txt")))
    (print (solve in))
    (close in)))

(defun solve (in)
  (sum (lst in)))

(defun lst (in)
  (loop for line = (read-line in nil)
        while line
        collect (calculate-fuel (parse-integer line))))

(defun calculate-fuel (value)
  (let ((amount (- (FLOOR (/ value 3)) 2)))
    (cond
      ((minusp amount) 0)
      ((eq value 3) 3)
      (t (+ amount (calculate-fuel amount))))))

(defun sum (lst)
  (reduce '+ lst))

