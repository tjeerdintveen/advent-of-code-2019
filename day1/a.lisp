(defun main ()
  (let ((in (open "input.txt")))
    (print (solve in))
    (close in)))

(defun solve (in)
   (sum (lst in)))

(defun lst (in)
  (loop for line = (read-line in nil)
        while line
        collect (calculate-fuel line)))

(defun calculate-fuel (value)
  (let ((mass (parse-integer value)))
    (- (FLOOR (/ mass 3)) 2)))

(defun sum (lst)
  (reduce '+ lst))
