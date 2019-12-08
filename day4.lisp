(defun main ()
  (solve 183564 657474))

(defun solve (low high)
  (do ((nr low (+ 1 nr))
       (count 0))
      ((eq nr  high) count)
    (when (check nr)
        (incf count 1))))

(defun check (number)
  (let ((minimum 183564)
        (maximum 657474)
        (str (write-to-string number)))
    (and
     (> number minimum)
     (< number maximum)
     (eq 6 (length str))
     (has-adjacent-values str)
     (increases-or-same str)
     )))

(defun increases-or-same (str)
  (cond
    ((not (> (length str) 1)) t)
    ((string= (elt str 0) (elt str 1))
     (increases-or-same (subseq str 1)))
    ((> (digit-char-p (elt str 1)) (digit-char-p (elt str 0)))
     (increases-or-same (subseq str 1)))
    (t nil)))

(defun has-adjacent-values (str)
  (cond
    ((not (> (length str) 1)) nil)
    ((string= (elt str 0) (elt str 1)) t)
    (t (has-adjacent-values (subseq str 1)))))

(defun test ()
  (and
   (check 444444)
   (check 445678)
   (not (check 345678))
   (not (check 777777)) ; above maximum
   (not (check 177777)) ; below minimum
   (not (check 444)) ;less than six chars
   (not (check 123456)) ; no adjacent values
   ))
