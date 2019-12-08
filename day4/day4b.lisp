(defun main ()
  (solve 183564 657474))

(defun solve (low high)
  (do ((nr low (+ 1 nr))
       (count 0))
      ((eq nr  high) count)
    (when (check nr)
        (incf count 1))))

(defun check (number)
  (let* ((str (write-to-string number))
         (sorted-str (sort (copy-seq str) #'(lambda (x y)
                                              (< (digit-char-p x) (digit-char-p y))))))
    (and
     (string= str sorted-str)
     (has-adjacent-values sorted-str))))

(defun has-adjacent-values (str)
  (if (< (length str) 2)
      nil
      (eq 1 (check-adjacent (subseq str 1) (elt str 0) 0))))

(defun count-adjacent (str last-char count)
    (cond
      ((eq 0 (length str)) count)
      ((string= last-char (elt str 0))
       (count-adjacent (subseq str 1) (elt str 0) (+ 1 count)))
      (t
       (if (eq 1 count)
           count
           (count-adjacent (subseq str 1) (elt str 0) 0)))
       ))

(defun test ()
  (and
   (not (check 123444))
   (not (check 444444))
   (check 445678)
   (not (check 345678))
   (not (check 777777)) ; above maximum
   (not (check 177777)) ; below minimum
   (not (check 123456)) ; no adjacent values
   (has-adjacent-values "112233")
   (has-adjacent-values "111122")
   (has-adjacent-values "444455")
   (not (has-adjacent-values "123444")) ; failing
   ))

