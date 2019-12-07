; 266 is right answer. Brute forced it though

(defun main ()
  (let ((in (open "input.txt")))
    (print (solve (read in) (read in)))
    (close in)))

(defun solve (lhs rhs)
  (let ((table (make-hash-table :test 'equal))
        (results '())
        )
    (wire->points lhs table)
    (wire->points rhs table)

    (maphash #'(lambda (key value)
                 (when (eq 2 value)
                   (setf results (cons (distance '(0 0) key) results)))
                 ) table)

    (first (sort results #'<))))

(defun distance (lhs rhs)
  (+
   (abs (- (first lhs) (first rhs)))
   (abs (- (second lhs) (second rhs)))))

(defun split-symbol (dirsym)
  "Split the directionsymbol e.g. R2 into a string and an amount."
  (let ((char (elt (string dirsym) 0))
        (amount (parse-integer (subseq (string dirsym) 1))))
    (values char amount)))

(defun wire->points (wire table)
    (do* ((list wire (cdr list))
          (element (car list) (car list))
          (x 0)
          (y 0))
         ((not list))
      (multiple-value-bind (direction amount) (split-symbol element)
        (dotimes (i amount)
          (cond
            ((string= "R" direction)
             (setf x (+ x 1)))
            ((string= "L" direction)
             (setf x (- x 1)))
            ((string= "U" direction)
             (setf y (- y 1)))
            ((string= "D" direction)
             (setf y (+ y 1)))
            (t nil))

          (incf (gethash (list x y) table 0) 1)))))

;; (defun test ()
;;   (and
;;    (equalp '(13 0) (path->coordinate 'R10 '(3 0)))
;;    (equalp '(11 0) (path->coordinate 'R8 '(3 0)))
;;    (equalp '(-5 0) (path->coordinate 'L8 '(3 0)))
;;    (equalp '(3 -6) (path->coordinate 'U8 '(3 2)))
;;    (equalp '(3 10) (path->coordinate 'D8 '(3 2)))

;;    (equalp 6 (solve '(R8 U5 L5 D3) '(U7 R6 D4 L4)))
;;    (equalp 6 (solve '(R8 U5 L5 D3) '(U7 R6 D4 L4)))
;;    (equal 159 (solve '(R75 D30 R83 U83 L12 D49 R71 U7 L72) '(U62 R66 U55 R34 D71 R55 D58 R83)))
;;    ))

