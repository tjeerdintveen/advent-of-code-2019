(defun main ()
  (let ((in (open "input.txt")))
    (print (solve (read in) (read in)))
    (close in)))

(defun solve (lhs rhs)
  (let ((results '())
        (table-lhs (wire->points lhs))
        (table-rhs (wire->points rhs)))

    (maphash #'(lambda (coordinate steps-lhs)
                 (let ((steps-rhs (gethash coordinate table-rhs)))
                   (when steps-rhs
                     (setf results (cons (+ steps-lhs steps-rhs) results))))
                 )
             table-lhs)

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

(defun wire->points (wire)
  (let ((table (make-hash-table :test 'equal)))
    (do* ((list wire (cdr list))
          (element (car list) (car list))
          (total-steps 0)
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

          (setf total-steps (+ total-steps 1))
          (incf (gethash (list x y) table 0) total-steps))))

    table))

(defun test ()

  (and
   (equalp 30
           (solve 
            '(R8 U5 L5 D3)
            '(U7 R6 D4 L4))
           )
   (equalp 610
           (solve
            '(R75 D30 R83 U83 L12 D49 R71 U7 L72)
            '(U62 R66 U55 R34 D71 R55 D58 R83)))
   ))
