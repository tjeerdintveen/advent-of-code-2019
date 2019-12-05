(defun main  ()
 (let ((in (open "input.txt")))
   (let ((temp (read in)))
     (print (elt (solve temp) 0)))
   (close in)))

(defun solve (list)
  (let ((arr (make-array (length list) :initial-contents list)))
    (process arr)))

(defun process (arr)
  (process-with-index arr 0))

(defun process-with-index (arr index)
 (cond
   ((eq (elt arr (+ index 0)) 1)
    (update arr index #'+)
    (process-with-index arr (+ index 4)))
   ((eq (elt arr (+ index 0)) 2)
    (update arr index #'*)
    (process-with-index arr (+ index 4)))
   ((eq (elt arr (+ index 0)) 99)
    arr)
   (t nil)))

;; Applies a function to the second and third position of an arr, and stores it in the third position
(defun update (arr index fn)
  (let ((first (elt-reference (+ index 1) arr))
        (second (elt-reference (+ index 2) arr)))
    (setf (aref arr (elt arr (+ index 3))) (funcall fn first second)) ; Update arr first index
    arr))

;; Find the element in the array stored in the index
(defun elt-reference (index arr)
 (elt arr (elt arr index)))

(defun test ()
 (and
  (equalp #(2 0 0 0 99) (solve '(1 0 0 0 99)))
  (equalp #(2 3 0 6 99) (solve '(2 3 0 3 99)))
  (equalp #(2 4 4 5 99 9801) (solve '(2 4 4 5 99 0)))
  (equalp #(30 1 1 4 2 5 6 0 99) (solve '(1 1 1 4 99 5 6 0 99)))
  (equalp #(3500 9 10 70 2 3 11 0 99 30 40 50) (solve '(1 9 10 3 2 3 11 0 99 30 40 50)))
  ))
