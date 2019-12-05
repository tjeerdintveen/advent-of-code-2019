(defun main  ()
 (let ((in (open "input.txt")))
   (solve (read in))
   (close in)))

(defun solve (list)
  (do ((i 0 (+ i 1)))
      ((or (> i 99)))
    (do ((j 0 (+ j 1))
         (arr  (list->arr list) (list->arr list)))
        ((or (> j 99)))
      (setf (aref arr 1) i)
      (setf (aref arr 2) j)
      (when (process arr)
          (print "found amount at")
          (print i)
          (print j)
          (print "Solution is:")
          (print (+ (* 100 i) j))))))

(defun list->arr (list)
  (make-array (length list) :initial-contents list))

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
    (if (eq (aref arr 0) 19690720)
        t
        nil))
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

