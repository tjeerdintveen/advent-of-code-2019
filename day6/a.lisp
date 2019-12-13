(defvar *graph* (make-hash-table :test 'equal))
(defvar *orbit-count* 0)

(defun main ()
  (setf *orbit-count* 0)
  (let ((in (open "input.txt")))
    (when in
      (loop for line = (read-line in nil)
            while line do (multiple-value-bind (lhs rhs) (split-string line)
                            (setf (gethash rhs *graph*) lhs)))
      (close in))
    (solve)))

(defun solve ()
  (maphash #'(lambda (key value)
           (traverse key))
           *graph*)

  (format t "Orbit count ~a" *orbit-count*))

(defun traverse (key)
  (let ((value (gethash key *graph*)))
        (when value
          (incf *orbit-count* 1)
          (traverse value))))

(defun split-string (string)
  (let ((pos (position #\) string)))
    (values (subseq string 0 pos) (subseq string (+ 1  pos)))))
