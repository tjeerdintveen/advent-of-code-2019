(defvar *graph* nil)
(defvar *visited* nil)

(defun main ()
  (setf *graph* (make-hash-table :test 'equal))
  (setf *visited* (make-hash-table :test 'equal))

  (with-open-file (stream "input.txt" :if-does-not-exist nil)
    (when stream
      (loop for line = (read-line stream nil)
            while line do (multiple-value-bind (lhs rhs) (split-string line)
                            (setf (gethash rhs *graph*) lhs))))
    (solve)))

(defun solve ()
  (traverse "YOU" -1 99999)
  (traverse "SAN" -1 99999))

(defun traverse (key count min-step-count)
  (let* ((value (gethash key *graph*))
         (visited-count (gethash key *visited*)))
    (cond
      ((not value) min-step-count) ;; done traversing, return lowest step count
      ((not visited-count)
       (setf (gethash key *visited*) count)
       (traverse value (+ 1 count) min-step-count))
      (t (let ((new-count (+ visited-count count)))
           (traverse value (+ 1 count) (min min-step-count new-count)))))))

(defun split-string (string)
  (let ((pos (position #\) string)))
    (values (subseq string 0 pos) (subseq string (+ 1  pos)))))
