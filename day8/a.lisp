(defparameter *columns* 25)
(defparameter *rows* 6)

(defun main ()
  (with-open-file (stream "input.txt")
    (let ((input (read stream)))
      (solve input))))

(defun solve (input)
  (let* ((zero-counts (make-hash-table :test 'equal))
         (one-counts (make-hash-table :test 'equal))
         (two-counts (make-hash-table :test 'equal))
         (string-input (write-to-string input))
         (len (length string-input)))

    (do* ((i 0 (+ 1 i))
          (layer 0 (floor (/ i (* *rows* *columns*)))))
         ((>= i len))

      (if (char= #\0 (elt string-input i))
          (incf (gethash layer zero-counts 0)))

      (if (char= #\1 (elt string-input i))
          (incf (gethash layer one-counts 0)))

      (if (char= #\2 (elt string-input i))
          (incf (gethash layer two-counts 0))))

    (multiple-value-bind (key value)
        (hash-min #'(lambda (key value) value) zero-counts)

      (* (gethash key one-counts) (gethash key two-counts)))))

(defun hash-min (fn table)
  "Get minimum key value depending on what lambda (fn) returns"
  (let ((lowest nil)
        (return-key nil)
        (return-value nil))
    (maphash #'(lambda (key value)
                 (let ((result (funcall fn key value)))
                   (when (or (null lowest) (< result lowest))
                       (setf lowest result)
                       (setf return-key key)
                       (setf return-value value))))
             table)
    (values return-key return-value)))

(defun layers (partitions)
  (partition partitions 2))

(defun partition (seq size)
"Partition sequence into chunks"
  (reverse
   (let ((seq-length (length seq)))
     (do ((start 0 (+ start size))
          (end size (+ end size))
          (partitions '() (push (subseq seq start end) partitions)))
         ((> end seq-length) partitions)))))
       ;; (format t "Start ~a end ~a~%" start end)))))
