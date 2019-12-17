(defparameter *columns* 25)
(defparameter *rows* 6)

(defun main ()
  (with-open-file (stream "input.txt")
    (let ((input (read stream)))
      (solve input))))

(defun solve (input)
  (let ((result (mapcar #'resolve-pixel
                              (cross-section
                               (partition (listize input) (* *columns* *rows*))))))

    (draw result)))

(defun draw (strings)
  (format t "~a~%" (subseq strings 0 25))
  (format t "~a~%" (subseq strings 25 50))
  (format t "~a~%" (subseq strings 50 75))
  (format t "~a~%" (subseq strings 75 100))
  (format t "~a~%" (subseq strings 100 125))
  (format t "~a~%" (subseq strings 125)))

(defun resolve-pixel (list)
  ;; 0 is black, 1 is white, and 2 is transparent.
  (case (car list)
    (0 " ")
    (1 "X")
    (2 (resolve-pixel (cdr list)))
    (t " ")))

(defun cross-section (lists)
  "Get overlapping elements from nested lists, e.g. combine all car elements into a list, repeat"
  (if (null (caar lists))
      '()
      (cons (multi-car lists) (overlap (multi-cdr lists)))))

(defun multi-cdr (lists)
  "get cdr for all nested lists"
  (reduce #'(lambda (acc list)
              (append acc (list (cdr list))))
          lists
          :initial-value '()))
  
(defun multi-car (lists)
  "get car for all nested lists"
  (reduce #'(lambda (acc list)
              (append acc (list (car list))))
              lists
              :initial-value '()))

(defun listize (nr)
  "number to list, e.g. 1234 becomes (1 2 3 4), possible bug if nr ends with 0"
  (labels ((rec (nr)
             (if (eq nr 0)
                 '()
                 (multiple-value-bind (rest digit) (floor nr 10)
                   (cons digit (rec rest))))))
    (reverse (rec nr))))

(defun partition (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
