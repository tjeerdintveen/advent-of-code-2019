(defun main ()
  (first
   (sort (mapcar #'start-compute-array (all-permutations ' (0 1 2 3 4)))
         #'>)))

(defun start-compute-array (configurations)
  (compute-array configurations 0))

(defun compute-array (configurations input)
  ;; (format t "Input is ~a~%" input)
  (if (car configurations)
      (let* ((in (open "input.txt"))
             (contents (read in))
             (instructions (make-array (length contents) :initial-contents contents))
             (output (compute instructions 0 (list (car configurations) input) nil)))
        (close in)
        ;; (format t "OUTPUT for ~a is ~a~%" (car configurations) output)
        (compute-array (cdr configurations) output))
      input))

(defun compute (instructions pos input output)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt instructions pos))
    ;; (format t "Opcode ~a position ~a~%" opcode pos)
    (case opcode
      (1
       (set-value instructions
                  (+ (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute instructions (+ 4 pos) input output))
      (2
       (set-value instructions
                  (* (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute instructions (+ 4 pos) input output))
      (3
       (set-value instructions (car input) (+ pos 1))
       (compute instructions (+ 2 pos) (cdr input) output))
      (4
       ;; (format t "OUTPUT: ~a~%" (get-value 1))
       (compute instructions (+ 2 pos) input (get-value 1)))
      (5
       (if (not (eq 0 (get-value 1)))
           (compute instructions (get-value 2) input output)
           (compute instructions (+ 3 pos) input output)))
      (6
       (if (eq 0 (get-value 1))
           (compute instructions (get-value 2) input output)
           (compute instructions (+ 3 pos) input output)))
      (7
       (if (< (get-value 1)
              (get-value 2))
           (progn (set-value instructions 1 (+ 3 pos))
                  (compute instructions (+ 4 pos) input output))
           (progn (set-value instructions 0 (+ 3 pos))
                  (compute instructions (+ 4 pos) input output))))
      (8
       (if (eql (get-value 1)
                (get-value 2))
           (progn
             (set-value instructions 1 (+ 3 pos))
             (compute instructions (+ 4 pos) input output))
           (progn
             (set-value instructions 0 (+ 3 pos))
             (compute instructions (+ 4 pos) input output))))
      (99 ;; halt
       ;; (format t "I'm finished~%")
       output)
      (t ;; Unknown opcode
       (format t "Error, opcode ~a received~%" opcode)
       ))))

(defmacro get-value (pos)
  (if (eql 1 pos)
      `(real-value instructions (+ ,pos pos) first-mode)
      `(real-value instructions (+ ,pos pos) second-mode)))

(defun real-value (instructions pos mode)
  (if (eq mode 0)
      (eltderef instructions pos) ;; Pos mode, get value at pos
      (elt instructions pos)));; Immediate mode, get value directly

;; Pos is always resolved to pos mode
(defun set-value (instructions value pos)
  (let ((actual-pos (elt instructions pos))) ;; A value that's stored is always in pos mode (at least never in immediate mode)
    (setf (aref instructions actual-pos) value))
  instructions
  )

(defun extract-instruction (value)
  "Don't look at me, I'm hideous"
  (multiple-value-bind (rest opcode) (floor value 100)
    (multiple-value-bind (rest first-mode) (floor rest 10)
      (multiple-value-bind (_ second-mode) (floor rest 10)
        (declare (ignore _))
        (values opcode first-mode second-mode)))))

(defun eltderef (sequence index)
  "Find the element in the array stored in the index"
  (elt sequence (elt sequence index)))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
                 append (mapcar (lambda (l) (cons element l))
                                (all-permutations (remove element list)))))))
