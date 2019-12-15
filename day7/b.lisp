(defvar *file-stream* nil)
(defun main ()
  (with-open-file (stream "input.txt")
    (setf *file-stream* (read stream)))
  (first
   (sort (mapcar #'start-compute-array (all-permutations '(9 8 7 6 5)))
         #'>)))

(defun start-compute-array (phases)
  (calculate (mapcar #'make-amp phases) 0 0))

(defun calculate (amps index input)
  (declare (type integer index input))
  (let* ((current-amp (elt amps index))
         (output (funcall current-amp input)))

    (if (eq 99 output)
        input
        (calculate amps
                   (if (eql index (- (length amps) 1))
                       0
                       (+ 1 index)) output))))

(defun make-amp (phase)
  (let* ((input (copy-list *file-stream*))
         (instructions (make-array (length input) :initial-contents input))
         (has-phase t)
         (position 0))

    (lambda (in)
      (let ((input-list (if has-phase
                            (list phase in)
                            (list in))))
        (setf has-phase nil)

        (multiple-value-bind (output last-position)
            (compute instructions position input-list)

          (setf position last-position)
          output)))))

(defun compute (instructions pos input)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt instructions pos))
    (case opcode
      (1
       (set-value instructions
                  (+ (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute instructions (+ 4 pos) input))
      (2
       (set-value instructions
                  (* (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute instructions (+ 4 pos) input))
      (3
       (assert (car input) nil "Expected input, but there's no input to be found at position: ~a length of program ~a" pos (length instructions))
       (set-value instructions (car input) (+ pos 1))
       (compute instructions (+ 2 pos) (cdr input)))
      (4
       (values (get-value 1) (+ 2 pos)))
      (5
       (if (not (eq 0 (get-value 1)))
           (compute instructions (get-value 2) input)
           (compute instructions (+ 3 pos) input)))
      (6
       (if (eq 0 (get-value 1))
           (compute instructions (get-value 2) input)
           (compute instructions (+ 3 pos) input)))
      (7
       (if (< (get-value 1)
              (get-value 2))
           (progn (set-value instructions 1 (+ 3 pos))
                  (compute instructions (+ 4 pos) input))
           (progn (set-value instructions 0 (+ 3 pos))
                  (compute instructions (+ 4 pos) input))))
      (8
       (if (eql (get-value 1)
                (get-value 2))
           (progn
             (set-value instructions 1 (+ 3 pos))
             (compute instructions (+ 4 pos) input))
           (progn
             (set-value instructions 0 (+ 3 pos))
             (compute instructions (+ 4 pos) input))))
      (99 ;; halt
       99)
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
  instructions)

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
