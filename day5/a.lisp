(defun main  ()
  (let ((in (open "day5/input.txt")))
    (solve (read in))
    (close in)))

(defun solve (list)
  (let ((instructions (make-array (length list) :initial-contents list)))

    (compute instructions 0 1))) ;; passing input 1

(defun compute (instructions position &optional input)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt instructions position))
  (cond
    ((eq opcode 1) ;; Addition
     (let ((new-instructions (set-value (+
                                         (get-value instructions (+ 1 position) first-mode)
                                         (get-value instructions (+ 2 position) second-mode))
                                        instructions
                                        (+ 3 position))))
       (compute new-instructions (+ 4 position))))
    ((eq opcode 2) ;; Multiply
     (let ((new-instructions (set-value (*
                                         (get-value instructions (+ 1 position) first-mode)
                                         (get-value instructions (+ 2 position) second-mode))
                                        instructions
                                        (+ 3 position))))
       (compute new-instructions (+ 4 position))))
    ((eq opcode 3) ;; Store input
     (compute (set-value input
                         instructions
                         (+ position 1)) (+ 2 position)))
    ((eq opcode 4)
     (print "Outputting:")
     (print (get-value instructions (+ 1 position) first-mode))
     (compute instructions (+ 2 position)))
    ((eq opcode 99) ;; Halt
     (print "I'm finished")
     ))))


(defun get-value (instructions position mode)
  (if (eq mode 0)
      (elt-reference position instructions) ;; Position mode, get value at position
      (elt instructions position)));; Immediate mode, get value directly

(defun set-value (value instructions position)
  (let ((actual-position (elt instructions position))) ;; A value that's stored is always in position mode (at least never in immediate mode)
    (setf (aref instructions actual-position) value))
  instructions
  )

(defun extract-instruction (value)
  "Don't look at me, I'm hideous"
  (multiple-value-bind (rest opcode) (floor value 10)
    (multiple-value-bind (rest _) (floor rest 10)
      (declare (ignore _))
      ;;We skip one digit (e.g. from opcode 02 we only need the tenth, so 2)
      (multiple-value-bind (rest first-mode) (floor rest 10)
        (multiple-value-bind (_ second-mode) (floor rest 10)
          (declare (ignore _))
          (values opcode first-mode second-mode))))))

(defun elt-reference (index arr)
  "Find the element in the array stored in the index"
  (elt arr (elt arr index)))
