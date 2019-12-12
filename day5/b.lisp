(defun main  ()
  (let ((in (open "day5/input.txt")))
    (solve (read in))
    (close in)))

(defun solve (list)
  (let ((instructions (make-array (length list) :initial-contents list)))
    (compute instructions 0 5))) ;; passing input 5

(defun compute (instructions position &optional input)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt instructions position))
  (cond
    ((eq opcode 1) ;; Addition
     (let ((new-instructions (set-value (+
                                         (value-at 1)
                                         (value-at 2))
                                        instructions
                                        (+ 3 position))))
       (compute new-instructions (+ 4 position))))
    ((eq opcode 2) ;; Multiply
     (let ((new-instructions (set-value (*
                                         (value-at 1)
                                         (value-at 2))
                                        instructions
                                        (+ 3 position))))
       (compute new-instructions (+ 4 position))))
    ((eq opcode 3) ;; Store input
     (compute (set-value input
                         instructions
                         (+ position 1)) (+ 2 position)))
    ((eq opcode 4)
     (format t "Output is: ~a~%" (value-at 1))
     (compute instructions (+ 2 position)))
    ((eq opcode 5)
     (if (not (eq 0 (value-at 1)))
         (compute instructions
                  (value-at 2))
         (compute instructions (+ 3 position))))
    ((eq opcode 6)
     (if (eq 0 (value-at 1))
           (compute instructions
                    (value-at 2))
           (compute instructions (+ 3 position))))
    ((eq opcode 7)
     ;; (print "opcode 7")
     (if (< (value-at 1)
            (value-at 2))
         (progn (set-value 1 instructions (+ 3 position))
                (compute instructions (+ 4 position)))
         (progn (set-value 0 instructions (+ 3 position))
                (compute instructions (+ 4 position)))))
    ((eq opcode 8)
     (if (eql (value-at 1)
            (value-at 2))
         (progn
           (set-value 1 instructions (+ 3 position))
           (compute instructions (+ 4 position)))
         (progn
           (set-value 0 instructions (+ 3 position))
           (compute instructions (+ 4 position)))))

    ((eq opcode 99) ;; Halt
     (format t "I'm finished"))
    (t
     (format t "Error, opcode ~a received~%" opcode)
     ))))

(defmacro value-at (pos)
  (if (eql 1 pos)
      `(get-value instructions (+ ,pos position) first-mode)
      `(get-value instructions (+ ,pos position) second-mode)))

(defun get-value (instructions position mode)
  (if (eq mode 0)
      (elt-reference position instructions) ;; Position mode, get value at position
      (elt instructions position)));; Immediate mode, get value directly

;; Position is always resolved to position mode
(defun set-value (value instructions position)
  (let ((actual-position (elt instructions position))) ;; A value that's stored is always in position mode (at least never in immediate mode)
    ;; (format t "Storing value ~a to ~a ~a~%" value actual-position position)
    (setf (aref instructions actual-position) value))
  instructions
  )

(defun extract-instruction (value)
  "Don't look at me, I'm hideous"
  (multiple-value-bind (rest opcode) (floor value 100)
    (multiple-value-bind (rest first-mode) (floor rest 10)
      (multiple-value-bind (_ second-mode) (floor rest 10)
        (declare (ignore _))
        (values opcode first-mode second-mode)))))

(defun elt-reference (index arr)
  "Find the element in the array stored in the index"
  (elt arr (elt arr index)))
