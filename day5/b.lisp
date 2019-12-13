(defvar *instructions* nil)
(defun main  ()
  (let ((in (open "day5/input.txt")))
    (solve (read in))
    (close in)))

(defun solve (list)
  (setf *instructions* (make-array (length list) :initial-contents list))
  (compute 0 5)) ;; passing input 5

(defun compute (position &optional input)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt *instructions* position))
  (case opcode
    (1
     ;; TODO Remove new-instructions, just call setf directly
     (let ((new-instructions (set-value (+
                                         (value-at 1)
                                         (value-at 2))
                                        (+ 3 position))))
       ;; TODO not needed
       (setf *instructions* new-instructions)
       (compute (+ 4 position))))
    (2
     (let ((new-instructions (set-value (*
                                         (value-at 1)
                                         (value-at 2))
                                        (+ 3 position))))

       (setf *instructions* new-instructions)
       (compute (+ 4 position))))
    (3
     (set-value input (+ position 1))
     (compute (+ 2 position)))
    (4
     (format t "Output is: ~a~%" (value-at 1))
     (compute (+ 2 position)))
    (5
     (if (not (eq 0 (value-at 1)))
         (compute
                  (value-at 2))
         (compute (+ 3 position))))
    (6
     (if (eq 0 (value-at 1))
           (compute
                    (value-at 2))
           (compute (+ 3 position))))
    (7
     ;; (print "opcode 7")
     (if (< (value-at 1)
            (value-at 2))
         (progn (set-value 1 (+ 3 position))
                (compute (+ 4 position)))
         (progn (set-value 0 (+ 3 position))
                (compute (+ 4 position)))))
    (8
     (if (eql (value-at 1)
            (value-at 2))
         (progn
           (set-value 1 (+ 3 position))
           (compute (+ 4 position)))
         (progn
           (set-value 0 (+ 3 position))
           (compute (+ 4 position)))))

    ((eq opcode 99) ;; Halt
     (format t "I'm finished"))
    (t
     (format t "Error, opcode ~a received~%" opcode)
     ))))

(defmacro value-at (pos)
  (if (eql 1 pos)
      `(get-value (+ ,pos position) first-mode)
      `(get-value (+ ,pos position) second-mode)))

(defun get-value (position mode)
  (print "a")
  (if (eq mode 0)
      (elt-reference position *instructions*) ;; Position mode, get value at position
      (elt *instructions* position)));; Immediate mode, get value directly

;; Position is always resolved to position mode
(defun set-value (value position)
  (let ((actual-position (elt *instructions* position))) ;; A value that's stored is always in position mode (at least never in immediate mode)
    (setf (aref *instructions* actual-position) value))
  *instructions*
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
