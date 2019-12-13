(defvar *instructions* nil)
(defun main  ()
  (let ((in (open "input.txt")))
    (solve (read in))
    (close in)))

(defun solve (list)
  (setf *instructions* (make-array (length list) :initial-contents list))
  (compute 0 5)) ;; passing input 5

(defun compute (pos &optional input)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt *instructions* pos))
    (case opcode
      (1
       (set-value (+ (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute (+ 4 pos)))
      (2
       (set-value (* (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute (+ 4 pos)))
      (3
       (set-value input (+ pos 1))
       (compute (+ 2 pos)))
      (4
       (format t "Output is: ~a~%" (get-value 1))
       (compute (+ 2 pos)))
      (5
       (if (not (eq 0 (get-value 1)))
           (compute (get-value 2))
           (compute (+ 3 pos))))
      (6
       (if (eq 0 (get-value 1))
           (compute (get-value 2))
           (compute (+ 3 pos))))
      (7
       (if (< (get-value 1)
              (get-value 2))
           (progn (set-value 1 (+ 3 pos))
                  (compute (+ 4 pos)))
           (progn (set-value 0 (+ 3 pos))
                  (compute (+ 4 pos)))))
      (8
       (if (eql (get-value 1)
                (get-value 2))
           (progn
             (set-value 1 (+ 3 pos))
             (compute (+ 4 pos)))
           (progn
             (set-value 0 (+ 3 pos))
             (compute (+ 4 pos)))))
      (99 ;; halt
       (format t "I'm finished"))
      (t ;; Unknown opcode
       (format t "Error, opcode ~a received~%" opcode)
       ))))

(defmacro get-value (pos)
  (if (eql 1 pos)
      `(real-value (+ ,pos pos) first-mode)
      `(real-value (+ ,pos pos) second-mode)))

(defun real-value (pos mode)
  (if (eq mode 0)
      (eltderef *instructions* pos) ;; Pos mode, get value at pos
      (elt *instructions* pos)));; Immediate mode, get value directly

;; Pos is always resolved to pos mode
(defun set-value (value pos)
  (let ((actual-pos (elt *instructions* pos))) ;; A value that's stored is always in pos mode (at least never in immediate mode)
    (setf (aref *instructions* actual-pos) value))
  *instructions*
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
