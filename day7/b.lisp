;; (defun main  ()
  ;; (compute-array (create-starting-states) '(9 8 7 6 5) 0 0))

(defun main ()
  (calculate (make-amps '(9 8 7 6 5)) 0 0))

(defun calculate (amps index input)
  (let* ((current-amp (elt amps index))
         (output (funcall current-amp input)))

    (if (eq 99 output)
        input
        (calculate amps
                   (if (eql index (- (length amps) 1))
                       0
                       (+ 1 index)) output))))

(defun make-amps (phases)
  (mapcar #'make-amp phases))

(defun make-amp (phase)
  (with-open-file (stream "inputb.txt")
    (let* ((input (read stream))
           (instructions (make-array (length input) :initial-contents input))
           (has-phase t)
           (position 0)
           (starting-phase phase))

      (lambda (in)
        ;; (format t "Amp called, starting phase is ~a~%" starting-phase)
        (let ((input-list (if has-phase
                              (list phase in)
                              (list in))))
          (setf has-phase nil)

          (multiple-value-bind (output last-position)
              (compute instructions position input-list nil)
            ;; (format t "Output ~a last-position ~a" output last-position)

            (setf position last-position)
            output))))))



;; (defun create-starting-states ()
;;   (loop for i upto 4 collect
;;                      (with-open-file (stream "inputb.txt" :if-does-not-exist nil)
;;                        (let ((input (read stream)))
;;                          (make-array (length input) :initial-contents input)))))

;; (defun start-compute-array (configurations)
;;   (compute-array configurations 0))

;; (defun compute-array (states configurations input loops)
;;   ;; (format t "Configurations ~a Input is ~a~%" configurations input)
;;   (if (car configurations)
;;       (let* ((instructions (elt states (- (length states) (length configurations))))
;;             (output (compute instructions 0 (list (car configurations) input) nil)))
;;         (assert (not (null output)) nil "Didn't expect nil output for configuration ~a and input ~a~%" (car configurations) input)
;;         (format t "INDEX IS ~a~%"(- (length states) (length configurations)))
;;         (when (eq output 69814864)
;;           (format t "ALMOST THERE~%"))
;;         (format t "OUTPUT for ~a is ~a loops ~a~%" (car configurations) output loops)
;;         (compute-array states (cdr configurations) output loops))
;;       (progn
;;         (format t "Done with array, looping back in with input ~a~%" input)
;;         (if (eq 6 loops)
;;             input
;;             (compute-array states '(9 8 7 6 5) input (+ 1 loops))))))

(defun compute (instructions pos input output)
  (multiple-value-bind (opcode first-mode second-mode)
      (extract-instruction (elt instructions pos))
    ;; (when (eq (car input) 69814864)
    ;;   (format t "GETTING THERE value at pos ~a~%" (elt instructions pos))
    ;; (format t "Input ~a Opcode ~a position ~a instructions ~a~%" input opcode pos instructions)
    (case opcode
      (1
       (set-value instructions
                  (+ (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute instructions (+ 4 pos) input output))
      (2
       ;; (format t "multiplying ~a with ~a and store new value ~a at at ~a~%" (get-value 1) (get-value 2) (* (get-value 1) (get-value 2)) (+ 3 pos))
       (set-value instructions
                  (* (get-value 1)
                     (get-value 2))
                  (+ 3 pos))
       (compute instructions (+ 4 pos) input output))
      (3
       (assert (car input) nil "Expected input, but there's no input to be found at position: ~a length of program ~a" pos (length instructions))
       (set-value instructions (car input) (+ pos 1))
       (compute instructions (+ 2 pos) (cdr input) output))
      (4
       ;; (format t "$$$$$$$$$$$$$$$$$$$$$$$$$$$ OUTPUT: ~a~%" (get-value 1))
       (values (get-value 1) (+ 2 pos)))
       ;; (compute instructions (+ 2 pos) input (get-value 1)))
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
       ;; (format t "-------------------------------------- HALT ~%")
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
