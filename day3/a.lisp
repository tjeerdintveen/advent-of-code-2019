; 266 is right answer. Brute forced it though
(defun main  ()
  (let ((in (open "input.txt")))
    (print (solve (read in) (read in)))
    (close in)))

(defun solve (lhs rhs)
  (second
   (sort
    (mapcar (lambda (point) (distance '(0 0) point))
            (intersection (generate lhs) (generate rhs) :test #'is-equal))
    #'<
    )))

(defun is-equal (lhs rhs)
  (and (eq (first lhs) (first rhs))
       (eq (second lhs) (second rhs))))

(defun distance (lhs rhs)
  (+
   (abs (- (first lhs) (first rhs)))
   (abs (- (second lhs) (second rhs)))))

(defun generate (symbols)
  "Generate all coordinates for a list of symbols"
  (do ((lhs (paths->coordinates symbols) (cdr lhs)) ; poor man's zip
       (rhs (subseq (paths->coordinates symbols) 1) (cdr rhs))
       (result '() (append
                    result
                    (fill-coordinates (car lhs) (car rhs))
                    )))
      ((not (and lhs rhs)) result)))

(defun fill-coordinates (lhs rhs)
  "Add intermediate coordinates. e.g. ((0 0) (0 4) (0 6)) Becomes ((0 0) (0 1) (0 2) (0 3) (0 4) (0 5) (0 6))"
  (defun low-high (low high)
    (loop
      for i from low to high
      collect i))

  (defun high-low (high low)
    (loop
      for i from high downto low
      collect i))

  (cond
    ((and ; x pos from low to high
      (eq (second lhs) (second rhs))
      (< (first lhs) (first rhs)))
     (mapcar (lambda (x)
               (list x (second lhs)))
             (low-high (first lhs) (first rhs))))
    ((and ; x pos from high to low
      (eq (second lhs) (second rhs))
      (> (first lhs) (first rhs)))
     (mapcar (lambda (x)
               (list x (second lhs)))
             (high-low (first lhs) (first rhs))))
    ((and ; y pos from low to high
      (eq (first lhs) (first rhs))
      (< (second lhs) (second rhs)))
     (mapcar (lambda (y)
               (list (first lhs) y))
             (low-high (second lhs) (second rhs))))
    ((and ; y pos from high to low
      (eq (first lhs) (first rhs))
      (> (second lhs) (second rhs)))
     (mapcar (lambda (y)
               (list (first lhs) y))
             (high-low (second lhs) (second rhs))))
        (t nil)))

(defun paths->coordinates (paths)
  "Create coordinates from multiple paths. E.g. '(R8 R8 R10) becomes ((0 0) (8 0) (16 0) (26 0))"
  (reverse (reduce (lambda (result path)
                     (append (list (path->coordinate path (first result))) result))
                   paths
                   :initial-value '((0 0))
                   )))

(defun path->coordinate (path start)
  "Turn a path e.g. R8 into a coordinate (0 8), offset by start. E.g. 'R8 (3 0) becomes (11 0). Left/Up is minus"
  (multiple-value-bind (direction amount) (split-symbol path)
    (cond
      ((string= "R" direction)
       (list
        (+ amount (first start))
        (second start)))
      ((string= "L" direction)
       (list
        (- (first start) amount)
        (second start)))
      ((string= "U" direction)
       (list
        (first start)
        (- (second start) amount)))
      ((string= "D" direction)
       (list
        (first start)
        (+ (second start) amount)))
      (t nil))))

(defun split-symbol (dirsym)
  "Split the directionsymbol e.g. R2 into a string and an amount."
  (let ((char (elt (string dirsym) 0))
        (amount (parse-integer (subseq (string dirsym) 1))))
    (values char amount)))

(defun test ()
  (and
   (equalp '(13 0) (path->coordinate 'R10 '(3 0)))
   (equalp '(11 0) (path->coordinate 'R8 '(3 0)))
   (equalp '(-5 0) (path->coordinate 'L8 '(3 0)))
   (equalp '(3 -6) (path->coordinate 'U8 '(3 2)))
   (equalp '(3 10) (path->coordinate 'D8 '(3 2)))

   (equalp 6 (solve '(R8 U5 L5 D3) '(U7 R6 D4 L4)))
   (equalp 6 (solve '(R8 U5 L5 D3) '(U7 R6 D4 L4)))
   (equal 159 (solve '(R75 D30 R83 U83 L12 D49 R71 U7 L72) '(U62 R66 U55 R34 D71 R55 D58 R83)))
   ))

