(defun factorial(n)
    (if (= n 0)
    1
    (* n (factorial(- n 1))
    )))

(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
  ))


(defun division (n m )
    (if ( < n m)
    0
    (+ 1 (division (- n m) m)) 
    )
)