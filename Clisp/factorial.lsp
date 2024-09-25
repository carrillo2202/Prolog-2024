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

;n base
;m exponente
(defun multiplicar-por-suma (a b)
  (if (= b 0)
      0
      (+ a (multiplicar-por-suma a (- b 1)))))
(defun potencia-por-suma (a b)
  (if (= b 0)
      1  ; a^0 es siempre 1
      (multiplicar-por-suma a (potencia-por-suma a (- b 1)))))


;(potencia-por-suma 2 3)