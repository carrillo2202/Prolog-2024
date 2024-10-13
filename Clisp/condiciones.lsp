;Escribir un programa que pregunte la edad al usuario
;determinar si es mayor de edad usando con todos ls casos if unless when, 

(defun edadIf()
    (princ "Dame tu edad ")
    (setq a (read))
    (if (> a 17)
    (princ "Es mayor de edad")
    (princ "Es menor de edad"))) 

(defun edadWhen()
    (princ "Dame tu edad ")
    (setq a (read))
    (when (> a 17)
    (princ "Es mayor de edad"))
    (when (< a 17)
    (princ "Es menor de edad"))
)

(defun edadUnlesss()
    (princ "Dame tu edad ")
    (setq a (read))
    (unless (> a 17)
    (princ "Es menor de edad"))
    (unless (< a 17)
    (princ "Es mayor de edad"))
)


;Escribir un programa que pida un numero entero y muestre si es par o impar, eql se puede usar
(defun parImpar()
    (princ "Dame el numero ")
    (setq a (read))
    (setq b(mod a 2))
    (if (eql b 0)
    (princ "Es par")
    (princ "Es impar"))
)

;Hacer el recorrido de descabechamiento de una lista, no se lleva mas de 4 lineas

(defun descabechar (lst)
  (when lst
    (print lst)
    (descabechar (cdr lst))))
