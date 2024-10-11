;regresar el 4 elemento
Break 2 [12]> (car(cdddr '(2 4 4 8 9 4 3 4)))

;3 ultimos
Break 8 [18]> (cdr(cddddr '(2 4 4 8 9 4 3 4)))
(4 3 4)

;3 primeros
(list (car '(2 4 4 8 9 4 3 4))
      (car (cdr '(2 4 4 8 9 4 3 4)))
      (car (cdr (cdr '(2 4 4 8 9 4 3 4)))))


;eliminar el primer y ultimo

;sumar los primeros tres numeros

;recorrer toda la lista

;invertir los pares de lista, es decir, cambar el 2 por el 4, luego el 4 por el 8,etc