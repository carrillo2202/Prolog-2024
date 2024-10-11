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
(defun eliminar-primero-ultimo (lst)
  (if (null (cdr (cdr lst)))
      nil
      (cons (car (cdr lst)) (eliminar-primero-ultimo (cdr lst)))))

;(eliminar-primero-ultimo '(2 4 4 8 9 4 3 4))

;sumar los primeros tres numeros
(defun suma-primeros-tres (lst)
  (+ (car lst)
     (car (cdr lst))
     (car (cdr (cdr lst)))))

;(suma-primeros-tres '(2 4 4 8 9 4 3 4))


;recorrer toda la lista
(defun print-list (lst)
  (when lst
    (print (car lst))
    (print-list (cdr lst))))

;(print-list '(2 4 4 8 9 4 3 4))




;invertir los pares de lista, es decir, cambar el 2 por el 4, luego el 4 por el 8,etc

(defun swap-pairs (lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) lst)
        (t (cons (car (cdr lst))
                 (cons (car lst)
                       (swap-pairs (cdr (cdr lst))))))))

;(swap-pairs '(2 4 4 8 9 4 3 4))
