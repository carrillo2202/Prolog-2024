(defun recorre (lista) 
    (when lista 
        (let ((elemento (car lista)))
        (format t "¿Tu personaje es ~a?~%" (car elemento)) 
        (setq a (read))
        (if (string-equal a "si")
            (progn
                (setq b (cadr (assoc (car elemento) lista)))
                (recorre b)
            )
        (recorre (cdr lista))
        )
        )
    )
)
;;

(defun recorredos (lista &optional (nivel 0))
  (when lista
    (let ((elemento (car lista)))
      ;; Cambia la pregunta según el nivel
      (cond
        ((= nivel 0)
         (format t "¿Es un tipo de auto ~a?~%" (car elemento)))
        ((= nivel 1)
         (format t "¿Es de la marca ~a?~%" (car (car elemento))))
        (t
         (format t "¿Es el modelo ~a?~%" (car (cdr elemento)))))

      ;; Leer la respuesta del usuario
      (setq a (read))
      ;; Si el usuario responde "si", proceder
      (if (string-equal a "si")
          (let ((b (cdr elemento)))
            (cond
              ;; Si hay más listas anidadas, continúa recursivamente
              ((and (listp b) (listp (car b)))
               (recorredos b (1+ nivel)))
              ;; Si estamos en el último nivel, imprime el modelo final
              ((listp b)
               (format t "¡Encontrado! Tu personaje es el modelo ~a.~%" (car b)))
              ;; Caso base para evitar un ciclo
              (t
               (format t "¡Finalizado!"))))
          ;; Si el usuario responde "no", avanzar en el mismo nivel
          (recorredos (cdr lista) nivel))))) ;; Continuar con el siguiente elemento en el mismo nivel


(defparameter *nodes* '(
    (Sedan(
        (Toyota (Camry))
        (Honda (Civic ))
        (Mazda (tres ))
        (Nissan (Versa ))
        (Chevrolet (Civic ))
    )
    )
    (SUV (
        (Toyota (Rav Cuatro))
        (Honda (CRV ))
        (Mazda (CX-Noventa ))
        (Nissan (X-trail ))
        (Chevrolet (Blazer ))
    )
    )
    (Pickup (
        (Toyota (Tacoma))
        (Honda (Ridgeline ))
        (Mazda (BT-Cincuenta ))
        (Nissan (Frontier ))
        (Chevrolet (S10 ))
        )
    )
    (Deportivo (
        (Toyota (GT86))
        (Honda (CivicTypeR ))
        (Mazda (MX5 ))
        (Nissan (GTR ))
        (Chevrolet (Camaro ))
        )
    )
    (Hatchback (
       (Toyota (Yaris))
        (Honda (City ))
        (Mazda (dos ))
        (Nissan (March ))
        (Chevrolet (Aveo ))
    )
    )
))
