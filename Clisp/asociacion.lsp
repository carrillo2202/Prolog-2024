(defun recorre (lista) 
    (when lista 
        (let ((elemento (car lista)))
        (format t "¿Tu personaje es -a?-%" (car elemento)) 
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

(defparameter *nodes* '(
    (Sedan
        (Toyota (Camry))
        (Honda (Civic ))
        (Mazda (tres ))
        (Nissan (Versa ))
        (Chevrolet (Civic ))
      
    )
    (SUV
        (Toyota (Rav 4))
        (Honda (CRV ))
        (Mazda (CX-90 ))
        (Nissan (X-trail ))
        (Chevrolet (Blazer ))
       
    )
    (Pickup
        (Toyota (Tacoma))
        (Honda (Ridgeline ))
        (Mazda (BT-50 ))
        (Nissan (Frontier ))
        (Chevrolet (S10 ))
    )
    (Deportivo
        (Toyota (GT86))
        (Honda (CivicType R ))
        (Mazda (MX-5 ))
        (Nissan (GTR ))
        (Chevrolet (Camaro ))
    )
    (Hatchback
       (Toyota (Yaris))
        (Honda (City ))
        (Mazda (dos ))
        (Nissan (March ))
        (Chevrolet (Aveo ))
    )
))
