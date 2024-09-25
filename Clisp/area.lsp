(defun areaCuadrado()
    (princ "Dame un lado")
    (setq a (read))
    (setq b (* a a))
    (format t "El area del cuadrado es: ~a~%" b)) 

(defun volumenCubo()
    (princ "Dame un lado")
    (setq a (read))
    (setq b (* a a a))
    (format t "El volumen del cubo es: ~a~%" b)) 
    
    
    