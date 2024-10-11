(defun recorre (lista) 
    (when lista 
        (let ((elemento (car lista)))
            (format t "¿Tu personaje es -a?-%" (car elemento)) 
         ;   (format t "Atributos: -a-%" (cadr elemento)) 
         (recorre (cdr lista))
        )
    )
)





(defparameter *nodes* '(
    (hombre
        (Brian_O'Conner ( Brian O'Conner))
        (Deckard_Shaw ( Deckard Shaw))
        (Dominic_Toretto ( Dominic Toretto))
        (Han_Lue ( Han Lue))
        (Luke_Hobbs ( Luke Hobbs))
        (Roman_Pearce ( Roman Pearce))
        (Sean_Boswell ( Sean Boswell))
        (Tej_Parker ( Tej Parker))
        (Rico_Santos ( Rico Santos))
    )
    (mujer
        (Cipher ( Cipher))
        (Elena_Neves ( Elena Neves))
        (Gisele_Yashar ( Gisele Yashar))
        (Letty_Ortiz ( Letty Ortiz))
        (Megan_Ramsey ( Megan Ramsey))
        (Mia_Toretto ( Mia Toretto))
    )
    (villano
        (Cipher ( Cipher))
        (Deckard_Shaw ( Deckard Shaw))
    )
    (negro
        (Roman_Pearce ( Roman Pearce))
        (Tej_Parker ( Tej Parker))
    )
    (aliado
        (Brian_O'Conner ( Brian O'Conner))
        (Dominic_Toretto ( Dominic Toretto))
        (Elena_Neves ( Elena Neves))
        (Gisele_Yashar ( Gisele Yashar))
        (Han_Lue (Han Lue))
        (Letty_Ortiz ( Letty Ortiz))
        (Luke_Hobbs ( Luke Hobbs))
        (Megan_Ramsey ( Megan Ramsey))
        (Mia_Toretto ( Mia Toretto))
        (Rico_Santos ( Rico Santos))
        (Roman_Pearce ( Roman Pearce))
        (Sean_Boswell ( Sean Boswell))
        (Tej_Parker ( Tej Parker))
    )
))
