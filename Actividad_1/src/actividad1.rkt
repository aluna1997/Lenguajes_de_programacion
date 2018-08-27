#lang plai

#| Lenguajes de Programación
   Actividad de laboratorio 1 |#

;; Función que calcula el área de un cilindro.
;; area-cilindro: number number -> number
( define ( area-cilindro d h )
   (let([r (/ d 2)])
(*(*(* 2 pi) r)(+ r h)))
   )

; Agregar 5 pruebas aquí.
(area-cilindro 15 20)
(area-cilindro 10 2)
(area-cilindro 1 20)
(area-cilindro 1 2)
(area-cilindro 10 25)

;; Función que calcula el tipo de multa que recibirá un conductor.
;; tipo-multa: number boolean -> symbol
( define ( tipo-multa v c)
   (if(equal? c #t)

      (let ([v (* v 5)])
        (cond
          [(and (> v 0) (< v 61)) 'sin-multa]
          [(and (> v 59)(< v 81)) 'multa-pequena]
          [ (> v 80) 'multa-grande]
          [else (error 'v 'velocidad-incorrecta)]))  
   
      (cond
        [(and (> v 0) (< v 61)) 'sin-multa]
        [(and (> v 59)(< v 81)) 'multa-pequeña]
        [ (> v 80) 'multa-grande]
        [else (error 'velocidad-incorrecta)])

   )
)

; Agregar 5 pruebas aquí.
(tipo-multa 50 #f)
(tipo-multa 61 #f)
(tipo-multa 70 #f)
(tipo-multa 81 #f)
(tipo-multa 70 #t)
