#lang plai
#| Lenguajes de Programación
   Práctica 1 |#

;; Función que toma dos números enteros y los eleva a sí mismos para luego sumar las potencias, es
;; decir, debe regresar a^b + b^a.
;; pot-sum: number number -> number
(define (pot-sum a b)
    (+ (expt a b) (expt b a)))


;; Función que calcula el área de un triángulo dados sus lados, usando la fórmula de Herón.
;; area-heron: number number number -> number
(define (area-heron a b c)
  (let* ([s (/ (+ (+ a b) c) 2)] [x (- s a)] [y (- s b)] [z (- s c)])
   (sqrt (* (* (* x y) z) s)) ))


;; Función que dado un día de la semana representado mediante los 0 = do, 1 = lu, 2 = ma, ... sá = 6,
;; y un valor booleano y un valor booleano indicando si son vacaciones, regresa un símbolo que indica
;; a qué hora sonará una alarma.
;; alarma: number boolean -> symbol
(define (alarma dia vacaciones)
    (if (equal? vacaciones #t)

        (cond
         [(= dia 1) '10:00]
         [(= dia 2) '10:00]
         [(= dia 3) '10:00]
         [(= dia 4) '10:00]
         [(= dia 5) '10:00]
         [(= dia 6) 'apagada]
         [(= dia 0) 'apagada]
         [else (error 'dia-incorrecto)])

    (cond
         [(= dia 1) '7:00]
         [(= dia 2) '7:00]
         [(= dia 3) '7:00]
         [(= dia 4) '7:00]
         [(= dia 5) '7:00]
         [(= dia 6) '10:00]
         [(= dia 0) '10:00]
         [else (error 'dia-incorrecto)])))



;; Predicado que dados dos números enteros, determina si el segundo es divisor propio del primero.
;; divisor-propio?: number number -> boolean
(define (divisor-propio? n m)
  (if (and (= (modulo n m) 0) (not (= n m))) 
    #t
    #f
  ))

;; Predicado que dado un número natural, determina si es un número perfecto.
;; es-perfecto?: number -> boolean
(define (es-perfecto? n)
  (aux-perfecto 1 0 n))
 
;; Fucion auxiliar para resolver es-perfecto?
(define (aux-perfecto c s n)
  (if (< c n)
      (if (divisor-propio? n c)
          (aux-perfecto (+ c 1) (+ s c) n)
          (aux-perfecto (+ c 1) s n))
      (if (= s n)
          #t
          #f)))
  
; Funcion que dado un número n regresa una lista del rango 2-(n-1)
(define (crea-lista n)
  (cdr(build-list (- n 1) add1)))

;; Función que obtiene la longitud de una lista.
;; longitud: list -> number
(define(long l)
  (match l
    ['() 0]
    [(cons x xs)(+ 1 (long xs))]))

;; Función que devuelve una lista sin elementos antes de la posición n de la lista original.
;; quita: (listof a) number -> (listof a)
(define (quita lst n)
     (if (or (empty? lst) (< n 1))
         lst
(quita (cdr lst) (- n 1))))

;Elimina multilos de un numero dado en un alista
(define (eli-mults l1 laux n)
  (cond
   [(= (long l1) 0) (reversa laux)]
   [(= (car l1) n) (eli-mults (cdr l1) (cons (car l1) laux) n)]
   [(divisor-propio? (car l1) n) (eli-mults (cdr l1) laux n)]
   [(not (divisor-propio? (car l1) n)) (eli-mults (cdr l1) (cons (car l1) laux) n)])) 
        
;Reversa de na lista  
(define (reversa l)
    (if (null? l) '()
    (append (reversa (cdr l)) (list (car l)))))
        


;; Función auxiliar que calcula el cuadrado s-dígito de un número natural n.
;; sdigito: number -> number
(define (sdigito n m)
  (cond
  [(< m 20)  
  (if (< n 10)
        n 
        (sdigito (+ (expt (modulo n 10) 2 )
                    (expt (sdigito (quotient n 10)(+ m 1)) 2)) (+ m 1)))]
  [else -1])
)


;; Predicado que dado un número natural, determina si un número es feliz.
;; es-feliz?: number -> boolean
(define (es-feliz? n)
    (cond
      [(< n 10) (if (= 1 (sdigito (expt n 2) 0))
                    #t
                    #f)
                    ]
      [else (if (= 1 (sdigito n 0))
                #t
#f)]))


;; Función que recibe una lista de números y regresa una nueva lista que contiene únicamente aquellos
;; que son felices.
(define (felices lista)
    (filter es-feliz? lista))


;Función que recibe un número y regresa una lista de sus digitos.
(define (number->list num)
  (map (lambda (c) (- (char->integer c) (char->integer #\0)))
       (string->list
        (number->string num))))


;; Función que encuentra el factorial de un número.
;; factorial: number -> number
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;; Función que encuentra el factorión de un número.
;; factorionr: number -> number
(define (factorionr n)
    (foldr + 0 (map factorial (number->list n))))


;; Función que encuentra el factorión de un número.
;; factorionl: number -> number
(define (factorionl n)
    (foldl + 0 (map factorial (number->list n))))


        
;; Función recursiva que encuentra los números primos en un rango de m a n usando la Criba de 
;; Eratóstenes.
;; criba-eratostenes number number -> (listof number)
;(define (criba-eratostenes n)
    ;(aux-criba (crea-lista n) '() (long (crea-lista n))))


  

#|
(define (son-amigos? a b)
    #| Aquí va su código. |#)



;; Función recursiva que encuentra los números primos en un rango de m a n usando la Criba de 
;; Eratóstenes.
;; criba-eratostenes number number -> (listof number)
(define (criba-eratostenes n)
    #| Aquí va su código. |#)

;; Función recursiva que toma un número y regresa una lista de pares con la descomposición en primos
;; del mismo.
;; descomposicion-primos: number -> (listof number)
(define (descomposicion-primos n)
    #| Aquí va su código. |#)

;; Función que recibe una lista de números entre 0 y 99 y regresa una lista con su representación en
;; japones.
;; a-japones: (listof number) -> (listof string)
(define (a-japones lista)
    #| Aquí va su código. |#)

;; Función que recibe una lista de números y regresa una nueva lista que contiene únicamente aquellos
;; que son felices.
(define (felices lista)
    #| Aquí va su código. |#)





|#