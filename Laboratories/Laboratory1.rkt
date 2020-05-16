;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Taller) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp")) #f)))
;; TALLER

;; EJERCICIO 1
;; Volumen-de-circulo r: numero -> numero
;; Calcular el volumen de una esfera
;; ejemplo: (Volumen-de-circulo 1) se debe producir #i4.1887902047863905 =(* 4/3 pi)

(define (Volumen-de-circulo r)(* 4/3 pi (expt r 3)))

;;prueba
(check-within (Volumen-de-circulo 1) 4 4.2 )


;; EJERCICIO 2
;; natural? n : numero -> Booleano
;; Definir si un numero pertenece a los numeros naturales
;; ejemplo: (natural? 1.1) se debe producir false

(define (natural? n)
   (cond
    [(and(> n 0) (= 1 (/ n (round n)))) true]
    [else false]))

;; prueba
(check-expect (natural? 1.1) false)
(check-expect (natural? -1) false)
(check-expect (natural? 1) true)
(check-expect (natural? 0) false)


;; EJERCICIO 3
;; par? x: numero -> Booleano
;; Definir si un numero es par
;; ejemplo: (par? 2) se debe producir true

(define (par? x)
  (if (= 0 (remainder x 2) )
      true
      false))
;; prueba
(check-expect(par? 2) true)
(check-expect(par? 3) false)
(check-expect(par? -3) false)
(check-expect(par? -2) true)


;; EJERCICIO 4
;; multiplos? a b: numero numero-> Booleano
;; Definir si un numero es multiplo de otro
;; ejemplo: (multiplos? 4 2) se debe producir true

(define (multiplos? x y) (cond [ (= (/ x y) (round (/ x y))) true] [else false]))         

;; prueba
(check-expect (multiplos? 4 2) true)
(check-expect (multiplos? 8 3) false)

;; EJERCICIO 5
;; raiz a b c: numero numero numero-> numero
;; Determinar el numero de raices reales de la ecuacion ax^2+bx+c=0
;; ejemplo: (raiz 1 2 3) se debe producir 0

(define(raiz a b c)(cond [(= (- (* b b)(* 4 a c)) 0) 1]
                           [else (cond [(> (- (* b b)(* 4 a c)) 0) 2]
                                       [else 0])]))
;;prueba
(check-expect (raiz 1 2 3) 0)
(check-expect (raiz 2 3 1) 2)
(check-expect (raiz 1 2 1) 1)

;; EJERCICIO 6
;; fact x: numero-> numero
;; Determinar el factorial de un numero
;; ejemplo: (fact 3) se debe producir 6

(define (fact x) (cond [(> x 0)(* x (fact (- x 1)))][(= 0 x) 1]))

;; prueba
(check-expect (fact 3) 6)
(check-expect (fact 0) 1)
(check-expect (fact 4) 24)



       
  