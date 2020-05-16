;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname parcial) (read-case-sensitive #t) (teachpacks ((lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "guess.rkt" "teachpack" "htdp") (lib "master.rkt" "teachpack" "htdp")) #f)))
; Punto 7
; Contrato:
; player es una estructura, que representa un jugador.
; sus atributos son nick de tipo string y level de tipo number.
; El level es un numero entre 0 y 32.

(define-struct player (nick level))


; instancias de la estructura player
(define player1 (make-player "cat2" 16))
(define player2 (make-player "ross" 24))
(define player3 (make-player "adcadc" 30))
(define player4 (make-player "ggg" 10))
(define player5 (make-player "dark1" 20))
(define player6 (make-player "dog" 28))
(define player7 (make-player "mk4" 14))
(define player8 (make-player "lol" 6))
(define player9 (make-player ":P" 32))
(define player10 (make-player "abc123" 15))

; Contrato
; team es una estructura que representa un equipo de LoL.
; sus atributos son name de tipo string.
; y top, middle, adc, support, jungle representan estructuras
; de player, de esta forma se identifica a cada jugador con su rol
; en el equipo.

(define-struct  team (name top middle adc support jungle))

; instancias de la estructura team
(define equipoA (make-team "A" player1 player2 player3 player4 player5))
(define equipoB (make-team "B" player6 player7 player8 player9 player10))

; Contrato:
; ponderacion: struct -> number
; Proposito: calcular la influencia segun el rol en el juego
; calculando la ponderacion por un factor determinado
; ejemplos: (ponderacion equipoA) debe arrojar 20.4
;           (ponderacion equipoB) debe arrojar 18.775 


(define (ponderacion x) (/(+
                          (*(player-level (team-top x)) 10)
                          (*(player-level (team-middle x)) 9)
                          (*(player-level (team-adc x)) 8)
                          (*(player-level (team-support x)) 6)
                          (*(player-level (team-jungle x)) 7)) 40))

;pruebas

(check-expect (ponderacion equipoA) 20.4)
(check-expect (ponderacion equipoB) 18.775)


; Contrato
; favorito: team team -> team
; Proposito: La funcion que predice el ganador entre 2 equipos a y b
; funciona sumando los niveles de los integrantes del equipo ponderandolos por
; un factor que experimentalmente dice cual es la influencia de cada rol en el juego
; Ejemplo (favorito equipoA equipoB) deberia de producir equipoA



(define (favorito a b) (cond
                         [ (= (ponderacion a) (ponderacion b)) "no hay favorito"]
                         [(> (ponderacion a) (ponderacion b)) a]
                         [else b]))
                         
                                                 
;prueba

(check-expect (favorito equipoA equipoB) equipoA)

;-----------------------------------------------------------------------------------------------------------------

;Punto 8
; Contrato:
; la estructura lugar representa el lugar de trabajo que se
; le asignan a los empleados
; cuyos parametros son edificio, oficina y extension de tipo number.
(define-struct lugar (edificio oficina extension))

;Instancias de la estructura lugar
(define lugar1 (make-lugar 3212 43 12345))
(define lugar2 (make-lugar 32 43 12545))
(define lugar3 (make-lugar 3212 43 14345))

; Contrato:
; la estructura empleado representa a una persona natural
; cuyos parametros son nombre y apellido que son de tipo strings
; cedula que es de tipo number y lugar que es de tipo struct
(define-struct empleado (nombre apellido cedula lugar))

; Instancias de la estructura empleado
(define empleado1 (make-empleado "Juan" "Velasquez" 1005869667 lugar1))
(define empleado2 (make-empleado "Jhoan" "Henao" 1144097614 lugar2))
(define empleado3 (make-empleado "Diana" "Ochoa" 16775170 lugar3))

;-----------------------------------------------------------------------------------------------
; Punto 9
; Contrato:
; get-phone: struct number -> number
; Proposito: Retornar el numero de extension del empleado
; si el numero de la cedula es igual al parametro dado
; ejemplos: (get-phone empleado1 1005869667) debe de arrojar 12345
;           (get-phone empleado2 1005869667) debe de arrojar ""
;           (get-phone empleado3 16775170) debe de arrojar 14345

(define (get-phone empleado cedula) (cond
                                      [(equal? (empleado-cedula empleado) cedula)
                                       (lugar-extension (empleado-lugar empleado))   ]
                                      [else ""]))

(check-expect (get-phone empleado1 1005869667) 12345)
(check-expect (get-phone empleado2 1005869667) "")
(check-expect (get-phone empleado3 16775170) 14345)


;------------------------------------------------------------------------------------
; Punto 10

; Contrato:
; suma-anteriores : number -> number
; Proposito: Esta funcion determina una secuencia
; que mapea 1 cuando n es igual a 0 y a 1
; que mapea 2 cuando n es igual a 2
; y siguiendo la secuencia ordenada, seguira arrojando
; la suma de las 3 numeros anteriores de la secuencia.
; ejemplos: (suma-anteriores 2) deberia de arrojar 2
;           (suma-anteriores 3) deberia de arrojar 4
;           (suma-anteriores 4) deberia de arrojar 7

(define (suma-anteriores n) (cond
                      [(= n 0) 1]
                      [(= n 1) 1]
                      [(= n 2) 2]
                      [else (+ (suma-anteriores(- n 1))
                               (suma-anteriores (- n 2))
                               (suma-anteriores (- n 3)))]))
;pruebas
(check-expect (suma-anteriores 2)  2)
(check-expect (suma-anteriores 3)  4)
(check-expect (suma-anteriores 4)  7)


;---------------------------------------------------------------------------------------------------
; Punto 11

; Contrato:
; residuo: number number -> number
; Proposito:
; esta funcion determina el residuo de la division de dos numeros
; ejemplos: (residuo 5 3) deberia de arrojar 2
;           (residuo 8 4) deberia de arrojar 0

(define (residuo x y) (remainder x y))

(check-expect (residuo 5 3) 2)
(check-expect (residuo 8 4) 0)

; Contrato:
; MCD: number number -> number
; Proposito:
; Determinar el Maximo comun divisor entre dos numeros
; por medio del algoritmo de EUCLIDES
; ejemplos: (MCD 20 5) deberia de arrojar 5
;           (MCD 8.2 3) deberia de arrojar "error"
;           (MCD 8 4) deberia de arrojar 4
;           (MCD -6 -9) deberia de retornar "error"

(define (MCD m n) (cond
                    [(and ( and(integer? m)  (> m 0)) (and (integer? n)(> n 0)))
                    
                    (cond
                       [(> m n)
                         (cond
                       [(= (residuo m n) 0) n]
                       [else (MCD n (residuo m n))])]
                                            
                       [(> n m)
                         (cond
                       [(= (residuo n m) 0) m]
                       [else (MCD m (residuo n m))])])]
                    [else "error"]))
                        

;pruebas
(check-expect(MCD 20 5)  5)
(check-expect(MCD 8.2 3) "error")
(check-expect(MCD 8 4)  4)
(check-expect(MCD -6 -9)  "error")



;_-----------------------------------------------------------------------------------------
; Contrato:
; invertir-orden: number number number-> number
; Proposito: esta funcion retorna un numero
; a partir de 3 digitos dados empezando con el menos significativo
; ejemplos: (invertir-orden 1 2 3) deberia de retornar 321
;           (invertir-orden 4 5 2) deberia de retornar 254
;           (invertir-orden 4 9 7) deberia de retornar 794

(define (invertir-orden a b c)
  (+ (* a 1) (* b 10) (* c 100)))

;pruebas
(check-expect(invertir-orden 1 2 3) 321)
(check-expect(invertir-orden 4 5 2) 254)
(check-expect(invertir-orden 4 9 7) 794)


