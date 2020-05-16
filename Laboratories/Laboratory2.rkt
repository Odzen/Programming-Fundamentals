;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname taller-final) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
; Punto 1
; Contrato:
; persona es una estructura con tres atributos:
; nombre y nacimiento que son subestructuras e email que es una cadena.
; Esta almacena la informacion basica para una persona,
; anidando otras dos estructuras.


(define-struct persona (nombre correo-electronico nacimiento ))

; Punto 1-A
; Contrato: 
; Esta estructura llamada -nombre- esta compuesta por 3 parametros
; donde nombre, apelllido y apellido2 son una cadena 

(define-struct nombre (nombre apellido apellido2))

; Punto 1-C
; Contrato:
; Esta estructura llamada -nacimiento- esta compuesta por 3 parametros
; donde dia, mes y año son numeros

(define-struct nacimiento (dia mes año))

;------------------------------------------------------------------------------------------------------------

; Punto 2
; Contrato:
; Instancias de la estructura persona

(define Juan-Acevedo (make-persona (make-nombre "Juan" "Acevedo" "Velasquez")
                                 "juan@gmail.com"
                                 (make-nacimiento 28 8 2000)))

(define Juan-Monteria (make-persona (make-nombre "Juan" "Monteria" "Velasquez")
                                "monteria@gmail.com "
                                 (make-nacimiento 29 8 2000)))

(define Daniela-Velasquez (make-persona (make-nombre "Daniela" "Velasquez" "Monteria")
                                   "daniela@gmail.com"
                                 (make-nacimiento 20 8 1999)))

;-----------------------------------------------------------------------------------------------------------------------------------
; Contrato:
; Esta estructura llamada fechactual esta compuesta por 3 parametros
; donde dia,  mes y año son numeros
; esta estructura es necesaria para realizar las funciones posteriores
(define-struct fechactual (dia mes año))

; Instancia de la estructura fechactual
(define hoy (make-fechactual 27 9 2017))
;------------------------------------------------------------------------------------------------------------------------------
; Punto 3
; Contrato:
; mayor-edad? : persona -> boolean
; Proposito:
; Identifica a una persona como mayor de edad, retornando #true si asi es, y sino, #false
; La mayoria de edad se identifica cuando la diferencia entre el año actual y el de nacimiento
; es mayor o igual a 18, si esto no es asi, se identifica cuando la resta entre el mes actual y el de nacimiento
; es mayor o igual a 0, si esto no se cumple, se indentifica cuando la resta entre el dia actual y el dia de nacimiento
; es mayor o igual a 0
; Ejemplo: (mayor-edad? Juan-Acevedo) deberia devolver #false
; Ejemplo: (mayor-edad? DAniela-Velasquez) deberia devolver #true

(define (mayor-de-edad? c ) (cond
                          [(and (>= (- (fechactual-año hoy) (nacimiento-año (persona-nacimiento c) )) 18)
                               (>= (- (fechactual-mes hoy) (nacimiento-mes (persona-nacimiento c))) 0)
                               (>= (- (fechactual-dia hoy) (nacimiento-dia (persona-nacimiento c))) 0)) #t]
                           [else  #f ]))


;Pruebas
(check-expect (mayor-de-edad? Daniela-Velasquez) #t)
(check-expect (mayor-de-edad? Juan-Acevedo) #f)

;------------------------------------------------------------------------------------------------------------------------------
; Punto 4
; Contrato:
; mayor-persona: persona persona -> persona or display
; Proposito:
; Compara la fecha de nacimiento de dos personas y devuelve la persona de
; mayor edad. Si estos cumplen años el mismo dia se retorna un
; display que lo indica. La comparacion se empieza por los años de
; nacimiento, si estos son iguales, se comparan los meses y si tambien
; son iguales se compara el dia.
; ejemplo: (mayor Juan-Monteria Juan-Acevedo) deberia de retornar la estructura de persona de Juan-Acevedo
; ejemplo: (mayor Daniela-Velasquez Juan-Acevedo) deberia de retornar la estructura de persona de Daniela-Velasquez

(define (mayor-persona p1 p2)
  (cond
    [(<  (nacimiento-año (persona-nacimiento p1)) (nacimiento-año (persona-nacimiento p2))) p1]
    [(> (nacimiento-año (persona-nacimiento p1)) (nacimiento-año (persona-nacimiento p2))) p2]
    [(= (nacimiento-año (persona-nacimiento p1)) (nacimiento-año (persona-nacimiento p2)))
     (cond
       [(< (nacimiento-mes (persona-nacimiento p1)) (nacimiento-mes (persona-nacimiento p2))) p1]
       [(> (nacimiento-mes (persona-nacimiento p1)) (nacimiento-mes (persona-nacimiento p2))) p2]
       [(= (nacimiento-mes (persona-nacimiento p1)) (nacimiento-mes (persona-nacimiento p2)))
        (cond
          [(< (nacimiento-dia (persona-nacimiento p1)) (nacimiento-dia (persona-nacimiento p2))) p1]
          [(> (nacimiento-dia (persona-nacimiento p1)) (nacimiento-dia (persona-nacimiento p2))) p2]
          [(= (nacimiento-dia (persona-nacimiento p1)) (nacimiento-dia (persona-nacimiento p2))) (display "nacidos el mismo dia")]
          [else 'error])
        ]
       [else 'error])
     ]
    [else 'error]))

               
;Pruebas
(check-expect (mayor-persona  Juan-Monteria Juan-Acevedo)Juan-Acevedo)
(check-expect (mayor-persona  Daniela-Velasquez Juan-Acevedo)Daniela-Velasquez)

;--------------------------------------------------------------------------------------------------------------------------------------
; Punto 5
; Contrato:
; parientes?: persona persona -> Boolean
; Proposito:
; esta funcion determina si dos personas son parientes
; esto se identifica si tienen cualquier apellido en comun
; ejemplo: (parientes Juan-Acevedo Daniela-Velasquez) deberia retornar #true
; ejemplo: (parientes Juan-Monteria Daniela-Velasquez) deberia retornar #true
(define(parientes? a b) (cond
                         [(or (equal? (nombre-apellido(persona-nombre a)) (nombre-apellido(persona-nombre b)))
                              (equal? (nombre-apellido2(persona-nombre a)) (nombre-apellido2(persona-nombre b)))
                              (equal? (nombre-apellido(persona-nombre a)) (nombre-apellido2(persona-nombre b)))
                              (equal? (nombre-apellido2(persona-nombre a)) (nombre-apellido(persona-nombre b)))) #t]
                         [else #f]))

;pruebas
(check-expect (parientes? Juan-Acevedo Daniela-Velasquez)#t)
(check-expect (parientes?  Juan-Monteria Juan-Acevedo)#t)
(check-expect (parientes? Juan-Monteria Daniela-Velasquez)#t)

;----------------------------------------------------------------------------------------------------------------------------------------------------
; Punto 6
; Contrato:
; punto es una estructura que representa un punto bidimensional
; que cuenta con dos atributos: "x" "y" los cuales son numeros.

(define-struct punto (x y))
;------------------------------------------------------------------------------------------------
; Punto 7
; Contrato:
; Rectangulo es una estructura de dos atributos: punto1 y punto2
; que son subestructuras de punto. Esta estructura representa
; a un rectangulo, dandonos dos de sus vertices, puesto que los
; otros dos se pueden asumir.
(define-struct rectangulo (punto1 punto2))
;------------------------------------------------------------------------------------------------
; Punto 8
; Contrato:
; Instancias de punto 
(define punto1 (make-punto 5 3))
(define punto2 (make-punto 4 8))
(define punto3 (make-punto 2 1))
(define punto4 (make-punto 2 0))
(define punto5 (make-punto 0 2))
(define punto6 (make-punto 1 3))
(define punto7 (make-punto 3 1))
(define punto8 (make-punto 3 0))
(define punto9 (make-punto 2 3))
(define punto10 (make-punto 1 1))
(define punto11 (make-punto 4 2))
(define punto12 (make-punto 1 1))
(define punto13 (make-punto 3 2))
(define punto14 (make-punto 6 6))
(define punto15 (make-punto 7 7))
(define punto16 (make-punto 50 50))
(define punto17 (make-punto 150 150))

;------------------------------------------------------------------------------------------------
; Punto 9
; Contrato:
; Instancias de rectangulo
(define rectangulo1 (make-rectangulo punto1 punto2))
(define rectangulo2 (make-rectangulo punto3 punto8))
(define rectangulo3 (make-rectangulo punto1 punto3))
(define rectangulo4 (make-rectangulo punto1 punto3))
(define rectangulo5 (make-rectangulo punto11 punto10))
(define rectanguloa (make-rectangulo punto4 punto5))
(define rectangulob (make-rectangulo punto6 punto7))
(define rectanguloc (make-rectangulo punto8 punto9))
(define rectangulod (make-rectangulo punto10 punto11))
(define rectangulo6 (make-rectangulo punto12 punto13))
(define rectangulo7 (make-rectangulo punto14 punto15))
(define rectangulo8 (make-rectangulo punto16 punto17))

;-------------------------------------------------------------------------------------------------
; Punto 10
; Contrato:
; punto-contenido?: rectangulo punto -> boolean
; Proposito:
; Esta funcion determina si un punto bidimensional esta contenido
; dentro de un rectangulo, esto se identifica mediante intervalos
; de esta forma se compara las coordenadas del punto con los vertices
; del rectangulo.
; ejemplo: (punto-contenido? rectangulo1 punto1) deberia retornar #t
; ejemplo: (punto-contenido? rectangulo2 punto1) deberia retornar #f

(define (punto-contenido? rec punto)
  (and
   (or 
    (and (<= (punto-x punto) (punto-x (rectangulo-punto2 rec)))
         (>= (punto-x punto) (punto-x (rectangulo-punto1 rec))))
    (and (<= (punto-x punto) (punto-x (rectangulo-punto1 rec)))
         (>= (punto-x punto) (punto-x (rectangulo-punto2 rec)))))
   (or 
    (and (<= (punto-y punto) (punto-y (rectangulo-punto2 rec)))
         (>= (punto-y punto) (punto-y (rectangulo-punto1 rec))))
    (and (<= (punto-y punto) (punto-y (rectangulo-punto1 rec)))
         (>= (punto-y punto) (punto-y (rectangulo-punto2 rec)))))))
;pruebas
(check-expect (punto-contenido? rectangulo1 punto1) #t)
(check-expect (punto-contenido? rectangulo2 punto1) #f)



;-----------------------------------------------------------------------------------------------------
; Contrato:
; Se prosigue a definir los 4 puntos del rectangulo
; para facilitar la recursividad en funciones posteriores

(define (pto1 a) (rectangulo-punto1 a))
(define (pto2 a) (rectangulo-punto2 a))
(define (pto3 a) (make-punto (punto-x (rectangulo-punto1 a)) (punto-y (rectangulo-punto2 a))))
(define (pto4 a) (make-punto (punto-x (rectangulo-punto2 a)) (punto-y (rectangulo-punto1 a))))
;---------------------------------------------------------------------------------------------------------
; Punto 11
; Contrato:
; sobrelapan?: rectangulo rectangulo -> Boolean
; Proposito: 
; Determinar si un rectangulo se sobrelapa con otro
; Para esto se utiliza la funcion (punto-contenido?), de esta manera se utliza la recursividad
; Se evaluan los casos en los cuales se sobrelapan
; cuando alguno de los 4 vertices de un rectangulo pertence al otro
; o si se sobrelapan en cruz, para determinar esto se utillizan intervalos

;ejemplo: (sobrelapan? rectangulo2 rectangulo3) deberia retornar #true
;ejemplo (sobrelapan? rectangulo1 rectangulo2) deberia de producir #f
(define (sobrelapan? rec1 rec2)
  (or  
   (punto-contenido? rec2 (pto4 rec1))
   (punto-contenido? rec2 (pto3 rec1))
   (punto-contenido? rec2 (pto2 rec1))
   (punto-contenido? rec2 (pto1 rec1))
   (and
    (or
     (and(< (punto-x (pto1 rec1)) (punto-x (pto1 rec2)))
         (> (punto-x (pto2 rec1)) (punto-x (pto2 rec2))))

     (and(< (punto-x (pto2 rec1)) (punto-x (pto2 rec2)))
         (> (punto-x (pto1 rec1)) (punto-x (pto1 rec2)))))
    (or
     (and(< (punto-y (pto1 rec1)) (punto-y (pto1 rec2)))
         (> (punto-y (pto2 rec1)) (punto-y (pto2 rec2))))

     (and(< (punto-y (pto2 rec1)) (punto-y (pto2 rec2)))
         (> (punto-y (pto1 rec1)) (punto-y (pto1 rec2))))))))

(check-expect (sobrelapan? rectanguloc rectangulod)#t)
(check-expect(sobrelapan? rectangulo4 rectangulo5)#t)
(check-expect(sobrelapan? rectangulo1 rectangulo2)#f)

;-----------------------------------------------------------------------------------------------------------------------------------------
; Contrato: 
; distancia-punto: punto punto -> numero
; Proposito:
; Determinar la distancia entre dos puntos
; ejemplo: (distancia-punto punto1 punto2) deberia de producir #i5.0990195135927845 
(define (distancia-punto punto1 punto2)
  (sqrt (+ (sqr (- (punto-x punto2)
        (punto-x punto1)))
           (sqr (- (punto-y punto2)
        (punto-y punto1))))))

( check-range (distancia-punto punto1 punto2) 5.0 5.1)
(check-range (distancia-punto punto2 punto3) 7.28 7.29)




;--------------------------------------------------------------------------------------------------------------------------------------------
; Contrato:
; Con esta funcion se define el numero mas alto posible
(define minimo +inf.0)
;------------------------------------------------------------------------------------------------
; Punto 12
; Contrato:
; distancia-minima: rectangulo rectangulo-> numero
; Proposito:
; Determinar la distancia minima entre dos rectangulos, teniendo en cuenta
; que si estos se sobrelapan la distancia es 0
; Para esto se necesita determinar la combinacion de puntos que se pueden presentar en los rectangulos
; y luego calcular la distancia entre puntos de cada rectangulo para determinar el minima

;ejemplo: (distancia-minima rectangulo6 rectangulo7) se debe producir 5
; ejemplo: (distancia-minima rectanguloc rectangulod) se debe producir 0
 (define (distancia-minima rec1 rec2)
  (cond 
    [(sobrelapan? rec1 rec2) 0]
    [else (begin 
            (cond
              [(< (distancia-punto (pto1 rec1)
                                   (pto1 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto1 rec1)
                                   (pto1 rec2)))]
              [else minimo])
          
              (cond [(< (distancia-punto (pto1 rec1)
                                   (pto2 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto1 rec1)
                                   (pto2 rec2)))]
                    [else minimo])
              
              (cond[(< (distancia-punto (pto1 rec1)
                                   (pto3 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto1 rec1)
                                   (pto3 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto1 rec1)
                                   (pto4 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto1 rec1)
                                   (pto4 rec2)))]
                   [else minimo])
              
              (cond[(< (distancia-punto (pto2 rec1)
                                   (pto1 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto2 rec1)
                                   (pto1 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto2 rec1)
                                   (pto2 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto2 rec1)
                                   (pto2 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto2 rec1)
                                   (pto3 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto2 rec1)
                                   (pto3 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto2 rec1)
                                   (pto4 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto2 rec1)
                                   (pto4 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto3 rec1)
                                   (pto1 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto3 rec1)
                                   (pto1 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto3 rec1)
                                   (pto2 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto3 rec1)
                                   (pto2 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto3 rec1)
                                   (pto3 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto3 rec1)
                                   (pto3 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto3 rec1)
                                   (pto4 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto3 rec1)
                                   (pto4 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto4 rec1)
                                   (pto1 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto4 rec1)
                                   (pto1 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto4 rec1)
                                   (pto2 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto4 rec1)
                                   (pto2 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto4 rec1)
                                   (pto3 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto4 rec1)
                                   (pto3 rec2)))]
                   [else minimo])
              (cond[(< (distancia-punto (pto4 rec1)
                                   (pto4 rec2)) minimo)
                                        
               (set! minimo (distancia-punto (pto4 rec1)
                                   (pto4 rec2)))]
                   [else minimo])
              
            


            minimo)]))
;pruebas

(check-expect (distancia-minima rectanguloc rectangulod) 0)
(check-expect(distancia-minima rectangulo6 rectangulo7) 5)
(check-within(distancia-minima rectangulo1 rectangulo2) #i2.23606797749979 #i2.2361)
                                                          
;-----------------------------------------------------------------------------------------------
; Punto 13
; Contrato:
; Circulo es una estructura de dos atributos: c y r, siendo
; la primera una subestructuras de punto. Esta estructura representa
; a un circulo, pasandole un punto bidimensional como centro y un valor
; numerico como radio (distancia del centro a un punto del circulo).
(define-struct circulo (c r))
;-------------------------------------------------------------------------------------------------
; Contrato:
; Instancias de la estructura Circulo
(define circulo1 (make-circulo (make-punto 5 5) 3))
(define circulo2 (make-circulo (make-punto 100 100) 50))
(define circulo3 (make-circulo (make-punto 3 5) 2))
;------------------------------------------------------------------------------------------------
; Contrato:
; Se definen el centro y el radio de un circulo
; para reutilizar en ejercicios posteriores
(define (centro c1) (circulo-c c1))
(define (radio c1) (circulo-r c1))
;--------------------------------------------------------------------------------------------------
; Punto 14
; Contrato:
; circ-rec: circulo rectangulo -> boolean
; Proposito: Determinar si un rectangulo se sobrelapa con un circulo
; Para esto se plantean los distintos casos en los cuales se sobrelapan
; se utiliza punto-contenido? como recurso para determinar los casos
; Primero se verifica si el centro del circulo esta contenido en el rectangulo
; si esto no sucede, se verifica que la distancia entre el centro del circulo y cualquier punto del rectangulo
; sea menor al radio, si esto no sucede, se verifica cuando 

(define (circ-rec cr1 w1) (or (punto-contenido? w1 (centro cr1))
                        (< (distancia-punto (centro cr1) (pto1 w1)) (radio cr1))
                        (< (distancia-punto (centro cr1) (pto2 w1)) (radio cr1))
                        (< (distancia-punto (centro cr1) (pto3 w1)) (radio cr1))
                        (< (distancia-punto (centro cr1) (pto4 w1)) (radio cr1))
                        (punto-contenido? w1 (make-punto (- (punto-x (centro cr1)) (radio cr1)) (punto-y (centro cr1))))
                        (punto-contenido? w1 (make-punto (+ (punto-x (centro cr1)) (radio cr1)) (punto-y (centro cr1))))
                        (punto-contenido? w1 (make-punto (- (punto-y (centro cr1)) (radio cr1)) (punto-x (centro cr1))))
                        (punto-contenido? w1 (make-punto (+ (punto-y (centro cr1)) (radio cr1)) (punto-x (centro cr1))))))

;pruebas
(check-expect (circ-rec circulo1 rectangulo1 )#t)
(check-expect(circ-rec circulo1 rectangulo2) #f)
;--------------------------------------------------------------------------------------------------------------------------------
; Contrato:
; coordenada-punto: punto -> coordenada
; Proposito:
; Se convierte un punto en una coordenada para el canvas
; ejemplo:(coordenada-punto punto1) deberia de producir (make-posn (punto-x punto1) (punto-y punto1))
(define (coordenada-punto z) (make-posn (punto-x z) (punto-y z)))
;-----------------------------------------------------------------------------------------------------------------------
; Punto 15
; Contrato:
; dibujar: struct -> void
; Proposito
; Dibujar la figura de entrada en un posn, si es circulo
; o rectangulo
; ejemplo: (dibujar rectangulo8) deberia de dibujar el rectangulo8  en un posn
(define (dibujar a) (begin (start 200 200)
  (cond
    [(rectangulo? a) (draw-solid-rect (coordenada-punto (rectangulo-punto1 a))
                                      (- (punto-x (rectangulo-punto2 a))  (punto-x (rectangulo-punto1 a)))
                                      (- (punto-y (rectangulo-punto2 a))  (punto-y (rectangulo-punto1 a)))
                                       'blue)]
     [(circulo? a) (draw-circle (coordenada-punto(circulo-c a))
                       (circulo-r a) 'red)])))
;pruebas
 (dibujar rectangulo8) 
 (dibujar circulo2) 

;--------------------------------------------------------------------------------------------------------------------------
; Punto 16
; Contrato:
; circulo-escalado-> circulo numero-> circulo
; Proposito:
; escalar un circulo, la funcion aumenta el radio del circulo "c"
; seleccionado en el factor especificado "f"
; ejemplo: (circulo-escalado circulo3 5) deberia de producir (make-circulo (make-punto 3 5) 10),
; un circulo con el mismo centro pero escalado en 5

 (define  (circulo-escalado c f) (make-circulo  (circulo-c c)(* (circulo-r c) f)))

;pruebas
(check-expect (circulo-escalado circulo3 5 )(make-circulo (make-punto 3 5) 10))
(check-expect (circulo-escalado circulo1 3 )(make-circulo (make-punto 5 5) 9))
     


;-------------------------------------------------------------------------------------------------

; Punto 17
; Contrato:
; desplazar-rectangulo: rectangulo numero numero-> rectangulo
; Proposito:
; Desplazar un rectnagulo en el eje x y en el eje y,
; sumandole valores numericos para que se desplace
; ejemplo: (desplazar rectangulo1 2 3)
; debe producir (make-rectangulo (make-punto 7 6)(make-punto 6 11))

(define (desplazar-rectangulo rectangle x y)  (make-rectangulo
                                               (make-punto (+(punto-x (rectangulo-punto1 rectangle)) x)
                                                (+ (punto-y (rectangulo-punto1 rectangle)) y))
                                               (make-punto (+(punto-x (rectangulo-punto2 rectangle)) x)
                                                (+ (punto-y (rectangulo-punto2 rectangle)) y))))

;pruebas
(check-expect(desplazar-rectangulo rectangulo1 2 3)(make-rectangulo(make-punto 7 6) (make-punto 6 11)))
(check-expect(desplazar-rectangulo rectangulo2 1 5)(make-rectangulo(make-punto 3 6) (make-punto 4 5)))
             
 
