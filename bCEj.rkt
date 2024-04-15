#lang racket
(provide 
  deal
  crearDeck
  shuffleDeck
  valorGanador
  puntajes
  bust?
)
(require racket/gui)
(require "lista.rkt")
(require "matriz.rkt")


#|
  deal: le da 2 cartas a la casa y cada jugador al empezar el juego
  param: 
    -todas_las_cartas: lista de la forma (matriz_juego, deck)
    -receptores: lista de todos los receptores de cartas  
    -fila: fila en la cual empezar el deal
    -columna: columna en la cual empezar el deal
    -unica?: bool para saber si se debe entregar solo una carta
|#
(define (deal todas_las_cartas receptores fila columna unica?)
  (cond
    [(>= fila (lengthFilas (car todas_las_cartas)))
      todas_las_cartas]
    [(>= columna (lengthColumnas (car todas_las_cartas)))
      (deal todas_las_cartas receptores (+ fila 1) 0 unica?)]
    [(and (isInLista? fila receptores)(equal? (elementoMatriz fila columna (car todas_las_cartas)) 0))
      (if unica?
        ;then
        (deal 
          (cons
            (writeElementoMatriz (car todas_las_cartas) fila columna (caadr todas_las_cartas))
           (cons (cdadr todas_las_cartas) '())
          )
          (eliminarElementoLista fila receptores)
          fila
          (+ columna 1)
          unica?
        )
        ;else
        (deal 
          (cons
           (writeElementoMatriz (car todas_las_cartas) fila columna (caadr todas_las_cartas))
            (cons (cdadr todas_las_cartas) '())
          )
          receptores
          fila
          (+ columna 1)
          unica?
        )
      )
    ]
    [else (deal todas_las_cartas receptores fila (+ columna 1) unica?)]
  ))

  #|
    crearDeck: crea una lista de listas de la forma 
  ((valor1 tipo1)(valor2 tipo1)...(valor1 tipo2)(valor2 tipo2)...)
  param: 
    -lista_valores: lista con valores
    -lista_tipo: lista con el tipo de cada valor
|#
(define (crearDeck lista_valores lista_tipo)
  (cond
    [(null? lista_tipo)'()]
    [else (append (crearCartas lista_valores (car lista_tipo))(crearDeck lista_valores (cdr lista_tipo)))]
  ))

#|
  crearCartas: crea una lista de la forma 
  ((valor1 tipo)(valor2 tipo)...); aux de crearDeck
  param: 
    -lista_valores: lista con valores
    -tipo: tipo de los valores
|#
(define (crearCartas lista_valores tipo)
  (cond
    [(null? lista_valores)'()]
    [else (cons (cons (car lista_valores) (cons tipo '()))(crearCartas (cdr lista_valores) tipo))]
  ))

#|
  shuffleDeck: recibe un deck y lo revuelve 
  param:
    -deck: lista de cartas en la forma (valor tipo)
    -num_random: utilizando random de racker (random (- largo_lista 1))
|#
(define (shuffleDeck deck num_random)
  (cond
    [(null? deck)'()]
    [else 
      (if (<= (- (lengthList deck) 2) 0)
        ;then
        (cons 
          (elementoEnIndiceLista 0 deck)
          (shuffleDeck (eliminarIndiceLista 0 deck) 0) 
        );else
        (cons 
          (elementoEnIndiceLista num_random deck)
          (shuffleDeck 
           (eliminarIndiceLista num_random deck) 
           (random (- (lengthList deck) 2))
          )
        )
      )
    ]
  ))

(define (valorGanadorAux lista_puntajes valor_ganador)
  (cond [(null? lista_puntajes) valor_ganador]
        [(and (<= (car lista_puntajes) 21) (> (car lista_puntajes) valor_ganador)) (valorGanadorAux (cdr lista_puntajes) (car lista_puntajes))]
        [else (valorGanadorAux (cdr lista_puntajes) valor_ganador)]
        ))

#|
  valorGanador: para obtener el valor mas cercano a 21 y que sea menor a el de todos
  los puntajes
  param: 
    -lista_puntajes: una lista con los puntajes de la casas y los jugadores en orden
|#
(define (valorGanador lista_puntajes)
  (cond [(> (car lista_puntajes) 21) (valorGanador (cdr lista_puntajes))]
        [else (valorGanadorAux (cdr lista_puntajes) (car lista_puntajes))]
        ))

#|
  puntajes: calcula los puntajes de todos los jugadores y los devuelve como una lista 
  param:
    -matriz: matriz_de_juego, todos los jugadores 
|#
(define (puntajes matriz)
  (cond [(null? matriz) '()]
        [(bust? (car matriz)) (cons (puntaje (cambia_aces(car matriz))) (puntajes (cdr matriz)))]
        [else (cons (puntaje (car matriz)) (puntajes (cdr matriz)))]
        ))

#|
  puntaje: calcula el puntaje de una jugador (valor numerico)
  param:
    -lista: de cartas del jugador 
|#
(define (puntaje lista)
  (cond
    [(null? lista) 0]
    [(equal? (car lista) 0)(+ 0 (puntaje(cdr lista)))]
    [(equal? (caar lista) 0) (+ 0 (puntaje (cdr lista)))]
    [(equal? (caar lista) 'A) (+ 11 (puntaje (cdr lista)))]
    [(equal? (caar lista) 'J) (+ 10 (puntaje (cdr lista)))]
    [(equal? (caar lista) 'Q) (+ 10 (puntaje (cdr lista)))]
    [(equal? (caar lista) 'K) (+ 10 (puntaje (cdr lista)))]
    [else (+ (caar lista) (puntaje (cdr lista)))]
   ))

#|
  bust?: boolean para saber si un jugador se paso de 21
  param:
    -lista: de todas las cartas de un jugador
|#
(define (bust? lista)
  (cond
    [(> (puntaje lista) 21) #t]
    [else #f]
   ))

#|
  cambia_aces: una funcion que determina si los valores de los A's
  en una lista de cartas de jugador deben ser 1 o 11
  param: 
    -lista: de cartas de un jugador 
|#
(define (cambia_aces lista)
  (cond
    [(null? lista) '()]
    [(equal? (car lista) 0)(cons (car lista)(cambia_aces (cdr lista)))]
    [(equal? 'A (caar lista)) (cons (cons 1 (cdar lista)) (cambia_aces (cdr lista)))]
    [else (cons (car lista) (cambia_aces (cdr lista)))]
   ))
