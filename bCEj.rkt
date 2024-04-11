#lang racket
(require racket/gui)
(require "matriz.rkt")

#| 
  bCEj: inicializa juego con el numero de jugadores indicado
  param:
    -num_jugadores: numero de jugadores requerido
|#
(define (bCEj num_jugadores)
  (loopJuego
    (matriz (+ num_jugadores 1) 2)
    (crearDeck '(A 2 3 4 5 6 7 8 9 10 J Q K)'(D C T B))
  ))

#|
  loopJuego: el loop de juego se divide en deal y comparar para cada jugador
  param: 
    -matriz_juego: lista con filas para jugador mas casa y columnas para cada carta en posesion
    -lista_deck: lista con todas las cartas del deck 
|#
(define (loopJuego matriz_juego lista_deck)
  (cond
    [(equal? (caar matriz_juego) 0)(loopJuego (deal matriz_juego) lista_deck)]
    [else ( newDeal (newColumna matriz_juego) )]
  ))

#|
  deal: le da 2 cartas a la casa y cada jugador al empezar el juego
  param: 
    -matriz_juego: lista con filas para jugador mas casa y columnas para cada carta en posesion
|#
(define (deal matriz_juego)
  (cond
    [(null? matriz_juego)'()]
    [else (cons (dealJugador (car matriz_juego))(deal (cdr matriz_juego)))]
  ))

(define (newDeal matriz_juego)
  (cond
    [(null? matriz_juego)'()]
    [else (cons (dealJugador (car matriz_juego))(newDeal (cdr matriz_juego)))]
    ;[else (cons (car matriz_juego)(newDeal (cdr matriz_juego)))]
  ))

#|
  dealJugador: reemplaza los ceros en la lista de cartas del jugador con cartas en la forma (valor tipo)
  param:
    -lista_jugador: lista con las cartas del jugador
|#
(define (dealJugador lista_jugador)
  (cond
    [(null? lista_jugador)'()]
    ;[(equal? (car lista_jugador) 0)(cons (cartaRandom)(dealJugador (cdr lista_jugador)))]
    [else (cons (car lista_jugador)(dealJugador (cdr lista_jugador))) ]
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

(define (puntaje lista)
  (cond
    [(null? lista) 0]
    [else (+ (caar lista) (puntaje (cdr lista)))]
   ))

(define (bust? lista)
  (cond
    [(> (puntaje lista) 21) #t]
    [else #f]
   ))

(define (cambia_aces lista)
  (cond
    [(null? lista) lista]
    [(equal? 11 (car lista)) (cons 1 (cdr lista))]
    [else (cons (car lista) (cambia_aces (cdr lista)))]
   ))

#|
(println "Escriba el número de jugadores:")
(define jugadores (read-line))
(define num (string->number jugadores))
(bCEj num)
|#