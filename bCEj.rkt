#lang racket
(require racket/gui)
(require "lista.rkt")
(require "matriz.rkt")

#| 
  bCEj: inicia loop juego con la matriz de juego (reprensenta las cartas de casa y cada jugador)
  y crea el deck (todas las cartas de un deck de 52) 
    param:
    -num_jugadores: numero de jugadores requerido
|#
(define (bCEj num_jugadores)
  (loopJuego
    (cons
      (matriz (+ num_jugadores 1) 2)
      (cons
        (shuffleDeck
          (crearDeck '(A 2 3 4 5 6 7 8 9 10 J Q K)'(D C T B))
          (random 51)
        )
        '()
      )
    )
  ))

#|
  loopJuego: el loop de juego se divide en deal y comparar para cada jugador
  param: 
    -todas_las_cartas: lista de la forma (matriz_juego, deck)
|#
(define (loopJuego todas_las_cartas)
  (cond
    [(equal? (caaar todas_las_cartas) 0)(loopJuego (deal todas_las_cartas #f 0 0))]
    [else todas_las_cartas]
    ;[else ( newDeal (newColumna matriz_juego) )]
  ))

#|
  deal: le da 2 cartas a la casa y cada jugador al empezar el juego
  param: 
    -todas_las_cartas: lista de la forma (matriz_juego, deck)
|#
(define (deal todas_las_cartas preguntar? fila columna)
  (cond
    [(>= fila (lengthFilas (car todas_las_cartas)))
      todas_las_cartas]
    [(>= columna (lengthColumnas (car todas_las_cartas)))
      (deal todas_las_cartas preguntar? (+ fila 1) 0)]
    [(equal? (elementoMatriz fila columna (car todas_las_cartas)) 0)
      (deal 
        (cons
          (writeElementoMatriz (car todas_las_cartas) fila columna (caadr todas_las_cartas))
          (cons (cdadr todas_las_cartas) '())
        )
        preguntar?
        fila
        (+ columna 1)
      )
    ]
    [else (deal todas_las_cartas preguntar? fila (+ columna 1))]

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

(define (newDeal matriz_juego)
  (cond
    [(null? matriz_juego)'()]
    [else (cons (dealJugador (car matriz_juego))(newDeal (cdr matriz_juego)))]
    ;[else (cons (car matriz_juego)(newDeal (cdr matriz_juego)))]
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

(define (shuffleDeck deck num_random)
  (cond
    [(null? deck)'()]
    [else 
      (if (<= (- (lengthList deck) 2) 0)
        ;then
        (cons 
          (elementoLista 0 deck)
          (shuffleDeck (eliminarElementoLista 0 deck) 0) 
        );else
        (cons 
          (elementoLista num_random deck)
          (shuffleDeck 
           (eliminarElementoLista num_random deck) 
           (random (- (lengthList deck) 2))
          )
        )
      )
    ]
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
|#
(bCEj 1)
