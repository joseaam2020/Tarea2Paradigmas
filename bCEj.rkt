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
    [(equal? (caaar todas_las_cartas) 0)
      (loopJuego  
        (deal 
          todas_las_cartas 
         ; (crearListaAscendente (lengthFilas (car todas_las_cartas))) 
          '(0)
          0 
          0
          #t
        )
      )
    ]
    [else todas_las_cartas]
  ))

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
          (elementoLista 0 deck)
          (shuffleDeck (eliminarIndiceLista 0 deck) 0) 
        );else
        (cons 
          (elementoLista num_random deck)
          (shuffleDeck 
           (eliminarIndiceLista num_random deck) 
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
