#lang racket

#| 
  bCEj: inicializa juego con el numero de jugadores indicado
  param:
    -num_jugadores: numero de jugadores requerido
|#
(define (bCEj num_jugadores)
  (loop_juego (matriz (+ num_jugadores 1) 2))
  )

#|
  loop_juego: el loop de juego se divide en deal y comparar para cada jugador
  param: 
    -matriz_juego: lista con filas para jugador mas casa y columnas para cada carta en posesion
|#
(define (loop_juego matriz_juego)
  (deal matriz_juego)

  )

#|
  deal: le da 2 cartas a la casa y cada jugador al empezar el juego
  param: 
    -matriz_juego: lista con filas para jugador mas casa y columnas para cada carta en posesion
|#
(define (deal matriz_juego)
  (cond
    [(null? matriz_juego)'()]
    [else (cons (deal_jugador (car matriz_juego))(deal (cdr matriz_juego)))]
  ))

#|
  deal_jugador: reemplaza los ceros en la lista de cartas del jugador con cartas en la forma (valor tipo)
  param:
    -lista_jugador: lista con las cartas del jugador
|#
(define (deal_jugador lista_jugador)
  (cond
    [(null? lista_jugador)'()]
    [(cons (carta_random)(deal_jugador (cdr lista_jugador)))]
  ))

#|
  carta_random: genera una carta con un valor aleatorio y un tipo aleatorio.
  Los valores puedes ser  (A 1 2 3 4 5 6 7 8 9 10 J Q K) 
  y los tipos (D(iamante), C(orazon),T(rebol),B(astos))
|#
(define (carta_random)
  (cons (elemento_lista (random 12) '(A 2 3 4 5 6 7 8 9 10 J Q K))(cons (elemento_lista (random 3) '(D C T B))'()))
  )

#|
  elemento_lista: devuelve el elemento de la lista en la posicion indicada
  param:
    -num_elemento: indice del elemento requerido (0 - (longitud lista -1)
    -lista: lista de la que se requiere un elemento
|#
(define (elemento_lista num_elemento lista )
  (cond
    [(zero? num_elemento)(car lista)]
    [else (elemento_lista (- num_elemento 1) (cdr lista))]
  ))
  
(define (puntaje lista)
  (cond
    [(null? lista) 0]
    [else (+ (car lista) (puntaje (cdr lista)))]
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
  matriz: genera una matriz de ciertas filas y columnas
  param:
    -num_filas: el numero de filas de la matriz
    -num_columnas: el numero de columnas de la matriz
|#
(define (matriz num_filas num_columnas)
  (cond
    [(equal? num_filas 0)'()]
    [else (cons (crear_lista num_columnas) (matriz (- num_filas 1) num_columnas))]
  ))

#|
  crear_lista: crea una lista con elementos 0 de la longitud especificada
  param:
    -longitud: longitud de la lista
|#
(define (crear_lista longitud)
  (cond
    [(equal? longitud 0)'()]
    [else (cons 0  (crear_lista (- longitud 1)))]
  ))

(bCEj 3)