#lang racket


#| 
  bCEj: inicializa juego con el numero de jugadores indicado
  param:
    -num_jugadores: numero de jugadores requerido
|#
(define (bCEj num_jugadores)
  (loopJuego (matriz (+ num_jugadores 1) 2))
  )

#|
  loopJuego: el loop de juego se divide en deal y comparar para cada jugador
  param: 
    -matriz_juego: lista con filas para jugador mas casa y columnas para cada carta en posesion
|#
(define (loopJuego matriz_juego)
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
    [else (cons (dealJugador (car matriz_juego))(deal (cdr matriz_juego)))]
  ))

#|
  dealJugador: reemplaza los ceros en la lista de cartas del jugador con cartas en la forma (valor tipo)
  param:
    -lista_jugador: lista con las cartas del jugador
|#
(define (dealJugador lista_jugador)
  (cond
    [(null? lista_jugador)'()]
    [(cons (cartaRandom)(dealJugador (cdr lista_jugador)))]
  ))

#|
  cartaRandom: genera una carta con un valor aleatorio y un tipo aleatorio.
  Los valores puedes ser  (A 1 2 3 4 5 6 7 8 9 10 J Q K) 
  y los tipos (D(iamante), C(orazon),T(rebol),B(astos))
|#
(define (cartaRandom)
  (cons (elementoLista (random 12) '(A 2 3 4 5 6 7 8 9 10 J Q K))(cons (elementoLista (random 3) '(D C T B))'()))
  )

#|
  elementoLista: devuelve el elemento de la lista en la posicion indicada
  param:
    -num_elemento: indice del elemento requerido (0 - (longitud lista -1)
    -lista: lista de la que se requiere un elemento
|#
(define (elementoLista num_elemento lista )
  (cond
    [(zero? num_elemento)(car lista)]
    [else (elementoLista (- num_elemento 1) (cdr lista))]
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
    [else (cons (crearLista num_columnas) (matriz (- num_filas 1) num_columnas))]
  ))

#|
  crearLista: crea una lista con elementos 0 de la longitud especificada
  param:
    -longitud: longitud de la lista
|#
(define (crearLista longitud)
  (cond
    [(equal? longitud 0)'()]
    [else (cons 0  (crearLista (- longitud 1)))]
  ))

(bCEj 3)