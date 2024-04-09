#lang racket

(define (bCEj num_jugadores)
  (loop_juego (matriz (+ num_jugadores 1) 2))
  )

(define (loop_juego matriz_juego)
  (deal matriz_juego)
  )

(define (deal matriz_juego)
  (cond
    [(null? matriz_juego)'()]
    [else (cons (deal_jugador (car matriz_juego))(deal (cdr matriz_juego)))]
  ))

(define (deal_jugador lista_jugador)
  (cond
    [(null? lista_jugador)'()]
    [(cons (carta_random)(deal_jugador (cdr lista_jugador)))]
  ))

(define (carta_random)
  (cons (elemento_lista (random 12) '(A 2 3 4 5 6 7 8 9 10 J Q K))(cons (elemento_lista (random 3) '(D C T B))'()))
  )

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

(define (matriz num_filas num_columnas)
  (cond
    [(equal? num_filas 0)'()]
    [else (cons (crear_lista num_columnas) (matriz (- num_filas 1) num_columnas))]
  ))

(define (crear_lista num)
  (cond
    [(equal? num 0)'()]
    [else (cons 0  (crear_lista (- num 1)))]
  ))

(define (matriz_cuadrada num_filas_columnas)
  (matriz num_filas_columnas num_filas_columnas)
  )

( elemento_lista (random 12) '(1 2 3 4 5 6 7 8 9 10 11 12 13))
(bCEj 3)