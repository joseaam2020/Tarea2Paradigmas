#lang racket 

(require racket 2htdp/image)
(require racket 2htdp/universe)
(require "lista.rkt")
(require "matriz.rkt")
(require "bCEj.rkt")

(define carta-height 100)
(define carta-width 80)
(define bg-color (make-color 16 161 50))
(define world-height 600)
(define world-width 600)

(define numero 1)
  (place-image 
    (overlay
    
    )
    0 0
    () 
    (rectangle 300 200 "solid" bg-color)
  )

(define (carta valor tipo)
  (overlay/align "right" "bottom"
    (above
      (text (~a tipo) 18 "black")
      (text (~a valor) 18 "black")
    )
    (overlay/align "left" "top"
      (above
        (text (~a tipo) 18 "black")
        (text (~a valor) 18 "black")
      )
      (rectangle  carta-width carta-height "solid" "white")
    )
  ))

(define (mesa height width color)
  (rectangle height width "solid" color)
  )
  
(define (draw-world todas_las_cartas)
  (place-image 
    (carta (caaaar todas_las_cartas)(car (cdaaar todas_las_cartas)))
    (+ carta-width 10) (+ carta-height 10)
    (overlay
      (mesa world-height world-width bg-color)
      (empty-scene world-width world-height)
    )
  ))

(define (initialWorld num_jugadores) 
  (deal
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
    (crearListaAscendente num_jugadores)
    0 0 #f
  ))

(define (bCEj num_jugadores)
  (big-bang (initialWorld num_jugadores)
    [to-draw draw-world]
))