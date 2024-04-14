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
(define world-width 800)

(define (carta valor tipo width height)
  (overlay/align "right" "bottom"
    (above
      (text (~a tipo) 18 "black")
      (text (~a valor) 18 "black")
    )
    (overlay/align "left" "top"
      (above
        (text (~a valor) 18 "black")
        (text (~a tipo) 18 "black")
      )
      (rectangle  width height "solid" "white")
    )
  ))

(define (printCartas cartas x y width height num_cartas)
  (cond
    [(null? cartas)(rectangle width height "solid" bg-color)]
    [(equal? (car cartas) 0)
      (printCartas (cdr cartas) x y width height num_cartas)]
    [else 
      (place-image
        (carta
          (caar cartas)
          (cadar cartas)
          (- (/ width num_cartas) 10)
          (- height 60)
        )
        x y
        (printCartas (cdr cartas) (+ (+ x 10) (- (/ width num_cartas) 10)) y width height num_cartas)
      )
    ]
  ))

(define (jugador numero width height cartas num_cartas)
  (overlay/align "center" "top"
    (if (zero? numero)
    ;then 
    (overlay
        (text (string-append "Casa:") 12 "white")
        (rectangle (- width 20) (- height 180) "solid" "gray") 
      ) 
    ;else 
    (overlay
        (text (string-append "Jugador: " (~a numero)) 12 "white")
        (rectangle (- width 20) (- height 180) "solid" "gray") 
      ))
    (printCartas cartas (+ (/ (- (/ width num_cartas) 10) 2) 5) 100 width height num_cartas)
  ))

(define (jugadores height_mesa width_mesa cartas_jugadores num_jugadores)
  (cond
    [(null? cartas_jugadores)(rectangle width_mesa height_mesa "solid" bg-color)]
    [else 
      (place-image
        (jugador 
          (+ 1 (- num_jugadores (lengthList cartas_jugadores)))
          (/ width_mesa num_jugadores)
          (/ height_mesa 3)
          (car cartas_jugadores)
          (lengthList (car cartas_jugadores))
        )
        (+ (/ (/ width_mesa num_jugadores) 2)(* (/ width_mesa num_jugadores) (- num_jugadores (lengthList cartas_jugadores)))) 
        (+ (/ height_mesa 3)(/ (/ height_mesa 3) 2))
        (jugadores
          height_mesa
          width_mesa
          (cdr cartas_jugadores)
          num_jugadores
        )
      )
    ]
  ))


(define (mesa height width todas_las_cartas)
  (overlay/align "center" "top"
    (jugador 0 300 200 (caar todas_las_cartas) (lengthList (caar todas_las_cartas)))
    (jugadores height width (cdar todas_las_cartas) (lengthList (cdar todas_las_cartas))) 
  ))
  
(define (draw-world todas_las_cartas)
  (overlay
      (mesa world-height world-width todas_las_cartas)
      (empty-scene world-width world-height)
    )
  )

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