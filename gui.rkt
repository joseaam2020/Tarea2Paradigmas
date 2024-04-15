#lang racket 

(require racket 2htdp/image)
(require racket 2htdp/universe)
(require "lista.rkt")
(require "matriz.rkt")
(require "bCEj.rkt")

(define carta-height 180)
(define carta-width 120) 
(define back-carta (make-color 194 56 17))
(define bg-color (make-color 16 161 50))
(define world-height 600)
(define world-width 800)

(define botones
  (place-image/align 
  (overlay
    (text "Pasar" 12 "black")
    (overlay
      (rectangle (/ world-width 2) (/ world-height 12) "outline" "black")
      (rectangle (/ world-width 2) (/ world-height 12) "solid" "white")
    ))
  40 16 "left" "top" 
  (place-image/align
    (overlay
      (text "Carta" 12 "black")
      (overlay
       (rectangle (/ world-width 2) (/ world-height 12) "outline" "black")
       (rectangle (/ world-width 2) (/ world-height 12) "solid" "white")
      ))
    40 82 "left" "top"
    (place-image/align
      (overlay
        (text "Reiniciar" 12 "black")
        (overlay
          (rectangle (/ world-width 2) (/ world-height 12) "outline" "black")
          (rectangle (/ world-width 2) (/ world-height 12) "solid" "white")
        )
      )
      40 148 "left" "top"
      (rectangle world-width (/ world-height 3) "solid" bg-color)
    )
  )
))
  
(define (handle-mouse world x y button)
  (if (string=? button "button-down")
      ;then
      (cond
        ;Si presion boton pasar
        [(and 
          (<= x (+ 40 (/ world-width 2)))
          (<= y (+ 416 (/ world-height 12))) 
          (>= x 40) (>= y 416))
         (if (>= (elementoEnIndiceLista 2 world)(-(lengthList (car world)) 1))
            ;then
            (append 
              (eliminarIndiceLista 2 world)
              (cons 
                (string-append 
                  "Ganadores: " 
                  (~a (elementoLista (valorGanador (puntajes (car world)))(puntajes (car world)) 0)))
                '()
              )
            )
            ;else
            (append 
              (eliminarIndiceLista 2 world)
              (cons (+ (elementoEnIndiceLista 2 world) 1) '())
            )
          )
        ]

        ;Si presiona boton Carta 
        [(and
          (<= x (+ 40 (/ world-width 2)))
          (<= y (+ 482 (/ world-height 12)))
          (>= x 40) (>= y 482))
          (if (isInLista? 0 (elementoEnIndiceLista (elementoEnIndiceLista 2 world) (car world)))
          ;then
            (append 
              (deal 
                (cons (car world)(cons (cadr world)'()))
                (cons (elementoEnIndiceLista 2 world) '()) 
                0 0 #t
              ) 
              (cons (elementoEnIndiceLista 2 world) '())
            )
            (append 
              (deal 
                (cons (newColumna (car world)) (cons (cadr world)  '()))
                (cons (elementoEnIndiceLista 2 world) '()) 
                0 0 #t
              )
              (cons (elementoEnIndiceLista 2 world) '())
            ))]

        ;Si presiona boton reiniciar
        [(and (<= x (+ 40 (/ world-width 2))) (<= y (+ 548 (/ world-height 12))) (>= x 40) (>= y 548)) (initialWorld (- (lengthList (car world)) 1) 1)]

        [else world])
      world))

(define (maso num_cartas x y)
  (cond
    [(<= num_cartas 0)(rectangle (+ carta-height 24) (+ carta-width 24) "solid" bg-color)] 
    [(place-image
      (overlay
        (rectangle carta-width carta-height "outline" "black")
        (rectangle carta-width carta-height "solid" back-carta)
      )
      x y
      (maso (- num_cartas 4) (+ x 2) y)
    )
  ]
  ))

(define (carta valor tipo width height bocaArriba? casa?)
  (if bocaArriba?
    ;then
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
    )
    ;else
    (if casa?
      ;then    
      (rectangle  width height "solid" back-carta)
      ;else
      (overlay/align "right" "bottom"
        (above
          (text (~a tipo) 18 "white")
          (text (~a valor) 18 "white")
        )
        (overlay/align "left" "top"
          (above
            (text (~a valor) 18 "white")
            (text (~a tipo) 18 "white")
          )
          (rectangle  width height "solid" back-carta)
        )
      ) 
    )
  ))

(define (printCartas cartas x y width height num_cartas casa?)
  (cond
    [(null? cartas)(rectangle width height "solid" bg-color)]
    [(equal? (car cartas) 0)
      (printCartas (cdr cartas) x y width height num_cartas casa?)]
    [else
      (if (>= 0 (- num_cartas (lengthList cartas)))
        ;then
        (place-image
          (carta
            (caar cartas)
            (cadar cartas)
            (- (/ width num_cartas) 10)
            (- height 60)
            #t
            casa?
          )
          x y
          (printCartas 
            (cdr cartas) 
            (+ (+ x 10) (- (/ width num_cartas) 10)) 
            y width height num_cartas casa?
          )
        )
        ;else
        (place-image
          (carta
            (caar cartas)
            (cadar cartas)
            (- (/ width num_cartas) 10)
            (- height 60)
            #f
            casa?
          )
          x y
          (printCartas 
          (cdr cartas) 
          (+ (+ x 10) (- (/ width num_cartas) 10))
          y width height num_cartas casa?)
        )
      ) 
    ]
  ))

(define (jugador numero width height cartas num_cartas)
    (if (zero? numero)
    ;then 
    (overlay/align "center" "top"
      (overlay
        (text (string-append "Casa:") 12 "white")
        (rectangle (- width 20) (- height 180) "solid" "gray") 
      ) 
      (printCartas
        cartas 
        (+ (/ (- (/ width num_cartas) 10) 2) 5) 
        100 width height num_cartas #t
      )
    )
    ;else 
    (overlay/align "center" "top"
      (overlay
        (text (string-append "Jugador: " (~a numero)) 12 "white")
        (rectangle (- width 20) (- height 180) "solid" "gray") 
      )
      (printCartas
        cartas 
        (+ (/ (- (/ width num_cartas) 10) 2) 5) 
        100 width height num_cartas #f
      )
    )
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
  (place-image/align 
    botones 
    0 400 "left" "top"
    (overlay/align "center" "top"
      (jugador 0 300 200 (caar todas_las_cartas) (lengthList (caar todas_las_cartas)))
      (jugadores height width (cdar todas_las_cartas) (lengthList (cdar todas_las_cartas))) 
  )))
  
(define (draw-world todas_las_cartas)
  (overlay/align "right" "bottom" 
    (text (~a (caddr todas_las_cartas)) 12 "black")
    (place-image
      (maso (lengthList (cadr todas_las_cartas)) (/ carta-width 2)(/ carta-height 2))
      (+ (/ carta-width 2) 80) (/ carta-height 2) 
      (overlay
        (mesa world-height world-width todas_las_cartas)
        (empty-scene world-width world-height)
      )
  )))

(define (handle-tick world)
  (if (<= (elementoEnIndiceLista 0 (puntajes (car world))) 16)
    ;then 
    (append 
      (deal 
        (cons (newColumna (car world)) (cons (cadr world)  '()))
        (cons 0 '()) 
        0 0 #t
      )
      (cons (elementoEnIndiceLista 2 world) '())
    )
    ;else
    world
  )
  )
(define (initialWorld num_jugadores string) 
  (append (deal
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
  )(cons string '())))

(define (bCEj num_jugadores)
  (big-bang (initialWorld num_jugadores 1)
    [to-draw draw-world]
    [on-tick handle-tick]
    [on-mouse handle-mouse]
))
(bCEj 2)