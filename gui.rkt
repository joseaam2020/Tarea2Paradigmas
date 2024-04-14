#lang racket/gui

(define card-width 72)
(define card-height 96)
(define player-panel-height 100)
(define dealer-panel-height 150)
(define button-width 80)
(define button-height 30)
(define cartas '((A 2 3 4 5 6 7 8 9 10 J Q K) (D C T B)))

(define (crearDeck valores palos)
  (define (crear mazo valores palos)
    (cond
      [(or (null? valores) (null? palos)) mazo]
      [else (crear (append mazo (list (list (car valores) (car palos))))
                   (cdr valores)
                   (cdr palos))]))
  (crear '() valores palos))

(define (generar-carta)
  (define valores '(A 2 3 4 5 6 7 8 9 10 J Q K))
  (define palos '(D C T B))
  (define valor (list-ref valores (random (length valores))))
  (define palo (list-ref palos (random (length palos))))
  (list valor palo))

(define ventanaPrin (new frame%
                   [label "BlaCEJack"]
                   [width 400]
                   [height 300]
                   [alignment '(center center)]))
(define msg1 (new message%
                  [parent ventanaPrin]
                  [label "Bienvenidos"]))
(define msg2 (new message%
                  [parent ventanaPrin]
                  [label "Escoja la cantidad de jugadores"]))

(define boton1 (new button%
                    [parent ventanaPrin]
                    [label "GO!!"]
                    [callback (lambda (button event)
                                (send ventana show #t)
                                (send ventanaPrin show #f))]))
                    
;;crea la ventana del juego
(define ventana (new frame%
                     [parent ventanaPrin]
                     [label "BlaCEJack"]
                     [width 500]
                     [height 500]))



; Crea los paneles para el jugador y el crupier
(define jugador-panel (new panel%
                          [parent ventana]
                          [stretchable-width #t]
                          [min-height player-panel-height]))
(define crupier-panel (new panel%
                          [parent ventana]
                          [stretchable-width #t]
                          [min-height dealer-panel-height]))
;;boton de jugar y crear las primeras cartas
(define jugar-button (new button%
                          [parent ventana]
                          [label "Jugar"]
                          [callback (lambda (button event)
                                      (send jugador-canvas refresh))]
                          [min-width button-width]
                          [min-height button-height]))

; Crea el lienzo para mostrar las cartas
(define jugador-canvas (new canvas%
                           [parent jugador-panel]
                           [paint-callback
                           (lambda (canvas dc)
                             (send dc set-background "dark green")
                             (define carta (generar-carta))
                             (define valor (first carta))
                             (define palo (second carta))
                             (define texto(format "~a~a" valor palo))
                             (send dc set-font (make-object font% 12 'default 'normal 'normal))
                             (send dc draw-text texto 10 50)
                             (send dc clear))]
                           [min-width 400]
                           [min-height 300]))
;;canvas del crupier
(define crupier-canvas (new canvas%
                           [parent crupier-panel]
                           [paint-callback
                           (lambda (canvas dc)
                             (send dc set-background "dark green")
                             (send dc clear))]
                           [min-width 400]
                           [min-height 150]))

;; Crea e boton de pedir carta
(define carta-button (new button%
                        [parent ventana]
                        [label "Carta"]
                        [min-width button-width]
                        [min-height button-height]))
;;crea el boton de pasar turno
(define pase-button (new button%
                          [parent ventana]
                          [label "Paso"]
                          [min-width button-width]
                          [min-height button-height]))


(send ventanaPrin show #t)
