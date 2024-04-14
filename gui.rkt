#lang racket/gui

(define card-width 72)
(define card-height 96)
(define player-panel-height 100)
(define dealer-panel-height 150)
(define button-width 80)
(define button-height 30)

;;crea la ventana del juego
(define ventana (new frame%
                      [label "BlaCEJack"]
                      [width 500]
                      [height 500]))


;;(define secondary-frame (new frame%
                           ;[parent ventana]
                           ;[label "Ventana Secundaria"]
                           ;[width 400]
                           ;[height 300]
                           ;[alignment '(center center)]))



; Crea los paneles para el jugador y el crupier
(define jugador-panel (new panel%
                          [parent ventana]
                          [stretchable-width #t]
                          [min-height player-panel-height]))
(define crupier-panel (new panel%
                          [parent ventana]
                          [stretchable-width #t]
                          [min-height dealer-panel-height]))

; Crea el lienzo para mostrar las cartas
(define jugador-canvas (new canvas%
                           [parent jugador-panel]
                           [paint-callback
                           (lambda (canvas dc)
                             (send dc set-background "dark green")
                             (send dc clear))]
                           [min-width 400]
                           [min-height 300]))
(define crupier-canvas (new canvas%
                           [parent crupier-panel]
                           [paint-callback
                           (lambda (canvas dc)
                             (send dc set-background "dark green")
                             (send dc clear))]
                           [min-width 400]
                           [min-height 150]))

; Crea y muestra los botones
(define carta-button (new button%
                        [parent ventana]
                        [label "Carta"]
                        [min-width button-width]
                        [min-height button-height]))
(define pase-button (new button%
                          [parent ventana]
                          [label "Paso"]
                          [min-width button-width]
                          [min-height button-height]))


(send ventana show #t)
