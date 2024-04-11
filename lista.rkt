#lang racket 
(provide 
    elementoLista
    crearListaCero
    crearListaRandom
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

#|
  crearListaCero: crea una lista con elementos 0 de la longitud especificada
  param:
    -longitud: longitud de la lista
|#
(define (crearListaCero longitud)
  (cond
    [(equal? longitud 0)'()]
    [else (cons 0  (crearListaCero (- longitud 1)))]
  ))

#|
  cartaRandom: genera una carta con un valor aleatorio y un tipo aleatorio.
  Los valores puedes ser  (A 1 2 3 4 5 6 7 8 9 10 J Q K) 
  y los tipos (D(iamante), C(orazon),T(rebol),B(astos))
|#
(define (crearListaRandom num_elementos valor_maximo)
  (cond
  [(zero? num_elementos)'()]
  [else (cons (random valor_maximo) (crearListaRandom (- num_elementos -1) valor_maximo))]
  ))