#lang racket
(provide
    matriz
    newColumna
    elementoMatriz
    writeElementoMatriz
    lengthFilas
    lengthColumnas
)
(require "lista.rkt")

#|
  matriz: genera una matriz de ciertas filas y columnas
  param:
    -num_filas: el numero de filas de la matriz
    -num_columnas: el numero de columnas de la matriz
|#
(define (matriz num_filas num_columnas)
  (cond
    [(equal? num_filas 0)'()]
    [else (cons (crearListaCero num_columnas) (matriz (- num_filas 1) num_columnas))]
  ))

#|
    newColumna: inserta 0 en la matriz para crear una nueva columna
    param:
        -matriz: patriz a la cual insertar la nueva columna
|#
(define (newColumna matriz)
  (cond
    [(null? matriz)'()] 
    [else (cons (append (car matriz)'(0)) (newColumna (cdr matriz)))]
  )
  )

(define (elementoMatriz num_fila num_columna matriz)
  (cond
    [(zero? num_fila)(elementoLista num_columna (car matriz))]
    [else (elementoMatriz (- num_fila 1) num_columna (cdr matriz))]
  ))

(define (writeElementoMatriz matriz fila columna nuevo_elemento)
  (cond
    [(null? matriz) '()]
    [(zero? fila)
      (cons 
        (writeElementoLista (car matriz) columna nuevo_elemento)
        (writeElementoMatriz (cdr matriz) (- fila 1) columna nuevo_elemento)
      )
    ]
    [else
      (cons
        (car matriz)
        (writeElementoMatriz (cdr matriz) (- fila 1) columna nuevo_elemento)
      )
    ]
  ))

(define (lengthFilas matriz) (lengthList matriz))
(define (lengthColumnas matriz) (lengthList (car matriz)))

