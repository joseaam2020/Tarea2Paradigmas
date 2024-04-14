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

#|
  elementroMatriz: devuelve del valor del elemento en la fila y columna indicada 
  param:
    -num_fila: numero de fila en la que se ubica el elemnto
    -num_columna: numero de columna en la que se ubica el elemento
    -matriz: de la cual se obtiene el elemento
|#
(define (elementoMatriz num_fila num_columna matriz)
  (cond
    [(zero? num_fila)(elementoEnIndicaLista num_columna (car matriz))]
    [else (elementoMatriz (- num_fila 1) num_columna (cdr matriz))]
  ))

#|
  writeElementoMatriz: sobreescribe elemento en la fila y columna indicados 
  param: 
    -Matriz: en la que se va a escribir el elemento 
    -fila: numero de fila en la que se ubica el elemnto
    -columna: numero de columna en la que se ubica el elemento
    -nuevo_elemento: elemento a escribir.
|#
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


(define (lengthFilas matriz) (lengthList matriz));devuelve el numero de filas de la matriz indicada
(define (lengthColumnas matriz) (lengthList (car matriz)));devuelve el numero de columnas de la matriz indicada

