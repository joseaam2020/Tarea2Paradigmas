#lang racket 
(provide 
    elementoLista
    eliminarElementoLista
    eliminarIndiceLista
    crearListaCero
    crearListaAscendente
    lengthList
    writeElementoLista
    isInLista?
)

#|
  elementoLista: devuelve el elemento de la lista en la posicion indicada
  param:
    -num_elemento: indice del elemento requerido (0 - (longitud lista -1)
    -lista: lista de la que se requiere un elemento
|#
(define (elementoLista num_elemento lista)
  (cond
    [(zero? num_elemento)(car lista)]
    [else (elementoLista (- num_elemento 1) (cdr lista))]
  ))

(define (eliminarElementoLista elemento lista)
  (cond
    [(null? lista)'()]
    [(equal? elemento (car lista))(cdr lista)]
    [else (cons (car lista) (eliminarElementoLista elemento (cdr lista)))]
  ))
#|
  eliminarIndiceLista: elimina elemento en la posicion indicada 
  param:
    -num_elemento: indice numerico del elemento en la lista que se va a eliminar
    -lista: de elementos
|#
(define (eliminarIndiceLista indice lista)
  (cond
    [(null? lista)'()]
    [(zero? indice)(eliminarIndiceLista (- indice 1)(cdr lista))]
    [else (append 
            (cons (car lista) '())
            (eliminarIndiceLista (- indice 1)(cdr lista))
          )
    ]
  ))

#|
  writeElementoLista: sobreescribe elemento en el indice indicado 
  param: 
    -lista: en la que se va a escribir el elemento 
    -indice: posicion de la lista donde escribir el elemento
    -nuevo_elemento: elemento a escribir.
|#
(define (writeElementoLista lista indice nuevo_elemento)
  (cond
    [(null? lista)'()]
    [(zero? indice)
      (cons 
        nuevo_elemento 
        (writeElementoLista (cdr lista) (- indice 1) nuevo_elemento)
      )
    ]
    [else 
      (cons
        (car lista)
        (writeElementoLista (cdr lista) (- indice 1) nuevo_elemento)
      )
    ]
  )
)


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
  crearListaAscendente: genera una lista con valores 
  numericos ascendentes hasta llegar al numero de elementos indicado  
  -param: 
    num_elementos: valor numerico de elementos en la lista 
|#
(define (crearListaAscendente num_elementos)
  (cond
    [(zero? num_elementos) (cons 0 '())]
    [else 
      (append  
       (crearListaAscendente (- num_elementos 1)) 
       (cons num_elementos '())
      )
    ]
  ))

#|
  lengthLista: devuelve valor numerico de cuantos elementos hay en la lista
  param:
    -lista: para medir
|#
(define (lengthList lista)
  (cond
    [(null? lista) 0]
    [else (+ 1 (lengthList (cdr lista)))]
  ))

(define (isInLista? elemento lista)
  (cond
    [(null? lista) #f]
    [(equal? (car lista) elemento) #t]
    [else (isInLista? elemento (cdr lista))]
  ))
