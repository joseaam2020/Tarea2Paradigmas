#lang racket/base
(define (puntaje lista)
  (cond ((null? lista) 0)
        (else (+ (car lista) (puntaje (cdr lista))))))

(define (bust? lista)
  (cond ((> (puntaje lista) 21) #t)
        (else #f)))

(define (cambia_aces lista)
  (cond ((null? lista) lista)
        ((equal? 11 (car lista)) (cons 1 (cdr lista)))
        (else (cons (car lista) (cambia_aces (cdr lista))))))
