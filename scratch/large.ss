#!r6rs
(import (rnrs))

(define (f x y z u v w)
  (+ x y z))

(define a (make-list 10000000 (random 10)))
(define b (make-list 10000000 (random 10)))
(define c (make-list 10000000 (random 10)))
(define d (make-list 10000000 (random 10)))
(define e (make-list 10000000 (random 10)))
(define g (make-list 10000000 (random 10)))

(define val
  (time (map f a b c d e g)))

(display (car val))
