#!r6rs
(import (rnrs))

(define (f x y z)
  (+ x y z))

(define a (make-list 10000000 (random 10)))
(define b (make-list 10000000 (random 10)))
(define c (make-list 10000000 (random 10)))


(define val
  (time (map f a b c)))

(display (car val))
