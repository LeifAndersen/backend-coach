#!r6rs

(import (rnrs base))

(define (split-vals ls)
  (if (or (null? ls) (null? (cdr ls)))
      (values ls '())
      (let-values ([(o e) (split-vals (cddr ls))])
        (values (cons (car ls) o)
                (cons (cadr ls) e)))))

(define (split-set/rev ls)
  (define o '())
  (define e '())
  (let loop ([ls ls])
    (cond [(null? ls)
           (values (reverse o) (reverse e))]
          [(null? (cdr ls))
           (values (reverse (cons (car ls) o)) (reverse e))]
          [else
           (set! o (cons (car ls) o))
           (set! e (cons (cadr ls) e))
           (loop (cddr ls))])))

(define (split-set ls)
  (define o '())
  (define e '())
  (let loop ([ls ls])
    (cond [(or (null? ls) (null? (cdr ls)))
           (set! o ls)
           (set! e '())]
          [else
           (loop (cddr ls))
           (set! o (cons (car ls) o))
           (set! e (cons (cadr ls) e))]))
  (values o e))

(define the-list-size 1000)
(define the-list
  (let loop ([lst '()]
             [count (- the-list-size 1)])
    (if (< count 0)
        lst
        (loop lst (cons count lst) (- count 1)))))
