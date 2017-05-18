#!r6rs

;; Timing example taken from:
;; An Efficient Implementation of Multiple Return Values

(import (rnrs))

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

(define the-list-size 10000)
(define the-list
  (let loop ([lst '()]
             [count (- the-list-size 1)])
    (if (< count 0)
        lst
        (loop (cons count lst) (- count 1)))))

(define (print-split func)
  (let-values ([(o e) (func the-list)])
    (display o)
    (newline)
    (display e)
    (newline)))

(let-values ([(o1 e1) (split-vals the-list)]
             [(o2 e2) (split-set/rev the-list)]
             [(o3 e3) (split-set the-list)])
  (if (not (and (equal? o1 o2)
                (equal? o1 o3)
                (equal? e1 e2)
                (equal? e1 e3)))
      (error "Bad Implementation")))

(define run-count 10000)
(define (time-split func)
  (time (let loop ([count 0])
          (func the-list)
          (if (< count run-count)
              (loop (+ count 1))))))

(display "Splitting w/ Values: ")
(time-split split-vals)
(display "Splitting w/ set!: ")
(time-split split-set)
(display "Splitting w/ set! and reverse: ")
(time-split split-set/rev)
