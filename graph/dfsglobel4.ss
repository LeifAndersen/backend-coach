#!r6rs

(library (dfs)
 (export walk graph-for-each)
 (import (chezscheme)
         (graph))

 (define c1 #f)
 (define c2 #f)
 (define c3 #f)

 ;; Helper function for walk.
 ;; Node Node (Listof String) (Listof Node) -> (Values (U (Listof String) #f) (Listof Node))
 (define (wlk g end proc)
   (define path '())
   (define seen '())
   (define (wlk2 g path a b c)
     (cond [(equal? g end) (set! c1 a)
                           (set! c2 b)
                           (set! c3 c)
                           (cons (node-name g) path)]
           [(member g seen) #f]
           [else
            (set! seen (cons g seen))
            (if proc (proc g))
            (let loop ([next (node-next g)])
              (cond
                [(null? next) #f]
                [else
                 (or (wlk2 (car next)
                           (cons (node-name g) path)
                           (cons (add1 (car a)) a)
                           (cons (+ (car b) 2) b)
                           (cons (+ (car c) 3) c))
                     (loop (cdr next)))]))]))
   (wlk2 g path '(0) '(0) '(0)))
 
 ;; Walks graph using a depth first search
 ;; Node Node -> (U (Listof String) #f)
 (define (walk start end)
   (wlk start end #f))

 (define (graph-for-each proc start)
   (wlk start #f proc)
   (void)))
