#!r6rs

(library (dfs)
 (export walk graph-for-each)
 (import (chezscheme)
         (graph))

 (define-record-type count
   (fields (mutable acc inc inc!)))

 (define c1 (make-count 0))
 (define c2 (make-count 0))
 (define c3 (make-count 0))

 ;; Helper function for walk.
 ;; Node Node (Listof String) (Listof Node) -> (Values (U (Listof String) #f) (Listof Node))
 (define (wlk g end proc)
   (define path '())
   (define seen '())
   (define (wlk2 g path a b c)
     (inc! a 0)
     (inc! b 0)
     (inc! c 0)
     (cond [(equal? g end) (cons (node-name g) path)]
           [(member g seen) #f]
           [else
            (set! seen (cons g seen))
            (if proc (proc g))
            (let loop ([next (node-next g)])
              (cond
                [(null? next) #f]
                [else
                 (or (wlk2 (car next) (cons (node-name g) path) a b c)
                     (loop (cdr next)))]))]))
   (wlk2 g path c1 c2 c3))
 
 ;; Walks graph using a depth first search
 ;; Node Node -> (U (Listof String) #f)
 (define (walk start end)
   (wlk start end #f))

 (define (graph-for-each proc start)
   (wlk start #f proc)
   (void)))
