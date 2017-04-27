#!r6rs

(library (dfs)
 (export walk graph-for-each)
 (import (chezscheme)
         (graph))

 ;; Helper function for walk.
 ;; Node Node (Listof String) (Listof Node) -> (Values (U (Listof String) #f) (Listof Node))
 (define (wlk g end proc)
   (define path '())
   (define seen '())
   (define (wlk2 g path)
     (cond [(equal? g end) (cons (node-name g) path)]
           [(member g seen) #f]
           [else
            (set! seen (cons g seen))
            (if proc (proc g))
            (let loop ([next (node-next g)])
              (cond
                [(null? next) #f]
                [else
                 (or (wlk2 (car next) (cons (node-name g) path))
                     (loop (cdr next)))]))]))
   (wlk2 g path))
 
 ;; Walks graph using a depth first search
 ;; Node Node -> (U (Listof String) #f)
 (define (walk start end)
   (wlk start end #f))

 (define (graph-for-each proc start)
   (wlk start #f proc)
   (void)))
