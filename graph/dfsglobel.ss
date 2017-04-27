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
     (cond [(equal? g end) (values (cons (node-name g) path) seen)]
           [(member g seen) (values #f seen)]
           [else
            (if proc (proc g))
            (let loop ([next (node-next g)]
                       [seen seen])
              (cond
                [(null? next) (values #f seen)]
                [else (let-values ([(p s) (wlk2 (car next)
                                                (cons (node-name g) path))])
                        (if p
                            (values p s)
                            (loop (cdr next) s)))]))]))
   (wlk2 g path))
 
 ;; Walks graph using a depth first search
 ;; Node Node -> (U (Listof String) #f)
 (define (walk start end)
   (let-values ([(p s) (wlk start end '() #f)])
     p))

 (define (graph-for-each proc start)
   (let-values ([(p s) (wlk start #f proc)])
     (void))))
