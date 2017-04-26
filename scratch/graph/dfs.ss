#!r6rs

(library (dfs)
 (export walk)
 (import (rnrs)
         (graph))

 ;; Helper function for walk.
 ;; Node Node (Listof String) (Listof Node) -> (Values (U (Listof String) #f) (Listof Node))
 (define (wlk g end path seen)
   (cond [(equal? g end) (values (cons (node-name g) path) seen)]
         [(member g seen) (values #f seen)]
         [else
          (let loop ([next (node-next g)]
                     [seen seen])
            (cond [(null? next) (values #f seen)]
                  [else
                   (let-values ([(p s) (wlk (car next) end (cons (node-name g) path) (cons g seen))])
                     (if p
                         (values p s)
                         (loop (cdr next) s)))]))]))

 ;; Walks graph using a depth first search
 ;; Node Node -> (U (Listof String) #f)
 (define (walk start end)
   (let-values ([(p s) (wlk start end '() '())])
     p)))
 