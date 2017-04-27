#!r6rs

(library (gen)
 (export gen-graph)
 (import (chezscheme)
         (graph))

 (define gen-graph
   (case-lambda
     [() (gen-graph 100 200)]
     [(node-count connection-count)
      ;; First, generate a bunch of nodes
      (define node-set (make-vector node-count #f))
      (let loop ([i 0])
        (cond [(< i node-count)
               (vector-set! node-set i (mk-node (format "n~a" i)))
               (loop (add1 i))]))
      
      ;; Then, generate a bunch of connections
      (let loop ([i 0])
        (cond [(< i connection-count)
               (let ([n1 (vector-ref node-set (random node-count))]
                     [n2 (vector-ref node-set (random node-count))])
                 (-> n1 n2)
                 (loop (add1 i)))]))

      ;; Return the newly created graph
      node-set])))
