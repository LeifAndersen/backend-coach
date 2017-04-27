#!r6rs

(library (dot)
 (export graph->dot graph->scheme)
 (import (chezscheme)
         (graph)
         (dfs))
 (define (graph->dot G)
   (define strs '())
   (graph-for-each
    (lambda (n)
      (define src (node-name n))
      (for-each
       (lambda (ne)
         (define dst (node-name ne))
         (set! strs (cons (format "\"~a\" -> \"~a\";\n" src dst) strs)))
       (node-next n)))
    G)
   (format "digraph G {\n~a}\n" (apply string-append strs)))

 (define (graph->scheme G)
   (define nodes '())
   (define ->s '())
   (graph-for-each
    (lambda (n)
      (define src (node-name n))
      (set! nodes (cons `(def-node ,(string->symbol src)) nodes))
      (for-each
       (lambda (ne)
         (define dst (node-name ne))
         (set! ->s (cons `(-> ,(string->symbol src) ,(string->symbol dst)) ->s)))
       (node-next n)))
    G)
   `(library (G)
      (export ,(string->symbol (node-name G)))
      (import (chezscheme)
              (graph))
     (import (chezscheme)
             (graph))
     ,@nodes
     ,@->s)))
