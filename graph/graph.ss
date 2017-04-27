#!r6rs

(library (graph)
 (export mk-node def-node node? -> node-name node-next)
 (import (rnrs))

 (define-record-type node
   (fields name (mutable next node-next set-node-next!)))

 (define-syntax def-node
   (lambda (stx)
     (syntax-case stx ()
       [(_ name)
        #`(define name (make-node #,(symbol->string (syntax->datum #'name)) '()))])))
 
 (define (mk-node name)
   (make-node name '()))

 ;; Add `next` to the list of nodes that `node` points to.
 ;; Node Node -> Void
 (define (-> node next)
   (set-node-next! node (cons next (node-next node)))))
