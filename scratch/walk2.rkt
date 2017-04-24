#!r6rs

(import (rnrs))

(let ()

(define-record-type node
  (fields name (mutable next node-next set-node-next!)))

(define-syntax def-node
  (lambda (stx)
    (syntax-case stx ()
      [(_ name)
       #`(define name (make-node #,(symbol->string (syntax->datum #'name)) '()))])))

;; Add `next` to the list of nodes that `node` points to.
;; Node Node -> Void
(define (-> node next)
  (set-node-next! node (cons next (node-next node))))

;; ===================================================================================================

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
    p))

;; ===================================================================================================

;; Example Graph

(def-node A)
(def-node B)
(def-node C)
(def-node D)
(def-node E)
(def-node F)
(-> A B)
(-> A C)
(-> B C)
(-> D A)
(-> B D)
(-> E F)
(display (walk A D))
(newline))
