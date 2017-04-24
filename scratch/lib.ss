(display "hello")
"hello"
"world"

(define $hi
  (let ()
    (import (rnrs))
    (define (hello) "HI THERE")
    hello))
