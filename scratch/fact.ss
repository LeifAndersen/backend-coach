(import (rnrs))
(define (displayln x)
  (display x)
  (newline))

(define (fact x)
  (if (zero? x)
      1
      (* x (fact (- x 1)))))

(displayln (fact 10))

;(displayln (fact "hello!"))
