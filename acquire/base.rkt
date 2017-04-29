#!r6rs

(library (base)
 (export assert*)
 (import (chezscheme))

 ;; =============================================================================

 (define (assert* v p)
   (unless (p v)
     (error 'assert "value ~a does not pass test ~a" v (record-type-name p)))
   v))
 