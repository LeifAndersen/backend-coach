#!r6rs

(import (chezscheme)
        (graph)
        (G)
        (dfs))

(time
 (let loop ([i 0])
   (cond [(< i 100000)
          (walk n0 n1)
          (loop (add1 i))])))
 