;; To log a compilation record:
> (transcript-on "<output-file.txt>")
> (#%$np-tracer #t) ; or list of symbols for each pass
> (compile-file "filename.ss")

;; You can also find a debug build of chez at:
https://github.com/LeifAndersen/ChezScheme/tree/leif-debug
