#!r6rs

(library (basics)
 (export
  ;; ---------------------------------------------------------------------------------------------------
  ;; basic game ontology and data structures for simple concepts (hotels, cash, shares)

  ;; ---------------------------------------------------------------------------------------------------
  hotel?
  ;; (-> Any Boolean)
  hotel<=?
  ;; (-> Hotel Hotel Boolean)
 
  ALL-HOTELS
  ;; (Listof Hotel), also sorted
 
  SAFE-n FINAL-n
  ;; Natural

  hotel->label
  ;; (-> Hotel String)

  hotel->color
  ;; (-> Hotel Symbol)

  shares?
  ;; (-> Any Boolean)

  banker-shares0
  ;; Shares

  player-shares0
  ;; Shares

  shares-order?
  ;; 

  SHARES-PER-TURN-n
  ;; Natural

  shares++
  ;; (-> Shares Hotel Shares)

  shares--
  ;; (-> Shares Hotel Shares)
  ;; Precondition: (shares-available s h) > 0

  shares-available
  ;; (-> Shares Hotel Natural)

  shares-available?
  ;; (-> Shares (Listof Hotel) Boolean)
  ;; Precondition: (shares-order? s*)
  ;; "Can the given order of shares be satisfied in this wallet?

  shares-minus
  ;; (-> Shares Shares Shares)

  shares-plus
  ;; (-> Shares Shares Shares)

  shares-compatible
  ;; (-> Shares (-> Shares Boolean))

  shares-combinable?
  ;; (-> (Listof Shares) Boolean)

  *combine-shares
  ;; (-> (Listof Shares) Shares)
  ;; Precondition: shares-combinable

  *create-shares
  ;; (-> Hotel Natural Shares)
  
  shares->string
  ;; (-> Shares String)

  cash?
  ;; (-> Any Boolean)

  CASH0
  ;; Cash

  price-per-share
  ;; (-> Hotel Natural (Option Cash))

  bonus
  ;; (-> M*rity Hotel Natural Cash)
  )
 (import (chezscheme)
         (auxiliaries)
         (base))

 (define cons? pair?)
 (define first car)
 (define rest cdr)

 (define (exact-integer? x)
   (and (integer? x)
        (exact? x)))
 
 (define (exact-nonnegative-integer? x)
   (and (integer? x)
        (exact? x)
        (>= x 0)))

 ;; ---------------------------------------------------------------------------------------------------

 (define MIN-PLAYER-n 3)
 (define MAX-PLAYER-n 6)

 (define SAFE-n 12)
 (define FINAL-n 40)
 
 (define AMERICAN    "American")
 (define CONTINENTAL "Continental")
 (define FESTIVAL    "Festival")
 (define IMPERIAL    "Imperial")
 (define SACKSON     "Sackson")
 (define TOWER       "Tower")
 (define WORLDWIDE   "Worldwide")
 
 (define HOTELS
   `(,AMERICAN ,CONTINENTAL ,FESTIVAL ,IMPERIAL ,SACKSON ,TOWER ,WORLDWIDE))
 (define HOTEL:C
   '(red      blue        green    yellow   purple  brown   orange))
 
 ;; Hotel  :: HOTELS 
 
 (define (hotel? x)
   (cons? (member x HOTELS)))
 
 (define hotel<=? string<=?)
 
 (define ALL-HOTELS HOTELS)
 
 (define random-hotel (lambda () (randomly-pick HOTELS)))
 
 (define (hotel->color h)
   (define r
     (let loop ([acc #f]
                [i HOTELS]
                [c HOTEL:C])
       (cond [(or (null? i) (null? c))
              acc]
             [else
              (or acc (and (equal? h (car i)) (car c)))])))
   (or r 
       (error 'hotel->color (format "Unbound hotel ~a" h))))

 (define (string->hotel n)
   (and n (member n HOTELS) n))

 (define (hotel->label x)
   x)

 ;; ---------------------------------------------------------------------------------------------------
 ;; SHARES = [Hashof Hotel Nat]

 (define SHARES0 25)
 (define SHARES-PER-TURN-n 2)

 (define (listof-hotel? x)
   (and (list? x)
        (fold-left
         (lambda (acc x)
           (and acc (hotel? x)))
         #t
         x)))

 ;;bg; changed from shares-order/c
 (define (shares-order? x*)
   (define h* (assert* x* listof-hotel?))
   (and
    (not (null? h*))
    (let ([h1  (car h*)])
      (fold-left
       (lambda (acc h2)
         (and acc (string=? h1 h2)))
       #t
       (cdr h*)))
    (<= SHARES-PER-TURN-n (length h*))))

 (define player-shares0
   (let ([tab (make-hashtable equal-hash equal?)])
     (for-each (lambda (h) (hashtable-set! tab h 0) ALL-HOTELS))
     tab))

 (define banker-shares0
   (let ([tab (make-hashtable equal-hash equal?)])
     (for-each (lambda (h) (hashtable-set! tab h SHARES0)) ALL-HOTELS)))

 (define (shares? x)
   (define ret (hashtable? x))
   (and ret
        (let-values ([(k* v*) (hashtable-entries x)])
          (map
           (lambda (k v)
             (set! ret (and ret
                            (hotel? k)
                            (exact-nonnegative-integer? k))))
           k* v*))))

 (define (shares-minus s t)
   (define ret (hashtable-copy s #t))
   (let-values ([(k* v*) (hashtable-entries t)])
     (map
      (lambda (hotel n)
        (hashtable-update! ret
                           hotel
                           (lambda (m) (max 0 (- m n)))
                           (lambda () (error 'shares-minus "shouldn't be here"))))
      k* v*))
   ret)

 (define (shares-plus s t)
   (define ret (hashtable-copy s #t))
   (let-values ([(k* v*) (hashtable-entries t)])
     (map
      (lambda (hotel n)
        (hashtable-update! ret
                           hotel
                           (lambda (m) (+ m n))
                           (lambda () (error 'shares-plus "shouldn't be here"))))
      k* v*))
   ret)

 (define (shares-- s h)
   (unless (> (shares-available s h) 0)
     (error 'shares-- (format "Precondition failed: (> (shares-available ~a ~a) 0)" s h)))
   (shares--* s h))

 (define (shares--* s h)
   (define ret (hashtable-copy s #t))
   (hashtable-update! ret h sub1 (lambda () (error 'shares--* "shouldn't be here")))
   ret)

 (define (shares++ s h)
   (define ret (hashtable-copy s #t))
   (hashtable-update! ret h add1 (lambda () (error 'shares++ "shouldn't be here"))))

 (define (ext:shares-available s h)
   (unless (shares-order? h)
     (error 'shares-available (format "Precondition: shares-order ~a\n" h)))
   (shares-available s h))

 (define (shares-available s h)
   (hashtable-ref s h "shares-available error"))

 (define (shares-available? available-s hotels)
   (unless (shares-order? available-s)
     (error 'shares-available "Precondition"))
   (shares-available?* available-s hotels))

 (define (shares-available?* available-s hotels)
   (hashtable?
    (fold-left
     (lambda (s h)
       (and s
            (> (shares-available s h) 0)
            (shares--* s h)))
     available-s
     hotels)))

 (define (shares->list s)
   (let-values ([(k* v*) (hashtable-entries s)])
     (fold-left
      (lambda (l hotel count)
        (append (make-list count hotel)))
      '()
      k* v*)))

 (define (list->shares hotels)
   (fold-left
    (lambda (s h)
      (shares++ s h))
    player-shares0
    hotels))

 (define (shares-compatible s)
   (lambda (t)
     (let-values ([(k* v*) (hashtable-entries t)])
       (let loop ([hotel k*]
                  [count v*])
         (and (>= (shares-available s (car hotel)) (car count))
              (loop (cdr hotel) (cdr count)))))))
 
 (define (string->count x)
   (define n (string->number x))
   (and n (exact-integer? n) (<= 0 n) (<= n SHARES0) n))

 (define (shares->string sh)
   (let-values ([(k* v*) (hashtable-entries sh)])
     (apply string-append
            (map
             (lambda (h c) (format "~a : ~a, " h c))
             k* v*))))

 (define (*create-shares h n)
   (let loop ([i 0]
              [acc player-shares0])
     (cond [(< i n) (loop (add1 i) (shares++ acc h))]
           [else acc])))

 (define (shares-combinable? ls)
   (define s (fold-right shares-plus player-shares0 ls))
   (let-values ([(k* v*) (hashtable-entries s)])
     (fold-left
      (lambda (acc count)
        (and acc (<= count SHARES0)))
      #t
      v*)))

 (define (*combine-shares* s)
   (fold-right shares-plus player-shares0 s))

 (define (*combine-shares s)
   (unless (shares-combinable? s)
     (error '*combine-shares (format "Precondition error: shares-combinable ~a" s)))
   (*combine-shares* s))

 ;; ---------------------------------------------------------------------------------------------------
 ;; CASH

 (define CASH0 8000)

 ;(define-predicate cash? Cash)
 (define cash? exact-nonnegative-integer?)

 (define (string->cash s)
   (define n (string->number s))
   (and n (exact-integer? n) (>= n 0) n))

 (define (cash->string c)
   (number->string c))

 ;; ---------------------------------------------------------------------------------------------------
 ;; the cost table for hotels, for buying shares and merging hotels 

 (define PRICES
   `((Price (,WORLDWIDE ,SACKSON) (,FESTIVAL ,IMPERIAL ,AMERICAN) (,CONTINENTAL ,TOWER))
     (200            2                     0                        0)
     (300            3                     2                        0)
     (400            4                     3                        2)
     (500            5                     4                        3)
     (600            6                     5                        4)
     (700           11                     6                        5)
     (800           21                    11                        6)
     (900           31                    21                       11)
     (1000          41                    31                       21)
     (1100        +inf.0                  41                       31)
     (1200        +inf.0                 +inf.0                    41)))

 #;
 (when (fold-left
        (lambda (acc price)
          (and acc (member price HOTELS)))
        #t
        (apply append (rest (first PRICES))))
   (error 'PRICES "didn't port error message"))

 ;; determine the majority and minority bonus for a hotel of size tile#

 (define (bonus mode hotel tile-n)
   (* (or (price-per-share hotel tile-n) (error 'bonus "message"))
      (if (eq? mode 'majority) 10 5)))

 ;; determine the price per share for a hotel of size tile#

 (define (price-per-share hotel tile-n)
   (define table (rest PRICES))
   (define limit-selector
     (or
      (let loop ([hotels (rest (first PRICES))]
                 [selector (list cadr caddr cadddr)]
                 [acc #f])
        (cond [(or (null? hotels) (null? selector))
               acc]
              [else
               (loop (cdr hotels)
                     (cdr selector)
                     (and (member hotel (car hotels)) (car selector)))]))
      (error 'price-per-share "message")))
   (define price*
     (reverse (map first table)))
   (define limit*
     (reverse (map limit-selector table)))
   (let loop ([acc #f]
               [price price*]
               [limit limit*])
      (cond [(or (null? price) (null? limit))
             acc]
            [else
             (loop (or acc (and (>= tile-n (car limit) (assert* (car price) exact-nonnegative-integer?))))
                   (cdr price)
                   (cdr limit))]))))

