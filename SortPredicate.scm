
;      Vinay Mohan Behara



(define (sortpredicate sym lon)
  (cond ((null? lon) '())
        ((eq? sym >) (sort_descending lon))             ;checks whether to perform ascending or descending
        ((eq? sym <) (sort_ascending lon))
        (else '())
   )
)
(define (delete n lon)               
  (cond ( (null? lon) '() )                             ;deleting the number n from the list lon
        ( (= n (car lon)) (cdr lon))  
        (else (cons (car lon)(delete n (cdr lon))))
  )
)

(define (largest n lon)
  (cond ((null? lon) n)
        ((< n (car lon)) (largest (car lon)(cdr lon)))    ;finding the largest number of the list lon
        (else (largest n (cdr lon)))
  )
)

(define (smallest n lon)
  (cond ((null? lon) n)                                  ;finding the smallest number of the list lon
        ((> n (car lon)) (smallest (car lon)(cdr lon)))
        (else (smallest n (cdr lon)))
  )
)

(define (sort_descending lon) 
   (cond ((null? lon) '() )
         (else (cons (largest (car lon) lon)             ;constructs the list with largest element and removes the it from the remaining list
                      (sort_descending (delete (largest (car lon) lon) lon))))
   )
)

(define (sort_ascending lon) 
   (cond ((null? lon) '() )
         (else (cons (smallest (car lon) lon)            ;constructs the list with smallest element and removes the it from the remaining list
                      (sort_ascending (delete (smallest (car lon) lon) lon))))
   )
)


"should return (8 5 3 2 2 )"
(sortpredicate > '(8 2 5 2 3))
"should return (2 2 3 5 8)"
(sortpredicate < '(8 2 5 2 3))

