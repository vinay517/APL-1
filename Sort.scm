;Name: Swapnika Pasunuri-U00843540
;      Vinay Mohan Behara-U00851261
;EmailId: pasunuri.2@wright.edu
;         behara.4@wright.edu



(define (smallest n lon)
  (cond ((null? lon) n)                                 ;finding the smallest number of the list lon
        ((> n (car lon)) (smallest (car lon)(cdr lon)))
        (else (smallest n (cdr lon)))
  )
)

(define (delete n lon)               
  (cond ( (null? lon) '() )                             ;deleting the number n from the list lon
        ( (= n (car lon)) (cdr lon))  
        (else (cons (car lon)(delete n (cdr lon))))
  )
)

(define (sort lon) 
   (cond ((null? lon) '() )                              ;constructs the list with smallest element and removes the it from the remaining list
         (else (cons (smallest (car lon) lon) 
                      (sort (delete (smallest (car lon) lon) lon))))
   )
)
"should return (2 2 3 5 8)"
(sort '(8 2 5 2 3)) 
