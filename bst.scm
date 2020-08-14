;Name: Swapnika Pasunuri-U00843540
;      Vinay Mohan Behara-U00851261
;EmailId: pasunuri.2@wright.edu
;         behara.4@wright.edu


(define (find n tree) 
    (cond
      ((null? tree) '()) ;if tree is empty returns null
      ((< n (car tree)) (cons 'left (find n (cadr tree)))) ;left of binary tree is less in value than root
      ((> n (car tree)) (cons 'right (find n (caddr tree)))) ; right of binary tree is greater than the root
      ((= n (car tree)) '())
    )
)

(find 17 '(14 (7 () (12 () ()))
                       (26 (20 (17 () ()) ())
                           (31 () ()))))

