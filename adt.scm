
;      Vinay Mohan Behara




;1.1 Signatures in Abstract Data Type(Map)
;      emap         : empty map
;       clear        : map -> empty map
;      containsKey  : map, key-> boolean
;      containsValue: map, value(integer)->boolean
;      equals       : map1, map2->boolean
;      get          : map,key-> value(integer)
;      isEmpty      : map->boolean
;      put          : map, key, value(integer)->map
;      remove       : map,key-> map
;      size         : map->integer

;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------

;1.2 Classification of Operations
;      observers       : containsKey, containsValue, equals, get, isEmpty, size
;      constructors    : emap, put
;      non constructors: remove, clear

;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------
;1.3 Axioms
;      conatinsKey(emap, key) #f
;      containsKey(put(map,key1,value1),key2) (if (equal? key1 key2) #t (containsKey(map,key2)))
;      containsValue(map, value) (if(empty? map)) #f 
;      containsValue(put(map,key1,value1),value2) (if(equal? value1 value2 ))  #t containsValue(map,value2)
;      equals(map1,map2) (if (equal? m n) #t #f))
;      equals(emap,map) #f
;      equals(put(map,key,value),emap) #f
;      equals(put(map1, key1,value1), put(map2,key2,value2)) (if ((equal? key1 key2) and (equal? value1 value2)) #t #f)
;      get(emap, key) = null
;      get(put(map,key1,value),key2) (if (equal? key1 key2 ) value get(map,key2))
;      isEmpty(emap) = #t
;      isEmpty(put(map,key,value)) (if map is not empty) #f
;      size(emap) = 0
;      size(put(map,key,value)) = Integer
;      remove(emap,key) = '()
;      remove(put(map,key1,value),key2) (if(equal? key1 key2) remove(map,key1) put(remove(map,key2),key1,value))
;      clear(emap)= '()
;      clear(map)= '()

;-----------------------------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------------------
;2. Implementation of ADT Map

;      CONTRACT: containsKey : map() (key value)->boolean
;      PURPOSE: containsKey takes a map and key as input and checks if the map has the key and returns a boolean value
;      CODE:
       "containsKey"
      (define (containsKey map key)
        (if (null? map)#f
            (if(eq? (car(car map)) key)#t
              (if(null? (cdr map))#f
                 (containsKey(cdr map) key)
              )
           )
         )
      )
;      TEST CASES and EXPECTED OUTCOMES:
      (containsKey '((8 5) (2 4) (3 6))2) "should be #t"
      (containsKey '((23 56) (31 67) (99 88))89) "should be #f"
      (containsKey '((1 2) (3 4) (5 6) (7 8))3) "should be #t"
;--------------------------------------------------------------------------------------------------------------------------------------------------
;      CONTRACT: containsValue : map() (key value)->boolean
;      PURPOSE: containsValue takes a map and a value as input and checks if the map has the value bound to any key and returns a boolean value
;      CODE:
       "containsValue"
      (define (containsValue map value)
         (if (null? map) #f
             (if(eq? (car(cdr(car map))) value) #t
                (if(null? (cdr map)) #f
                   (containsValue(cdr map) value)
                   )
                )
             )
       )
;      TEST CASES and EXPECTED OUTCOMES:
       (containsValue '((8 5) (2 4) (3 6))5) "should be #t"
       (containsValue '((23 56) (31 67) (99 88))89) "should be #f"
       (containsValue '((1 2) (3 4) (5 6) (7 8))6) "should be #t"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;      CONTRACT: equals :   (map1 map2)->boolean
;      PURPOSE: Takes two maps as inputs and checks if the two maps are equal and if equal retunrs a boolean value
;      CODE:
       "equals"
       (define (equals map1 map2)
         (if (equal? map1 map2) #t #f)
        )
;      TEST CASES and EXPECTED OUTCOMES:
       (equals '((1 2) (5 6)) '((3 4) (5 6))) "should be #f"
       (equals '((4 5)) '((4 5))) "should be #t"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;      CONTRACT: get: (map key)->value
;      PURPOSE: Takes a map and a key as input and returns the value bound the key given as input
;      CODE:
       "get"
       (define (get map key)
         (if (null? map) #f
           (if (eq? (car(car map)) key) (cdr(car map))
              (if (null? (cdr map)) #f
                 (get(cdr map) key)
              )
           )
         )
       )
;     TEST CASES and EXPECTED OUTCOMES:
      (get '((33 5) (77 9) (45 7) (99 5)) 99) "should be 5"
      (get '((33 5) (77 9) (45 7) (99 5)) 96) "should be #f"
      (get '((33 5) (77 9) (45 7) (99 5)) '()) "should be #f"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;      CONTRACT:isEmpty (map)->boolean
;      PURPOSE: Takes a map and checks if its empty and returns a boolean value
;      CODE:
       "isEmpty"
       (define (isEmpty map)
         (if (null? map) #t #f)
       )
;      TEST CASES and EXPECTED OUTCOMES:
       (isEmpty '((33 5) (77 9) (45 7) (99 5))) "should be #f"
       (isEmpty '()) "should be #t"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;       CONTRACT: put: (map key value)->(map)
;       PURPOSE: takes a map and a new pair of Key and Value and returns the map with the new pair of key and value in it.
;       CODE:
        "put"
        (define (put map key_value)
           (append map key_value)
         )
;        TEST CASES and EXPECTED OUTCOMES:
         (put '((33 5) (77 9) (45 7) (99 5)) '((100 10)))
;-------------------------------------------------------------------------------------------------------------------------------------------------
;        CONTRACT: remove: (map key)->(map)
;        PURPOSE: Takes a map and a key as input and returns the new map with the mentioned key deleted if its present 
;        CODE:
         "remove"
         (define (remove map key)
            (cond ( (null? map) '() )           
              ((= key (car(car map))) (cdr map))  
            (else (cons (car map)(remove (cdr map) key)))
            )
           )
;        TEST CASES and EXPECTED OUTCOMES:
         (remove '((33 5) (77 9) (45 7) (99 5)) 99) "should print ((33 5) (77 9) (45 7))"
         (remove '() 34) "should print ()"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;         CONTRACT: size: (map)->Integer
;         PURPOSE: Takes a map and returns the number of key value pairs in it
;         CODE:
;         "size"
          (define (size map)
            (if (null? map) 0
               (+ (size (cdr map)) 1)
             )
          )
;        TEST CASES and EXPECTED OUTCOMES:
         (size '((33 5) (77 9) (45 7) (99 5))) "should be 4"
         (size '()) "should be 0"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;        CONTRACT: emap-> empty map
;        PURPOSE: yeilds an empty map
;        CODE:
         "emap"
         (define emap '())
;        TEST CASES and EXPECTED OUTCOMES:
         emap "should be ()"  
;-------------------------------------------------------------------------------------------------------------------------------------------------
;        CONTRACT: clear : (map)->empty map
;        PURPOSE: takes a map as input and returns an empty map
;        CODE:
          "clear"
         (define (clear map) '())
;         TEST CASES and EXPECTED OUTCOMES:
          (clear '((1 2)(2 3)(4 5))) "should be ()"
          (clear '()) "should be ()"
;-------------------------------------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------------------------------------------------

