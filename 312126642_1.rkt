#lang pl

;;auther dolev.h.
;; function that checks every head of the list is endding with suffix of "pl" by checking the last is 'l and the one before him is 'p  
(: plSuffixContained :(Listof String) -> (U String Boolean))
   (define (plSuffixContained list_s)

     (if (eq? list_s '()) #f
         (let ([first (first list_s)] [length (string-length (first list_s))])
       (if (and (eq? (string-ref first (- length 1)) #\l) (eq? (string-ref first (- length 2)) #\p)) first (plSuffixContained (rest list_s))))))
     
        

(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false)


;;2.1
 #|in this function we can see recursive loop on the head of the list and rerurn string of the polynom of it 
it works by conditions on the size of list (power) and the head (first element)
*checks:head- first element power the power of x 
head : num , real , negative
power=1
power=0|#


(: write-poly : (Listof Number) -> String)
 (define (write-poly list)
  (print-poly list "" (length list)))

 ;;we use negative? for the first elemenr and thats change the sign of the poly var
(: print-poly : (Listof Number) String Number -> String)
(define (print-poly list result size_list)
  (if (eq? list '()) result
      (let ([first (number->string (first list))][rest (rest list)][size (- (length list) 1)][flag (if (negative? (first list)) "" (if (eq? size_list (length list)) "" "+" ))])
        (cond
          [(eq? first "0") (print-poly rest result size_list)]
          [(eq? size 0) (print-poly rest (string-append result (string-append flag first)) size_list)]
          [(eq? size 1) (print-poly rest (string-append result (string-append flag (string-append first "x" ))) size_list)]
          [else (print-poly rest (string-append result (string-append flag (string-append first (string-append "x^" (number->string size))))) size_list)]))))




(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
;;(test (write-poly '(-0.37 1 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(7 8 -9 0)) => "7x^3+8x^2-9x+0")
(test (write-poly '(7.2 8 -9 10)) => "7.2x^3+8x^2-9x+10")

;;2.2
;;#lang pl
(: compute-poly : Number (Listof Number) -> Number)
(define (compute-poly base list)
  (if (eq? list '()) 0 (sum-poly list base 0 (length list))))
#|auther dolev.h.
function compute polynom by read head (first element) from list and we use sum-poly that return the polynom vars by length list 
and using function pow for compute power 
|#
(: sum-poly : (Listof Number) Number Natural Natural -> Number)
(define (sum-poly list num size real_size)
  (if (= size real_size) 0
  (if (= size (- real_size 1)) (+ (first list) (sum-poly (rest list) num (+ size 1) real_size))
          (let ([sum1 (pow num (- (length list) 1))]) 
          (let ([sum2 (* (first list) sum1)])
        (+ sum2 (sum-poly (rest list) num (+ size 1) real_size)))))));; sum alll our part-sum until now 

;; function to find pow
(: pow : Number Number -> Number)
(define (pow base num)
  (if (= num 0) 1 (* base (pow base (- num 1)))))



(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(6)) => 6)
(test (compute-poly 2 '(2 6)) => 10)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 3 '(0 0 0 7)) => 7)

;;3
;;#lang pl
#| used by hannael from the course he helped me understand how to using structs in pl (was the hard part for me ) 
   this struct for using in keystack and searching for symbol in it 
 define for  keyed-stack  :|# 
(define-type KeyStack
[EmptyKS]
[Push Symbol String KeyStack])

 #|in this function we use to pop element (symbol) to the stack
|#
(: pop-stack : KeyStack -> (U KeyStack Boolean))
( define (pop-stack keystack)
   (cases keystack
     [(EmptyKS) false]
     [(Push key val keystack) keystack]));;place to connect next symbol for the stack
 

;; function to search symbol in the stack 
(: search-stack : Symbol KeyStack -> (U String Boolean))
( define (search-stack symbol keystack)
   (cases keystack
     [(EmptyKS) false]
     [(Push key val next)(if (equal? key symbol) val(search-stack symbol next))]))


(test (EmptyKS) => (EmptyKS));;check that the empty list return empty
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "-AA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "-AA" (Push 'b "B" (Push 'a "A" (EmptyKS)))));;check -
(test (search-stack 'a (Push 'a "A134A" (Push 'b "B" (Push 'a"A" (EmptyKS))))) => "A134A");; check numbers
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a"A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f);; the list is empty so return false 

;;4
;;#lang pl
(: is-odd? : Natural -> Boolean)
;; the function receive natural number 
;; the function return true/false if number is odd 
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))
(: is-even? : Natural -> Boolean)
;; the function receive natural number 
;; the function return true/false if number is even
;;the function will work in recursive way on x until 0 until get true or false if the
;;(is-odd? 4)=f => (is-even? 3)=f => (is-odd? 2)=f => (is-even? 1)=f => (is-odd? 0)=f
;;so the test will not work thats why if we run (is-even? 4)
(define (is-even? x)
 (if (zero? x) ;;zero is even thats why the function have to bring back => true
 true
 (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
;; the function receive A list and from that is building a list of boolean list 
;; the function return true/false if all the test of this group return =t from head of list to tail...
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; the function receive A list and from that is building a list of boolean list 
;; it works by checking for every Natural if is-even?=t in the function every?
(define (all-even? lst)
 (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)));; the function return true/false for every test if the head (first element) of the list is-even?
(test (not (all-even? (list 1 3 5 7))));; the function return true/false for every test if the head (first element) of the list all-odd?=not (all-even?)
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
;; the function receive A  B  lists and from that is building a list of boolean tests if the lists behave in the same way - means that they both odd or even
;; the function takes the two heads of the lists and checks if they both from the same group : odd/ even 
;;;; the function return true if all the tests return true  otherwise false
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))





#|check tests- three mistake 3*2= -6 for first assainment|#
(test (plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => false) 
(test (plSuffixContained '("plpl" "plyy" "ppp" "lpTT" "lol")) => "plpl")
(test (plSuffixContained '("plll" "yyplyy" "pppl" "lpTT" "lol")) => "pppl")
(test (plSuffixContained '()) => false)
;;(test (plSuffixContained '("p" "l" "")) => false)
(test (plSuffixContained '("pl")) => "pl")
(test (write-poly '(3 2 6)) => "3x^2+2x+6") 
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(3.1 2 6.2)) => "3.1x^2+2x+6.2")
(test (write-poly '(1 1 1)) => "1x^2+1x+1")
;;(test (write-poly '(0 0 0)) => "0")
;;(test (write-poly '(1 0 1 0)) => "1x^3+1x")
(test (write-poly '(3 -2 6)) => "3x^2-2x+6")
(test (write-poly '(-3 2 6)) => "-3x^2+2x+6")
(test (compute-poly 2 '()) => 0) 
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
(test (compute-poly 2 '(0 0 1)) => 1) 
(test (EmptyKS) => (EmptyKS)) 
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)
(test (not (is-odd? 12))) 
(test (is-even? 12))
(test (not (is-odd? 0))) 
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7)))) 
(test (not (all-even? (list 1))))
