#lang plai
;Algorithms - Figure 1.2
;; ints are represented as reverse lists of binary digits which I call bitlists. Example:  (0 0 1) = 4
(define x (list 1 1 1 0 0 1)) ; = 39
(define y (list 1 1 0 1 1 0)) ; = 27
(define z (list 0 0 0 0 0 0)) ; = 0

;; bitlist->int : bitlist -> int
;; a conversion function from bitlist to int
;; example:  (bitlist-int '(0 1 1 0 1 1 0))
(define (bitlist->int lis)
  (cond
    ((null? (rest lis)) (first lis))
    ( #t   (+ (first lis)(* 2 (bitlist->int (rest lis))))))) 

(test (bitlist->int '(1 1 0 0 1 1)) 51)
(test (bitlist->int x) 39)
(test (bitlist->int y) 27)
(test (bitlist->int z) 0)


;; int->bitlist : bitlist -> int
;; a conversion function from int to bitlist
;; when converting to a bitlist, the number of digits might vary,
;; so we supply the length explicitly
;; example: (int-bitlist 32)
(define (int->bitlist n noDigits)  
  (cond
    ((=  noDigits 0) '())
    ( #t (cons (remainder n 2)(int->bitlist (quotient n 2)(- noDigits 1))))))

(test (int->bitlist 39 6) x)
(test (int->bitlist 12 7) '(0 0 1 1 0 0 0))
(test (int->bitlist 27 6) y)
(test (int->bitlist  0 6) z)


;; zero? : bitlist -> boolean
;; a predicate function that returns true if the bitlist represents a 0
;; example:  (zero? '(1 1 0 0 0 1))
;; example:  (zero? '(0 0 0))
(define (zero? x)
  (cond
     ((null? x) #t)
     ((= (first x) 1) #f)
     ( #t  (zero? (rest x))))) 

(test (zero? x) #f)
(test (zero? z) #t)

;; addWithCarry : bitlist, bitlist, integer -> bitlist
;; computes a bitlist that is the sum of the two input bitlists and the carry
;; the two bitlists can vary in size
;; example:  (addWithCarry '(0 1 0 1)'(1 1 0) 1)
(define (addWithCarry x y carry)
  (cond
    ((and (null? x)(null? y)) (if (= carry 0) '() '(1)))
    ((null? x) (addWithCarry '(0) y carry))
    ((null? y) (addWithCarry x '(0) carry))
    ( #t  (let ((bit1 (first x))
                (bit2 (first y)))
               (cond
                 ((= (+ bit1 bit2 carry) 0) (cons 0 (addWithCarry (rest x) (rest y) 0)))
                 ((= (+ bit1 bit2 carry) 1) (cons 1 (addWithCarry (rest x) (rest y) 0)))
                 ((= (+ bit1 bit2 carry) 2) (cons 0 (addWithCarry (rest x) (rest y) 1)))
                 (   #t                     (cons 1 (addWithCarry (rest x) (rest y) 1))))))))

(test (addWithCarry '(1 1 1)'(1) 0)'(0 0 0 1))
(test (addWithCarry '(1 1 1)'(1) 1)'(1 0 0 1))

;; add : bitlist, bitlist -> bitlist
;; a function that adds two bitlists with an implied carry of 0
;; works with different size bitlists
;; example: (test (add '(1 1 1)'(1 0 1)) '(0 0 1 1)) 
(define (add x y)
  (addWithCarry x y 0))

(test (add '(1 0 1 0 1 0 1)'(1 1 0 0 0 1 1)) '(0 0 0 1 1 1 0 1))    
(test (add '(1 1 0 0 0 1 1)'(1 1 0 0 0 1 1)) '(0 1 1 0 0 0 1 1)) 
(test (add '(1 1 0)'(1 1 1 1 ))'(0 1 0 0 1))
(test (add '(1)'(1 1 1 1 )) '(0 0 0 0 1))

(define (addem lis)
  (cond
    ((null? lis) 0)
    ( #t ( + (car lis) (addem cdr)))))
