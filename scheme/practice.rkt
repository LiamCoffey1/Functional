
;Making lists
(define (hello) (cons "hi" "everybody"))
(define (just0) (cons 0 '()))
(define (upTens) (cons 1 (cons 10 100)))
(define (upTens2) (cons 1 (cons 10 (cons 100 '()))))
(define (girls) (cons #\I (cons "saw" (cons 3 (cons "girls" '())))))
(define (sumOf1234) (cons "Sum of" (cons '(1 2 3 4) (cons "is" (cons 10 '())))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;functions
; Hello world as a variable
(define vhello "Hello world")     ;1

; Hello world as a function
(define fhello (lambda ()         ;2
		 "Hello world"))

; hello with name
(define hello
  (lambda (name)
    (string-append "Hello " name "!")))

; sum of three numbers
(define sum3
  (lambda (a b c)
    (+ a b c)))

(define (addOne n)
  (+ n 1))

(define (subtractOne n)
  (- n 1))

(define (square n)
  (* n n))

(define (pythagerous x y)
  (sqrt (+ (square x) (square y))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branching

(define (abs. x)
  (if (> x 0) x (- x (* 2 x))))

(define (recip x)
  (if (equal? x 0) #f (/ 1 x)))

(define (posProd x y z)
  (if (and (< 0 x) (< 0 y) (< 0 z)) (* x y z) #f))

(define (negProd x y z)
  (if (or (< 0 x) (< 0 y) (< 0 z)) #f (* x y z)))

(define (score n)
  (cond
    ((>= n 70) "A")
    ((>= n 60) "B")
    ((>= n 50) "C")
    ((>= n 40) "D")
    (else "F")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let local variable
(define (throw v a)
  (let ((r (/ (* 4 a (atan 1.0)) 180)))
    (/ (* 2 v v (cos r) (sin r)) 9.8)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Recursion
(define (elementOf value list)
  (cond
    ((null? list) #f)
    ((equal? value (car list)) #t)
    (else (elementOf value (cdr list)))))

(define (my-length list)
  (if (null? list) 0 (+ 1 (my-length (cdr list)))))

(define (remove-x list x)
  (cond
    ((null? list) '())
    ((equal? (car list) x) (remove-x (cdr list) x))
    (else (cons (car list) (remove-x (cdr list) x)))))

(define (get-index list x i)
  (cond
    ((null? list) #f)
    ((equal? (car list) x) i)
    (else (get-index (cdr list) x (+ i 1)))))

;Tail recursion
(define (my-reverse list)
  (my-reverse-rec list '()))
(define (my-reverse-rec list new)
  (if (null? list)
      new
      (my-reverse-rec (cdr list) (cons (car list) new))))

(define (sum-num list)
  (sum-num-rec list 0))
(define (sum-num-rec list count)
  (if (null? list)
      count
      (sum-num-rec (cdr list) (+ count (car list)))))

;Named let
(define (fact-let n)
  (let loop((n1 n) (p n))           ; 1
    (if (= n1 1)                    
	p
	(let ((m (- n1 1)))
	  (loop m (* p m))))))      ; 2

(define (remove-x-let list x)
  (let loop((l1 list) (new '()))
    (if (null? l1)
        (reverse new)
        (loop (cdr l1)
              (if (equal? x (car l1))
                          new
                          (cons (car l1) new))))))

(define (get-index-let list x)
  (let loop((l1 list) (i 0))
    (if (null? l1)
        #f
        (if (equal? (car l1) x)
            i
            (loop (cdr l1) (+ i 1))))))

(define (list-n n)
  (let loop((l1 '()) (i 0))
    (if (equal? i n)
        (reverse l1)
        (loop (cons i l1) (+ i 1)))))

;letrec
(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
		   (if (= n1 1)
		       p
		       (let ((m (- n1 1)))
			 (iter m (* p m)))))))     ; *
    (iter n n)))

(define (my-reverse-let-rec list)
  (letrec ((iter (lambda (list1 new)
            (if (null? list1)
                new
                (iter (cdr list1) (cons (car list1) new))))))
    (iter list '())))

;; do
;;(do binds (predicate value)
   ;; body)
;; [binds] → ((p1 i1 u1) (p2 i2 u2) ... )
(define (fact-do n)
  (do
      ((n1 n (- n1 1)) (p n (* p (- n1 1))))
    ((= n1 1) p)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                       

;HOF
(define (addLists l1 l2)
  (map + l1 l2))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exam Questions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Define a Scheme function SCATTER-GATHER which takes two
;arguments, a list INDICES of indices and a list VALS of values,
;and returns a list of the same length as INDICES but with each
;value K replaced by the K-th element of VALS, or if that is out of
;range, by #f.
;Example:
;(scatter-gather '(0 1 4 1 1 7 2) '(a b c d e))
;=> (a b e b b #f c)
(define (get-by-index list index)
  (if (> index (length list))
      #f
      (list-ref list index)))

(define (scatter-gather l1 l2)
  (let loop((l1rec l1)(output '()))
    (if (null? l1rec)
        (reverse output)
        (loop
         (cdr l1rec)
         (cons (get-by-index l2 (car l1rec)) output )))))



;Define a Scheme function CROSS-MAP which takes a binary function
;F and two lists XS and YS, and returns a list of the result of applying F
;to each possible pair of elements with the first taken from XS and the
;second from YS. (Order of elements in output is unspecified.)
;Example:
;(cross-map - '(100 200 300) '(4 3 2 1))
;⇒ ( 96 97 98 99 196 197 198 199 296 297 298 299)

(define (cross-map bin-fun list1 list2)
  (let loop((l1 list1) (output '()))
    (if (null? l1)
        (reverse output)
        (loop
         (cdr l1)
         (append
          (map
           (lambda (x) (bin-fun (car l1) x))
           (reverse list2))
          output)))))
         

         


;
;Q1: Scheme
; Define a Scheme function map-skip which takes a function and a
; list and returns the result of applying the given function to
; every other element of the given list, starting with the first
; element.
; Example:
; (map-skip (λ (x) (+ x 1000)) '(1 2 3 4 5 6))
; => (1001 2 1003 4 1005 6)

(define (map-skip fun list)
  (let loop ((l1 list) (new '()) (i 0))
        (if (null? l1)
            (reverse new)
              (loop (cdr l1)
                    (cons (if (equal? (modulo i 2) 0)
                         (fun (car l1))
                         (car l1)) new)
                    (+ i 1)))))
            



;Q1: Scheme
; Define a Scheme function tear which takes two arguments, a
; predicate p? and a list xs, and returns a list of two lists, the
; first of which is the elements of xs that pass p?, and the second
; of which is the elements of xs that fail it, both in order.
; Examples:
; (tear number? '(a b c 1 2 3 d e f))
; => ((1 2 3) (a b c d e f))
; (tear (lambda (x) (> x 5)) '(1 10 2 12 3 13))
; => ((10 12 13) (1 2 3))

;tail recursive
(define (maybe-filter pred list)
  (let loop((l1 list) (r1 '()) (r2 '()))
    (if (null? l1)
        (reverse (cons (reverse r1) (cons (reverse r2) '())))
        (if (pred (car l1))
            (loop (cdr l1) r1 (cons (car l1) r2))
            (loop (cdr l1) (cons (car l1) r1) r2)))))


;
;Define a Scheme function foo that takes two lists and yields a list combining all the
;elements in the two input lists, taking 1 from the first list, 2 from the second list, 3 from
;the first list, 4 from the second list, etc, until both are exhausted.
;Examples:
;(foo '(a b c d e f g) '(aa bb cc dd ee ff gg))
; => (a aa bb b c d cc dd ee ff e f g gg)
;(foo '(a b c d e f g) '())
; => (a b c d e f g)
;(foo '() '(aa bb cc dd ee ff gg))
; => (aa bb cc dd ee ff gg)
(define (drop list n)
  (let loop((l1 list) (i 0))
    (if (or (null? l1) (equal? i n))
        l1
        (loop (cdr l1) (+ i 1)))))

(define (take list n)
  (let loop((l1 list) (i 0) (out '()))
    (if (or (null? l1) (equal? i n))
        (reverse out)
        (loop (cdr l1) (+ i 1) (cons (car l1) out)))))

(define (foo list1 list2)
  (let loop((l1 list1) (l2 list2) (i 1) (output '()))
    (if (and (null? l1) (null? l2))
             output
             (loop (cdr l1) (cdr l2) (+ i 2)
                   (cons output (cons (take l1 i) (take l2 (+ i 1))))))))
                      
    


;1 Scheme
;Define a Scheme function foo which finds all atoms inside an sexpression which pass a given predicate.
;Examples:
; (foo number? '(a (2 (c 3) 4)))
; => (2 3 4)
; (foo symbol? '(a (2 (c 3) 4)))
; => (a c)
; (foo symbol? 'a)
; => (a)
; (foo number? 'a)
; => ()
(define (flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

(define (get-atoms pred list)
  (atoms pred (flatten list) '()))

(define (atoms pred list res)
  (cond
    ((null? list) (reverse res))
    ((pred (car list)) (atoms pred (cdr list) (cons (car list) res)))
    (else (atoms pred (cdr list) res))))


(define (before pred list)
  (let loop((l1 list) (output '()))
    (if (null? (cdr l1))
        (reverse output)
        (loop
         (cdr l1)
         (if (pred (cadr l1))
          (cons (car l1) output)
          output)))))
