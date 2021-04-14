; TEST RIG --------------------------------------

(define (assert= input expected)
  (if (equal? expected input)
      0
      (list 'Fail: 'Expected: expected 'actual: input)))

(define (assert!= input expected)
  (if (not (equal? expected input))
      0
      (list 'Fail: 'Expected: expected 'actual: input)))

; END TEST RIG ----------------------------------

; START Q1 --------------------------------------

(define (test-expand-where)
  (list
   (assert= (expand-where '(val (sqrt (+ x y)) where x 3 y 6)) '(let ((x 3) (y 6)) (sqrt (+ x y))))
   (assert= (expand-where '(val (sqrt (+ x y z)) where x 2 y 3 z 4)) '(let ((x 2) (y 3) (z 4)) (sqrt (+ x y z))))
   (assert= (expand-where '(value (sqrt (+ x y z)) where x 2 y 3 z 4)) #f)
   (assert= (expand-where '(val ((* x y z)) where x 2 y 3 z 4)) '(let ((x 2) (y 3) (z 4)) ((* x y z))))
   (assert= (expand-where '(val ((/ x y z)) where x 2 y 3 z 4)) '(let ((x 2) (y 3) (z 4)) ((/ x y z))))
   (assert= (expand-where '(val (sqrt (+ v x y z)) where v 1 x 2 y 3 z 4)) '(let ((v 1) (x 2) (y 3) (z 4)) (sqrt (+ v x y z))))
   (assert!= (expand-where '(val (sqrt (+ x y)) where x 3 y 6)) '(let ((x 3) (y 6)) (sqrt (- x y))))))

(define where-part
  (lambda (expr)
    (if (null? expr)
        #f ; fail case
        (if (equal? (car expr) 'where)
            (cdr expr) ; found case
            (where-part (cdr expr)))))) ; induction case

(define extract-where-exp
  (lambda (expr)
    (if (null? expr)
        '() ; base case
        (if (and (symbol? (car expr))
                 (not (null? (cadr expr))))
            (cons (list (car expr) (cadr expr))
                  (extract-where-exp (cdr expr)))
            (extract-where-exp (cdr expr))))))
  
(define expand-where
  (lambda (expr)
    (let ((where-exp (where-part expr)))
      (if (and (equal? (car expr) 'val)
               where-exp
               (not (null? (cadr expr)))) ; format check
          (list 'let (extract-where-exp where-exp) (cadr expr))
          #f))))
                                           
; END Q1 ----------------------------------------

; START Q2 --------------------------------------

(define (test-subseq-locs)
  (list
   (assert= (subseq-locs '(1 2 1 2 1 2) '(1 2)) '(0 2 4))
   (assert= (subseq-locs '(1 2 1 2 1 2 4 5 3) '(1 2)) '(0 2 4))
   (assert= (subseq-locs '(a b c c b a a a a) '(a b c)) '(0))
   (assert= (subseq-locs '(a b c c b a a a a) '(a b d)) '())
   (assert!= (subseq-locs '(a b c c b a a a a) '(a b d)) '(0))
   (assert= (subseq-locs '(1 2 1 2 1 2) '(1 2)) '(0 2 4))))

(define (subseq-locs list1 list2)
  (define (check-all list1 list2 i)
    (if (null? list1)
        '() ; base case
        (if (check-once list1 list2)
            (cons i (check-all (cdr list1) list2 (+ i 1))) ; store index & continue
            (check-all (cdr list1) list2 (+ i 1))))) ; continue
  (check-all list1 list2 0)) ;bind 0 as start index
         
(define (check-once list1 list2)
  (cond
    ((null? list2) #t) ; base case
    ((null? list1) #f) ; list hit the end & input not consumed
    ((equal? (car list1) (car list2))
     (check-once (cdr list1) (cdr list2))) ; induction case
    (else #f)))

; END Q2 --------------------------------------

; START Q3 -------------------------------------

(define val
  (lambda (x) x))

(define (test-fib)
  (list
   (assert= (fib 1 val) (val 1))
   (assert= (fib 10 val) (val 89))
   (assert= (fib 8 val) (val 34))
   (assert= (fib 5 val) (val 8))))

(define fib
  (lambda (n k)
    (cond
      ((= n 0) (k 1))
      ((= n 1) (k 1))
      (else (fib (- n 1)
                 (lambda (x)
                   (fib (- n 2)
                        (lambda (y)
                          (k (+ x y))))))))))

; END Q3 --------------------------------------

; START Q4 ------------------------------------

(define (test-ddx)
  (list
   (assert= (ddx '(+ 3 x)) 1)
   (assert= (ddx '(* (* 3 x) x)) '(* 6 x))
   (assert= (ddx '(* 2 4)) 0)
   (assert= (ddx '(cos (* 3 x))) '(* -3 (sin (* 3 x))))
   (assert= (ddx (ddx '(sin (* 3 x)))) '(* -9 (sin (* 3 x))))
   (assert= (ddx '(sin (* 3 x))) '(* 3 (cos (* 3 x))))
   (assert= (ddx '(+ 10 x)) 1)))


(define (get-num-part list)
  (if (null? list)
      #f
      (if (number? (car list))
          (car list)
          (get-num-part (cdr list)))))

(define (get-other-part list)
  (if (null? list)
      #f
      (if (not (number? (car list)))
          (car list)
          (get-other-part (cdr list)))))


(define (can-add list1 list2)
  (if (or (not (equal? (car list1) '*)) (not (equal? (car list2) '*)))
      #f
      (let ((num1 (get-num-part (cdr list1)))
            (num2 (get-num-part (cdr list1)))
            (other1 (get-other-part (cdr list1)))
            (other2 (get-other-part (cdr list2))))
        (if (and num1 num2 (equal? other1 other2))
            (list '* (+ num1 num2) other1)
            #f))))


(define (by-constant num i-list)
  (if (and (equal? (car i-list) '*))
      (let ((num-part (get-num-part (cdr i-list)))
            (other-part (get-other-part (cdr i-list))))
        (if (and num-part other-part)
            (list '* (* num num-part) other-part)
            #f))
      #f))
      
(define (simplifier e)
  (if (list? e)
      (if (= 2 (length e))
          e ; case for sin/cost
          (let ((sign  (car e))
                (op1 (simplifier (cadr e)))  
                (op2 (simplifier (caddr e))))    
            (cond
              ((equal? sign '+)
               (cond
                 ((and (number? op1) (number? op2) (+ op1 op2))) ; e.g (+ 1 2)
                 ((equal? op1 0) op2) ; remove 0 addition
                 ((equal? op2 0) op1) ; remove 0 addition
                 ((and (list? op1) (list? op2) (can-add op1 op2))) ; (+ (* 3 x) (* 3 x)) = (* 6 x)
                 (else (list sign op1 op2))))
              ((equal? sign '*)
               (cond
                 ((and (number? op1) (number? op2) (* op1 op2))) ; e.g (* 1 2)
                 ((or (equal? op1 0) (equal? op2 0)) 0) ; remove 0 multiplications
                 ((and (list? op1) (number? op2) (by-constant op2 op1))); (* N (* N V)) = ((* N N) (V))
                 ((and (list? op2) (number? op1) (by-constant op1 op2)));  (* (* N V) N) = ((* N N) (V))
                 ((and (list? op1) (equal? op2 1) op1)) ; e.g (* 1 2)
                 ((and (list? op2) (equal? op1 1) op2)) ; e.g (* 1 2)
                 (else (list sign op1 op2))))
              (else (list sign op1 op2)))))
      e))


(define op-table
  (list (list 'exp exp (lambda (u du) (list '* du (list 'exp u))))
        (list 'sin sin (lambda (u du) (list '* du (list 'cos u))))
        (list 'cos cos (lambda (u du)
                         (list '* du (list '* -1 (list 'sin u)))))
        (list 'negate (lambda (x) (* x -1)) (lambda (u du) (list 'negate du)))
        (list '+ (lambda (x y) (+ x y))
              (lambda (u v du dv) (list '+ du dv)))
        (list '* (lambda (x y) (* x y))
              (lambda (u v du dv)
                (list '+ (list '* u dv) (list '* du v))))))

(define lookup-op
  (lambda (op) (cadr (assoc op op-table))))

(define lookup-deriv-calc
  (lambda (op) (caddr (assoc op op-table))))

(define ddx
  (lambda (e)
    (cond ((equal? e 'x) 1)		; (d/dx)x = 1
          ((number? e) 0)		; (d/dx)c = 0
          (else
           (let ((op (car e))
                 (args (cdr e)))
             (simplifier (apply (lookup-deriv-calc op) ; plug simplifier to result
                                (append args
                                        (map ddx args)))))))))

; END Q4 --------------------------------------


; START Q5 -------------------------------------

; 5.1 ------------------------------------------

(define (test-iterate)
  (list
   (assert= (iterate sqrt 16 3) 1.4142135623730951)
   (assert= (iterate cdr '(a b c d e f g) 3) '(d e f g))
   (assert= (iterate cdr '(a b c) 3) '())
   (assert= (iterate (lambda(x) (+ x 1)) 16 3) 19)))

(define (iterate f x0 i)
  (if (equal? i 1)
      (f x0) ; base case
      (iterate f (f x0) (- i 1)))) ; induction case

;5.2 -------------------------------------------

(define (test-find-iter)
  (list
   (assert= (find-iter sqrt 16 1.4142135623730951 100) 3)
   (assert= (find-iter cdr '(a b c) '() 50) 3)
   (assert= (find-iter (lambda(x) (+ x 1)) 16 19 50) 3)
   (assert= (find-iter (lambda(x) (+ x 1)) 16 19 2) #f)
   (assert= (find-iter cdr '(a b c d e f g) '(f g) 100) 5)))

(define (find-iter f x0 x m)
  (define (get-i f x0 x m i)
    (if (equal? i m)
        #f ; fail case
        (if (equal? (iterate f x0 i) x)
            i ; success case
            (get-i f x0 x m (+ i 1))))) ; induction case
  (get-i f x0 x m 1))

;5.3 -------------------------------------------

(define (test-find-iter2)
  (list
   (assert= (find-iter2 cdr '(a b c d d d e f) 8 (lambda (xs) (cons 'd xs)) '(e f) 20) '(3 3))))

(define (find-iter2 f x0 mf g y0 mg)
  (define (get-i f x0 mf g y0 mg i)
    (if (equal? i mf)
        #f ; fail case
        (let ((check (verify f x0 mf g y0 mg i 1)))
          (if check
              (list i check)
              (get-i f x0 mf g y0 mg (+ i 1))))))
  (get-i f x0 mf g y0 mg 1))

(define (verify f x0 mf g y0 mg i j)
  (if (equal? j mg)
      #f
      (if (equal? (iterate f x0 i) (iterate g y0 j))
          j
          (verify f x0 mf g y0 mg i (+ j 1)))))

; END Q5 --------------------------------------       
            
