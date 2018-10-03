#lang scheme
                                        ;
; standard recursive definition
(define (fac-rec x)
  (if (eq? x 0)
      1
      (* x (fac-rec (- x 1)))))

(fac-rec 3)


; Lists
(car '(this is a list of symbols))
(cdr '(this is a list of symbols))
(cdr '(singleton))
(list 'a 'b 'c 'd 'e)
(cadddr '(this is a list of symbols))

(cons 'a 'b)

(define (ismember x xs)
    (cond
      ((null? xs) #f)
      ((eq? x (car xs)) xs)
      (else (ismember x (cdr xs)))))

(ismember 2 '(1 2 3 4))
(ismember 2 '(1 3 4))

(define (sqr x) (* x x))

; Standard predicates
(symbol? 'sqr)
(number? 'sqr)
(pair? '())
(pair? '(1 2 3))
(list? '())
(list? (cons 'a 'b))
(pair? (cons 'a 'b))



; Functional arguments
(define (map-unary fun xs)
  (cond
    ((null? xs) '())
    (else (cons (fun (car xs))
                (map-unary fun (cdr xs))))))

(map-unary sqr '(1 2 3 4))
(map-unary (lambda (x) (* x x)) '(1 2 3 4))
(map-unary sqr (map sqr '(1 2 3 4)))

(map * '(1 2 3 4) '(1 2 3 4))


; Locals
(let ((a 3) (b 4) (square (lambda (x) (* x x))) (plus +))
     (sqrt (plus (square a) (square b))))

(let* ((a 3) (b 4) (sqra (* a a)) (sqrb (* b b)))
     (sqrt (+ sqra sqrb)))

; this fails because factorial is not yet bound
; (let ((factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1)))))))
;   (factorial 5))

; this works
(letrec ((factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1)))))))
  (factorial 5))


