#lang scheme

; Church Booleans
(define true (lambda (x y) x))
(define false (lambda (x y) y))

(define and (lambda (a b) (a b false)))
(define or (lambda (a b) (a true b)))
(define not (lambda (b) (b false true)))

(define ite (lambda (b x y) (b x y)))

(define evalbool (lambda (b) (b #t #f)))

(evalbool (and true false))
(evalbool (and true true))

; Church Numerals
(define zero (lambda (s) (lambda (z) z)))
(define one (lambda (s) (lambda (z) (s z))))
(define two (lambda (s) (lambda (z) (s (s z)))))
(define three (lambda (s) (lambda (z) (s (s (s z))))))

(define evalnum (lambda (n) ((n (lambda (x) (+ x 1))) 0)))

(define iszero (lambda (n) ((n (lambda (x) false)) true)))
(define succ (lambda (n) (lambda (s) (lambda (z) (s ((n s) z))))))
(define plus (lambda (m) (lambda (n) ((m succ) n))))
(define mult (lambda (m) (lambda (n) ((m (plus n)) zero))))
(define expo (lambda (m) (lambda (n) (n m))))

(define pair (lambda (x y) (lambda (b) (b x y))))
(define fst (lambda (p) (p true)))
(define snd (lambda (p) (p false)))

(define pred (lambda (n) (snd ((n (lambda (p) (pair (succ (fst p)) (fst p)))) (pair zero zero)))))

(define four ((plus two) two))
(define five ((plus two) three))

(evalnum (succ zero))
(evalnum ((mult three) two))
(evalbool (iszero ((mult zero) two)))
(evalbool (iszero (pred two)))
(evalbool (iszero (pred one)))
(evalnum (pred five))

; Functional for factorial
(define Fac (lambda (fac) (lambda (n) ((iszero n) one ((mult n) (fac (pred n)))))))


; Y Combinator
(define Y (lambda (F) ((lambda (X) (F (X X))) (lambda (X) (F (X X))))))

; this does not terminate because of applicative-order evaluation of the Y combinator
;(define fac (Y Fac))
;(evalnum (fac three))

; Y Combinator (version for applicative-order)
; the additional lambda abstractions prevent the premature evaluation of the Y combinator
(define fix
  (lambda (F)
      ((lambda (X)
         (F (lambda (n) (lambda (f) (lambda (x) ((((X X) n) f) x))))))
       (lambda (X)
         (F (lambda (n) (lambda (f) (lambda (x) ((((X X) n) f) x)))))))))

; The factorial function
(define fac (fix Fac))

(evalnum (fac three))
(evalnum (fac four))
(evalnum (fac five))

