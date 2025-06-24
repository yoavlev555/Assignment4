#lang racket

(provide (all-defined-out))

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))

(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

(define leaf? (lambda (x) (not (list? x))))

;; Signature: map-lzl(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define map-lzl
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (map-lzl f (tail lzl)))))))

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

; Signature: nth(lz-lst,n)
;; Type: [LzL*Number -> T]
;; Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (- n 1)))))


;;; Q3.1
; Signature: append$(lst1, lst2, cont) 
; Type: [List * List * [List -> T]] -> T
; Purpose: Returns the concatination of the given two lists, with cont pre-processing
(define append$
  (lambda (lst1 lst2 cont)
    (if (empty? lst1)
      (cont lst2) 
      (append$ (cdr lst1) lst2 
        (lambda (append_res)
          (cont (cons (car lst1) append_res))
        )
      )
    )
  )
)

(define len$
  (lambda (lst n) 
    (if (empty? lst)
      n
      (len$ (cdr lst) (+ n 1))
    )
  )
)

(define len 
  (lambda (lst)
    (len$ lst 0)
  )
)

;;; Q3.2
; Signature: equal-trees$(tree1, tree2, succ, fail) 
; Type: [Tree * Tree * [Tree ->T1] * [Pair->T2]] -> T1 U T2
; Purpose: Determines the structure identity of a given two lists, with post-processing succ/fail
(define equal-trees$ 
  (lambda (tree1 tree2 succ fail)
    (if (empty? tree1)
      (if (empty? tree2)
        (succ '())
        (fail (cons '() tree2))
      )
      (if (empty? tree2)
        (fail (cons tree1 '()))
        (if (or (and (leaf? tree1) (not (leaf? tree2))) (and (leaf? tree2) (not (leaf? tree1))))
          (fail (cons tree1 tree2))
          (if (and (leaf? tree1) (leaf? tree2))
            (succ (cons tree1 tree2))
            (equal-trees$ (car tree1) (car tree2)
              (lambda (res1) 
                (equal-trees$ (cdr tree1) (cdr tree2) 
                  (lambda (res2) (succ (cons res1 res2)))
                  (lambda (err) (fail err)))
              )
              (lambda (err) (fail err))
            )
          )
        )
      )
    )
  )
)



;;; Q4.1

;; Signature: as-real(x)
;; Type: [ Number -> Lzl(Number) ]
;; Purpose: Convert a rational number to its form as a
;; constant real number
(define as-real
  (lambda (x)
    (cons x (lambda () (as-real x)))
  )
)


;; Signature: ++(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Addition of real numbers
(define ++
  (lambda (x y)
    (cons (+ (head x) (head y)) (lambda () (++ (tail x) (tail y))))
  )
)

;; Signature: --(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Subtraction of real numbers
(define --
  (lambda (x y)
    (cons (- (head x) (head y)) (lambda () (-- (tail x) (tail y))))
  )
)

;; Signature: **(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Multiplication of real numbers
(define **
  (lambda (x y)
    (cons (* (head x) (head y)) (lambda () (** (tail x) (tail y))))
  )
)
;; Signature: //(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Division of real numbers
(define //
  (lambda (x y)
    (cons (/ (head x) (head y)) (lambda () (// (tail x) (tail y))))
  )
)

;;; Q4.2.a
;; Signature: sqrt-with(x y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Lzl(Number)) ]
;; Purpose: Using an initial approximation `y`, return a 
;; sequence of real numbers which converges into the 
;; square root of `x`
(define sqrt-with
  (lambda (x y)
    (cons-lzl y
      (lambda ()
        (let ((nexty (// (++ (** y y) x) (** (as-real 2) y))))
          (sqrt-with x nexty)))))
)


(define diag-lst
  (lambda (lzl n)
    (cons-lzl (nth (head lzl) n) (lambda () (diag-lst (tail lzl) (+ n 1))))
  ) 
)


;;; Q4.2.b
;; Signature: diag(lzl)
;; Type: [ Lzl(Lzl(T)) -> Lzl(T) ]
;; Purpose: Diagonalize an infinite lazy list
(define diag
  (lambda (lzl)
    (diag-lst lzl 0)
  )
)


;;; Q4.2.c
;; Signature: rsqrt(x)
;; Type: [ Lzl(Number) -> Lzl(Number) ]
;; Purpose: Take a real number and return its square root
;; Example: (take (rsqrt (as-real 4.0)) 6) => '(4.0 2.5 2.05 2.0006097560975613 2.0000000929222947 2.000000000000002)
(define rsqrt
  (lambda (x)
    (diag (sqrt-with x (as-real 1)))
  )
)
