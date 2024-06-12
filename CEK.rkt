#lang racket

;; CEK machine

(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))

;; here are how you can make maps first-class functions
;; however, in this implementation I use hash-sets
(define (empty-map) (lambda (_) (error "nothing in this map")))
(define (ext-map map x v)
  (lambda (y) (if (equal? x y)
                  v
                  (map y))))

;; the lambda above is a closure so it implicitly
;; stores the variables (x in this case) and the value
;; in its environment and its not evaluated until it needs to be used!
;; racket's lazy evaluation

;; Environments are maps from variables to values
;; Values are closures in this case
(define (eval ς)
  (match ς
    ;; First case of Figure 1
    [`(,(? symbol? x) ,ρ ,k)
     (match (hash-ref ρ x)
      [`(clo ,lam ,ρ+) `(,lam ,ρ+ ,k)]
      [_ (error "did not match a closure--but expected to")])]
    ;; Second case of Figure 2
    [`((,e0 ,e1) ,ρ ,k)
     `(,e0 ,ρ (ar ,e1 ,ρ ,k))]
    ;; Third case of Figure 3
    [`(,v ,ρ (ar ,e ,ρ+ ,k))
     `(,e ,ρ+ (fn ,v ,ρ ,k))]
    ;; Fourth case of Figure 4
    [`(,v ,ρ (fn (lambda (,y) ,e+) ,ρ+ ,k))
     `(,e+ ,(hash-set ρ+ y `(clo ,v ,ρ)) ,k)]
    [_ (error "Done!")]))

(define (evaluate-all-steps e)
  (define (h ς)
    (displayln "Current state:")
    (pretty-print ς)
    (displayln "⟶")
    (h (eval ς)))
  (h `(,e ,(hash) mt)))

 (evaluate-all-steps '((lambda (x) x) (lambda (y) y)))
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (y) y)))    
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (x) (x x)))
;; (evaluate-all-steps '((lambda (x) x) ((lambda (y) y) (lambda (y) y))))

(define (store-join sto0 sto1)
  (foldl (λ (k sto+)
           (hash-set
            sto+
            k
            (set-union (hash-ref sto0 k (set))
                      (hash-ref sto1 k (set)))))
         (hash)
         (append (hash-keys sto0) (hash-keys sto1))))
