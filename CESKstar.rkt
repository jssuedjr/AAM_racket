#lang racket
;; values (v) are syntactic lambdas
;; CESK* machine
;; Eliminates recursion from two of the structures in the
;; CEK machine: environments and continuations.

;; The Kont component of the machine is replaced by a pointer (address)
;; to a continuation allocated in the store. 

;; Type predicate of the language to be inputted into CESK* machine
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))

;; generate a new address not previously in domain(store)
(define (gen-new-addr)
  (gensym 'addr))

;; Fig 3. ς ⟼ CESK* ς'
(define (eval ς)
  (match ς
    ;; First case of Figure 3
    [`(,(? symbol? x) ,ρ ,σ ,a)
     (match (hash-ref σ (hash-ref ρ x)) 
       [`(clo ,lam ,ρ+) 
        `(,lam ,ρ+ ,σ ,a)]
       [_ (error "didn't match a closure-but expected to")])]
    
    ;; Second case of Figure 3
    [`((,e0 ,e1) ,ρ ,σ ,a)
     (define b (gen-new-addr))
     `(,e0 ,ρ ,(hash-set σ b `(ar ,e1 ,ρ ,a)) ,b)]

    ;; Third case of Figure 3
    [`(,v ,ρ ,σ ,a)
     (match (hash-ref σ a) 
       ;; if κ = ar(e,ρ′,c)
       [`(ar ,e ,ρ+ ,c)
        (define b (gen-new-addr))
        `(,e
          ,ρ+
          ,(hash-set σ b `(fn ,v ,ρ ,c))
          ,b)]
       ;; if κ = fn((λx.e),ρ′,c)
       [`(fn (lambda (,x) ,e) ,ρ+ ,c)
        (define b (gen-new-addr))
        `(,e
          ,(hash-set ρ+ x b)
          ,(hash-set σ b `(clo ,v ,ρ))
            ,c)]
     
       [`mt (displayln "Done!")])]))

;; (displayln) returns a #<void>

(define (evaluate-all-steps e)
  (define new-addr (gen-new-addr))
  (define new-σ (hash))
  (define (h ς)
    (displayln "Current state:")
    (pretty-print ς) 
    (displayln "⟶")
    (h (eval ς)))
  (h `(,e ,(hash) ,(hash-set new-σ new-addr 'mt) ,new-addr)))

;; IDENTITY FUNCTION (LAMBDA (X) X) AND OMEGA TERM
;; (evaluate-all-steps '(lambda (x) x))
;; (evaluate-all-steps 'x) <--- doesn't apply, all inputs are lambdas
;; (evaluate-all-steps '((lambda (x) x) (lambda (y) y)))
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (y) y)))    
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (x) (x x)))
;; (evaluate-all-steps '((lambda (x) x) ((lambda (y) y) (lambda (y) y))))