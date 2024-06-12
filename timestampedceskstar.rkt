#lang racket

;; Concrete Time-stamped CESK* machine
;; Eliminates recursion from two of the structures in the
;; CEK machine: environments and continuations.

;; The Kont component of the machine is replaced by a pointer (address)
;; to a continuation allocated in the store.

;; Bounds the set of possible addresses, which means
;; the store may need to re-use locations and multiple values
;; that reside in the same location approximates and joins the values

;; time-stamps are representations of the machine's history at
;; allocation time. 

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

;; Σ is the set of all possible t-CESK* machine states (for any input)

;; tick returns the next time based on an inputted machine state
;; tick⟨_, _, _, _,t⟩=t+1
(define (tick ς)
  (match ς
    [`(,expr ,ρ ,σ ,a ,t) (+ t 1)]
    ;[`(_ _ _ _ ,(? number? t)) (+ t 1)]
    [_ (error "input must be a machine state")]))

;; alloc allocates a fresh address for a variable binding to a closure
;; or for a continuation
;; alloc⟨_, _, _, _,t⟩=t
(define (alloc ς)
  (match ς
    [`(,expr ,ρ ,σ ,a ,t) (gen-new-addr)]
    ;[`(_ _ _ _ ,(? number? t)) (gen-new-addr)]
    [_ (error "input must be a machine state")]))

#;;; join two values in a store
(define (store-join sto0 sto1)
  (foldl (λ (k sto+)
           (hash-set
            sto+
            k
            (set-union (hash-ref sto0 k (set))
                      (hash-ref sto1 k (set)))))
         (hash)
         (append (hash-keys sto0) (hash-keys sto1))))

;; Fig 2. ς ⟼ t-CESK* ς'
(define (eval ς)
  (match ς
    ;; First case of Figure 3
    [`(,(? symbol? x) ,ρ ,σ ,a ,t)
     (match (hash-ref σ (hash-ref ρ x)) 
       [`(clo ,lam ,ρ+)
        (define u (tick ς))
        `(,lam ,ρ+ ,σ ,a ,u)]
       [_ (error "didn't match a closure-but expected to")])]
    
    ;; Second case of Figure 3
    [`((,e0 ,e1) ,ρ ,σ ,a ,t)
     (define b (alloc ς))
     (define u (tick ς))
     `(,e0 ,ρ ,(hash-set σ b `(ar ,e1 ,ρ ,a)) ,b ,u)]

    ;; Third case of Figure 3
    [`(,v ,ρ ,σ ,a ,t)
     (match (hash-ref σ a) 
       ;; if κ = ar(e,ρ,c)
       [`(ar ,e ,ρ ,c)
        (define b (alloc ς))
        (define u (tick ς))
        `(,e
          ,ρ
          ,(hash-set σ b `(fn ,v ,ρ ,c))
          ,b
          ,u)]
       ;; if κ = fn((λx.e),ρ′,c)
       [`(fn (lambda (,x) ,e) ,ρ+ ,c)
        (define b (alloc ς))
        (define u (tick ς))
        `(,e
          ,(hash-set ρ+ x b)
          ,(hash-set σ b `(clo ,v ,ρ))
            ,c
            ,u)] 
       [`mt (displayln "Done!")])]))

;; (displayln) returns a #<void>
 
(define (evaluate-all-steps e)
  (define a0 (gen-new-addr))
  (define new-σ (hash))
  (define t0 0)
  (define (h ς)
    (displayln "Current state:")
    (pretty-print ς) 
    (displayln "⟶")
    (h (eval ς)))
  (h `(,e ,(hash) ,(hash-set new-σ a0 'mt) ,a0 ,t0)))

;; IDENTITY FUNCTION (LAMBDA (X) X) AND OMEGA TERM
;; (evaluate-all-steps '((lambda (x) x) (lambda (y) y)))
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (y) y)))    
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (x) (x x)))
;; (evaluate-all-steps '((lambda (x) x) ((lambda (y) y) (lambda (y) y))))