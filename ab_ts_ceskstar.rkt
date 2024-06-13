#lang racket

; The abstract time-stamped CESK* machine!

;; The goal is to create a time-stamped CESK* machine that operates
;; over a finite state-space and it is allowed to be nondeterministic
;; (branch off into multiple possible states at each step in a program)

;; The abstracted variant comes from bounding the address space
;; of the store and the number of times available. By bounding
;; these sets, the state-space becomes finite, but for soundness,
;; an entry in the store may be forced to hold several values
;; simultaneously.

;; Stores now map an address to a set of storable elements
;; rather than a single value (set functions will be used now)

;; If a location in the store is re-used, the new value
;; (syntactic lambda) will be joined with the current set of values.

;; When a location is dereferenced, the analysis must consider
;; any of the values in the set as a result of the dereference.

;; At a given store location, the machine non-deterministically
;; chooses a particular value from the set for locations that
;; contain multiple storables (closures and continuations)

; (set->list)

;; need to find a way to have finite space in the store
;; so need to find a way to have a finite amount of addresses

;; Type predicate of the language to be inputted into a-t-CESK* machine
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))

;; generate a new address not previously in domain(store)
(define (gen-new-addr)
  (gensym 'addr))


;; κ ∈ Kont ::= mt | ar(e,ρ,a) | fn(v,ρ,a). (evaluation contexts!)
;; a-tick returns the next time based on an inputted machine state
;; tick⟨_, _, _, _,t⟩=t+1
(define (a-tick ς κ)
  (match ς
    [`(,expr ,ρ ,σ ,a ,t) (+ t 1)]
    ;[`(_ _ _ _ ,(? number? t)) (+ t 1)]
    [_ (error "input must be a machine state")]))

;; a-alloc allocates a fresh address for a variable binding to a closure
;; or for a continuation
;; alloc⟨_, _, _, _,t⟩=t
(define (a-alloc ς κ)
  (match ς
    [`(,expr ,ρ ,σ ,a ,t) (gen-new-addr)]
    ;[`(_ _ _ _ ,(? number? t)) (gen-new-addr)]
    [_ (error "input must be a machine state")]))

(define (find-clo-in-list lst)
  (match (car lst)
     [`(clo ,lam ,ρ+) (car lst)] ; if first element matches expression, return first element
     ['() #f]
     [_ (find-closure-in-list (cdr lst))]))

(define (find-cont-in-list lst)
  (match (car lst)
     ['mt (car lst)]
     [`(ar ,e ,ρ ,c) (car lst)]
     [`(fn (lambda (,x) ,e) ,ρ+ ,c) (car lst)]
     ['() #f]
     [_ (find-cont-in-list (cdr lst))]))

; (find-in-list '('(clo (lambda (x) x) (hash))) '(clo ,lam ,ρ+)) 

;; Fig 5. ς ⟼ a-t-CESK* ς'
(define (eval ς)
  (match ς
    
    ;; First case of Figure 5

     ;; check if x is bound to a value (closure) in the set 
     ;; corresponding to the address from the
     ;; environment, within the a-store
       
     ;; use helper function to iterate through set and find
     ;; element that matches a closure. 
    [`(,(? symbol? x) ,ρ ,σ ,a ,t)
     (define set-from-addr (set->list (hash-ref σ (hash-ref ρ x))))
     (let ([helper-result (find-clo-in-list set-from-addr)])
       (match helper-result
         [#f (error "closure not found in location specified by address")]
         [`(clo ,lam ,ρ+)
          (define κ (hash-ref σ a))
          (define u (a-tick ς κ))
          `(,lam ,ρ+ ,σ ,a ,u)]
         [_ (error "didn't match a closure-but expected to")]))]
    
    ;; Second case of Figure 5
    [`((,e0 ,e1) ,ρ ,σ ,a ,t)
     (define κ (hash-ref σ a))
     (define b (a-alloc ς κ))
     (define u (a-tick ς κ))
     `(,e0
       ,ρ
       ,(set-union (hash-ref σ b) (set `(ar ,e1 ,ρ ,a)))
       ,b
       ,u)]

    ;; Third case of Figure 5
    ;; remember a is pointer to a set of κ in the store 
    [`(,v ,ρ ,σ ,a ,t)
     (define b (a-alloc ς κ))
     (define u (a-tick ς κ))
     (define set-from-addr (set->list (hash-ref σ a)))
     (define helper-result (find-clo-in-list set-from-addr))
     (match (helper-result)
       ;; if κ = ar(e,ρ′,c)
       [`(ar ,e ,ρ+ ,c)
        `(,e
          ,ρ
          ,(set-union (hash-ref σ b) (set `(fn ,v ,ρ ,c)))
          ,b
          ,u)]
       ;; if κ = fn((λx.e),ρ′,c)
       [`(fn (lambda (,x) ,e) ,ρ+ ,c)
        `(,e
          ,(hash-set ρ+ x b)
          ,(set-union (hash-ref σ b) (set `(clo ,v ,ρ)))
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