#lang racket

;; CESK machine
;; Eliminates recursion from one of the structures in the
;; CEK machine: environments.

;; Type predicate of the language to be inputted into CESK machine
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))

;; generate a new address not previously in domain(store)
(define (gen-new-addr)
  (gensym 'addr))

;; Fig 2. ς ⟼ CESK ς'
(define (eval ς)
  (match ς
    ;; First case of Figure 2
    [`(,(? symbol? x) ,ρ ,σ ,k)
     ;; the environment stores addresses for variables,
     ;; plugging the address into the store should give you a closure (lambda + env)
     (match (hash-ref σ (hash-ref ρ x)) 
       [`(clo ,lam ,ρ+) 
        `(,lam ,ρ+ ,σ ,k)]
       [_ (error "didn't match a closure-but expected to")])]
    
    ;; Second case of Figure 2
    [`((,e0 ,e1) ,ρ ,σ ,k)
     `(,e0 ,ρ ,σ (ar ,e1 ,ρ ,k))]

    ;; Third case of Figure 2
    [`(,v ,ρ ,σ (ar ,e ,ρ+ ,k))
     `(,e ,ρ+ ,σ (fn ,v ,ρ ,k))]

    ;; Fourth case of Figure 2
    [`(,v ,ρ ,σ (fn (lambda (,x) ,e) ,ρ+ ,k))
     (define new-addr (gen-new-addr))
     `(,e
       ,(hash-set ρ+ x new-addr)
       ,(hash-set σ new-addr `(clo ,v ,ρ))
       ,k)]

    [_ (error "Done!")]))

(define (evaluate-all-steps e)
  (define (h ς)
    (displayln "Current state:")
    (pretty-print ς)
    (displayln "⟶")
    (h (eval ς)))
  (h `(,e ,(hash) ,(hash) mt)))
 
(evaluate-all-steps '((lambda (x) x) (lambda (y) y)))
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (y) y)))    
;; (evaluate-all-steps '((lambda (x) (x x)) (lambda (x) (x x)))
;; (evaluate-all-steps '((lambda (x) x) ((lambda (y) y) (lambda (y) y))))