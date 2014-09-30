;;;; COMP3109 Paradigms and Programming Languages 2014
;;;; Assignment 1
;;;; @author Hugh Purnell <hugh.purnell@gmail.com>
;;;; 310181941

#lang racket

(define operators '(s-var s-nand s-not))

(define (operator? e) (member e operators))

;;;; Task 1

(define (find-vars e)
  ;;;; returns a set (in list form) of all the varibles (symbols) in an expression, excluding the list of operator symbols
  (cond
    ;; empty list?
    ((null? e) null) 
    ; we need this because a recursive call of find-vars may return an empty list from the base case for a operator
    
    ;; list of expressions? find-vars for the first item, then recursively find-vars for the tail
    ((list? e)
     (remove-duplicates
      (append 
       (find-vars (first e)) 
       (find-vars (rest e)) 
       ))
     )
    
    ;; base case, single non list symbol
    ((symbol? e)   
     (if (not(operator? e)) 
         (list e) 
         null      ; appending an empty list to a list does nothing!
         )
     )  
    )
  )

;;;; Task 2

(define (env-lookup v env)
  ;;;; returns value from a environment dictionary (in pairs form)
  (first (dict-ref env v ))
  )

(define (transformer e) (lambda env
                          ;;;; takes a query e as an argument and recursively generates a function as an output
                          (cond
                            ;; base cases
                            ((boolean? e) e)
                            ((not (list? e) ) (env-lookup e env)) ; varible
                            ((equal? (length e) 1) (first e)) ; single varible in a list
                            
                            ;; s-var
                            ((equal? (first e) 's-var)
                             (env-lookup (second e) env))
                            
                            ;; s-not
                            ((equal? (first e) 's-not)
                             (not (apply (transformer (second e)) env ) )
                             )
                            
                            ;; s-nand
                            ((equal? (first e) 's-nand)
                             (
                              nand 
                              (apply (transformer (second e)) env)
                              (apply (transformer (third e)) env)
                              ))
                            
                            
                            )
                          ))

;;;; Task 3

(define (simplify e) 
  ;;;; transforms a query into a simplified query, based on a limited set of conditions
  ;; cases for simplification include: (where a, b, and e are expressions)
  ;; (s-not (s-var e) -> (s-not e)
  ;; (s-not (s-not e)) -> e
  ;; (s-not (s-nand (not a) (not b)) -> (s-nand a b)
  
  (cond
    ;; zero terms
    ; shouldn't reach this point
    ; ( (equal (length e) 0) e )
    
    ;; base case one term
    ( (symbol? e) e)
    ;( (symbol? e) (list 's-var 'e))

    ( (equal? (length e) 1) e)
    ;( (equal? (length e) 1) (list 's-var 'e))

    
    ; s-var?
    ( (s-var? e) (list 's-var (second e)))
    

    ( (s-not? e) (cond
                   ;; (s-not (s-var e) -> (s-not e)
                   ((s-var? (second e)) (list 's-not (second (second e)) ))
                   
                   ;; (s-not (s-not e)) -> e
                   ((s-not? (second e)) (simplify(second (second e) )))
                   
                   ;; (s-not (s-nand (s-not a) (s-not b)) -> (s-nand a b)
                   ((and (s-nand? (second e)) (s-not? (second (second e))) (s-not? (third (second e))))
                    (list 's-nand (simplify (second (second (second e)))) (simplify (second (third (second e))))) )
                   
                   ; do nothing, recursively simplify terms
                   (else (list 's-not (simplify (second e))) )
                   ))
    
    ; s-nand? do nothing, recursively simplify terms
    ((s-nand? e) (list 's-nand (simplify (second e)) (simplify (third e))))
    
    ))  

;;;; These helper functions return #t if the expression is a well formed s-operator application
(define (s-var? e) (cond
                     ((not (list? e)) #f)                     
                     ((equal? (length e) 2) (equal? (first e) 's-var))
                     (else #f)
                     ))
(define (s-not? e) (cond
                     ((not (list? e)) #f)                     
                     ((equal? (length e) 2) (equal? (first e) 's-not))
                     (else #f)
                     ))
(define (s-nand? e) (cond
                      ((not (list? e)) #f)
                      ((equal? (length e) 3) (equal? (first e) 's-nand))
                      (else #f)
                      ))



;;;; TESTING / PLAYING

(define my-environment '((a #f) (b #t) (c #f) ) )

(define test1 '(s-not a))
(define test2 '(s-var a))
(define test3 '(s-nand a b))

(define test4 '(s-not (s-not (s-not (s-not (s-not (s-var a)))))))
(define test5 '(s-nand (s-var a) (s-var b)))
(define test6 '(s-not (s-nand (s-not a) (s-not b))))

(define test7 '(s-nand (s-nand a b) c))

;(define tq (transformer test1 ))
;(apply tq my-environment)
