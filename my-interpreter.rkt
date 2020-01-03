#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))

;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent);hastable gives the initial bindings
  (set! framenumber (+ 1 framenumber))
  (frame (- framenumber 1) hashtable parent))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)));global frame - number 0

;This interprets a program. It uses the following function processdef.++
(define (eval-program prog)
         (match prog
           [(pgm deflist) (begin
                            (processdef-list deflist (top))
                            (return-value-of-main (top)))]))


;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-set! (frame-bindings fr) v/f (eval-exp exp))]));the identifier for a function is bound to a closure
(define (processdef-list lst fr)
  (for ([a-def lst])
    (processdef a-def fr)))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (begin
                         (define frame-where-symbol-is (search exp (top)))
                         (if(emptyframe? frame-where-symbol-is)
                            (begin (displayln exp) (error "Symbol not found"))
                            (hash-ref (frame-bindings frame-where-symbol-is) exp)))]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [(pair? exp) (cons (eval-exp (car exp)) (eval-exp (cdr exp)))];added on own
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam varlist _) (closure exp (top))];because lambda points to the environment where its created when a lambda is created it must be a closure
                [(app exp1 explist) (let*;exp1 is a symbol, explist are the arguments
                                        ([the-closure (eval-exp exp1)]
                                         [the-lambda (closure-lambda the-closure)]
                                         [the-frame-where-the-function-call-points-to (closure-frame the-closure)]
                                         [argument-list (map (lambda(x) (eval-exp x)) explist)])
                                      (begin
                                        (define fun-call-frame
                                          (createframe (make-hash
                                                        (map (lambda(x y) (cons x y)) (lam-varlist the-lambda) argument-list))
                                                       the-frame-where-the-function-call-points-to))
                                        (push fun-call-frame)

                                        (define ans (eval-exp (lam-exp the-lambda)))
                                        (pop)
                                        ans))]
                [(iff cond exp1 exp2) (if (eval-exp cond) (eval-exp exp1) (eval-exp exp2))]
                [(sett var exp-inside-sett) (cond
                                              [(emptyframe? (search var (top))) (begin
                                                                                  (displayln var)
                                                                                  (error "Symbol not found"))]
                                              [else (hash-set! (frame-bindings (search var (top))) var (eval-exp exp-inside-sett))])]
                [(lett deflist exp-in-lett) (begin
                                              (define new-frame (createframe (make-hash '()) (top)))
                                              (processdef-list deflist new-frame)
                                              (push new-frame)
                                              (define ans (eval-exp exp-in-lett))
                                              (pop)
                                              ans)]
                [(lets deflist exp-in-lets) (eval-exp (process-lets deflist exp-in-lets))]
                [(beginexp explist) (process-beginexp explist)]
                [(defexp deflist exp)
                 (begin
                   (map (lambda(x) (processdef x (top))) deflist)
                   (eval-exp exp))]
                [(debugexp)
;                 (begin
;                   (print-current-environment (top)))
                 (begin
                   (vector-set! stacks stacksindex stack)
                   (set! stacksindex (+ 1 stacksindex)))
                            ])]))

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (if(null? (cdr explist))
     (eval-exp (car explist))
     (begin
       (eval-exp (car explist))
       (process-beginexp (cdr explist))))
;  (begin
;    (for ([exp-in-begin (take explist (- (length explist) 1))])
;                                        (eval-exp exp-in-begin))
;    (eval-exp (last explist)))
  )

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (cond
    [(null? deflist) exp]
    [(null? (cdr deflist)) (lett deflist exp)]
    [else (lett (list (car deflist)) (process-lets (cdr deflist) exp))]))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
   (cond
     ;[(= 0 (frame-number fr)) (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@") (displayln fr))]
     [(emptyframe? fr) (displayln "@@@@@@@@@@@@@@@@@@@@@@@")]
     [else (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@") (displayln fr) (print-current-environment (frame-parent fr)))]))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.
(define (search sym fr)
  (cond
    [(emptyframe? fr) fr]
    [(hash-has-key? (frame-bindings fr) sym) fr]
    [else (search sym (frame-parent fr))]))
               
(define (cleanup)
  (set! stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))

