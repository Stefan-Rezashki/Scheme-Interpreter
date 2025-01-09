#lang racket

;; =====================
;; REPL Driver
;; =====================
(define (read-evaluate-print-loop evaluator)
  (display "repl> ")
  (let ((user-input (read)))
    (cond
      [(eq? user-input 'quit)
       (display "Quitting the REPL...")
       (newline)]
      [else
       (write (evaluator user-input))
       (newline)
       (read-evaluate-print-loop evaluator)])))

;; =====================
;; Basic Tokenization
;; =====================
(define LPAR (list 'lparen-token))   ;; Unique token for '('
(define RPAR (list 'rparen-token))   ;; Unique token for ')'

(define (next-token)
  (let ([ch (read-char)])
    (cond
      [(or (eq? ch #\space) (eq? ch #\newline))
       (next-token)]            ;; Skip whitespace
      [(eq? ch #\() LPAR]
      [(eq? ch #\)) RPAR]
      [(char-alphabetic? ch)    (read-identifier ch)]
      [(char-numeric? ch)       (read-number ch)]
      [else
       (error "Unexpected character:" ch)])))

(define (read-identifier first-ch)
  (define (consume-identifier-chars so-far)
    (let ([peeked (peek-char)])
      (if (or (char-alphabetic? peeked) (char-numeric? peeked))
          (consume-identifier-chars (cons (read-char) so-far))
          (reverse so-far))))
  (string->symbol
   (list->string
    (consume-identifier-chars (list first-ch)))))

(define (read-number first-ch)
  (define (consume-number-chars so-far)
    (let ([peeked (peek-char)])
      (if (char-numeric? peeked)
          (consume-number-chars (cons (read-char) so-far))
          (reverse so-far))))
  (string->number
   (list->string
    (consume-number-chars (list first-ch)))))

;; =====================
;; Parsing S-expressions
;; =====================
(define (micro-read)
  (let ([tok (next-token)])
    (cond
      [(eq? tok LPAR)  (parse-list '())]
      [else            tok])))

(define (parse-list collected)
  (let ([tok (next-token)])
    (cond
      [(eq? tok RPAR)            (reverse collected)]
      [(eq? tok LPAR)
       (parse-list (cons (parse-list '()) collected))]
      [else
       (parse-list (cons tok collected))])))

;; =====================
;; Evaluation
;; =====================
(define global-env '()) ;; Our global environment

(define (lookup-global var)
  (let ([binding (assoc var global-env)])
    (if binding
        (cadr binding)
        (error "Undefined variable:" var))))

(define (bind-global! name val)
  (let ([bd (assoc name global-env)])
    (if bd
        (set! global-env
              (map (λ (pair)
                     (if (eq? (car pair) name)
                         (list name val)
                         pair))
                   global-env))
        (set! global-env
              (cons (list name val) global-env)))))

;; Add basic math and list operations to the environment
(bind-global! '+ +)
(bind-global! '- -)
(bind-global! '* *)
(bind-global! '/ /)
(bind-global! 'list (λ args args))
(bind-global! 'cons cons)
(bind-global! 'car car)
(bind-global! 'cdr cdr)

;; Main evaluator
(define (my-eval expr)
  (cond
    [(symbol? expr)        (lookup-global expr)]
    [(pair? expr)          (evaluate-list expr)]
    [(or (number? expr)
         (boolean? expr))  expr]
    [else                  (error "Unrecognized expression:" expr)]))

(define (evaluate-list expr)
  (cond
    [(and (symbol? (car expr))
          (special-form? (car expr)))  (handle-special-form expr)]
    [else                             (apply-function expr)]))

(define (special-form? maybe-sym)
  (member maybe-sym '(define lambda if cond)))

(define (handle-special-form expr)
  (let ([keyword (car expr)])
    (case keyword
      [(define)  (handle-define expr)]
      [(lambda)  (handle-lambda expr)]
      [(if)      (handle-if expr)]
      [(cond)    (handle-cond expr)]
      [else      (error "Unknown special form:" keyword)])))

;; =====================
;; Special Forms
;; =====================
(define (handle-define expr)
  (bind-global! (cadr expr) (my-eval (caddr expr))))

(define (handle-lambda expr)
  (let ([param-list (cadr expr)]
        [body       (cddr expr)])
    (λ args
      (eval-with-extended-env body
                              (extend-env param-list args global-env)))))

(define (handle-if expr)
  (let ([condition   (cadr expr)]
        [true-branch (caddr expr)]
        [false-branch (if (= (length expr) 4)
                          (cadddr expr)
                          #f)])
    (if (my-eval condition)
        (my-eval true-branch)
        (my-eval false-branch))))

(define (handle-cond expr)
  (define (process-clauses clauses)
    (if (null? clauses)
        (error "No matching clause in cond")
        (let ([cl (car clauses)])
          (if (eq? (car cl) 'else)
              (if (null? (cdr clauses))
                  (eval-sequence (cdr cl))
                  (error "Else clause must come last in cond"))
              (if (my-eval (car cl))
                  (eval-sequence (cdr cl))
                  (process-clauses (cdr clauses)))))))
  (process-clauses (cdr expr)))

;; Helper to evaluate a list of expressions in sequence
(define (eval-sequence exprs)
  (cond
    [(null? exprs)    (void)]
    [(null? (cdr exprs))
     (my-eval (car exprs))]
    [else
     (my-eval (car exprs))
     (eval-sequence (cdr exprs))]))


;; =====================
;; Environment Helpers
;; =====================
(define (extend-env params args old-env)
  (if (= (length params) (length args))
      (append (map list params args) old-env)
      (error "Mismatched number of parameters and arguments")))

(define (eval-with-extended-env body new-env)
  (let ([original-env global-env])
    (parameterize ([global-env (append new-env original-env)])
      (eval-sequence body))))

;; =====================
;; Function Application
;; =====================
(define (apply-function expr)
  (let ([operator (my-eval (car expr))]
        [operands (map my-eval (cdr expr))])
    (apply operator operands)))
;;======================
;;EXAMPLES
;;======================


