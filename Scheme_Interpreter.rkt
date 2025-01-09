#lang racket

;; =====================
;; Global Environment
;; =====================
(define (create-env parent-env)
  "Create a new environment that extends a parent environment."
  (hash 'parent parent-env 'bindings (make-hash)))

(define (env-bind! env name value)
  "Bind a variable to a value in the current environment."
  (hash-set! (hash-ref env 'bindings) name value))

(define (env-lookup env name)
  "Look up a variable in the current environment or parent environments."
  (let loop ([current-env env])
    (cond
      [(null? current-env) (error "Undefined variable:" name)] 
      [(hash-has-key? (hash-ref current-env 'bindings) name)   
       (hash-ref (hash-ref current-env 'bindings) name)]
      [(eq? (hash-ref current-env 'parent) #f)                
       (error "Undefined variable:" name)]
      [else
       (loop (hash-ref current-env 'parent))])))              


;; Initialize the global environment
(define (initialize-global-env)
  "Initialize the global environment with built-in functions and variables."
  (let ([env (create-env #f)]) ; Global environment has no parent
    ;; Arithmetic operators
    (env-bind! env '+ +)
    (env-bind! env '- -)
    (env-bind! env '* *)
    (env-bind! env '/ /)
    (env-bind! env 'quotient quotient)
    (env-bind! env 'remainder remainder)
    ;; List operations
    (env-bind! env 'list (λ args args))
    (env-bind! env 'cons cons)
    (env-bind! env 'car car)
    (env-bind! env 'cdr cdr)
    ;; Comparison operators
    (env-bind! env '> >)
    (env-bind! env '< <)
    (env-bind! env '>= >=)
    (env-bind! env '<= <=)
    (env-bind! env '= =)
    env))


(define global-env (initialize-global-env))

;; =====================
;; Evaluation with Environments
;; =====================
(define (my-eval expr env)
  "Evaluate an expression in the given environment."
  (cond
    [(number? expr) expr]                        ; Numbers are self-evaluating
    [(boolean? expr) expr]                       ; Booleans are self-evaluating
    [(string? expr) expr]                        ; Strings are self-evaluating
    [(symbol? expr) (env-lookup env expr)]       ; Symbols are looked up in the environment
    [(pair? expr) (evaluate-list expr env)]      ; Lists are evaluated
    [else (error "Invalid expression:" expr)]))  ; Error for unsupported types


(define (evaluate-list expr env)
  "Evaluate a list expression."
  (cond
    [(and (symbol? (car expr)) (special-form? (car expr)))
     (handle-special-form expr env)]
    [else
     (apply-function (my-eval (car expr) env)
                     (map (λ (x) (my-eval x env)) (cdr expr)))]))

;; =====================
;; Special Forms
;; =====================
(define (special-form? symbol)
  (member symbol '(define lambda if cond and or not begin)))

(define (handle-special-form expr env)
  "Handle special forms like define, lambda, if, cond, and logical operations."
  (case (car expr)
    [(define) (handle-define expr env)]
    [(lambda) (handle-lambda expr env)]
    [(if) (handle-if expr env)]
    [(cond) (handle-cond expr env)]
    [(and) (handle-and expr env)]
    [(or) (handle-or expr env)]
    [(not) (handle-not expr env)]
    [(begin) (handle-begin expr env)]
    [else (error "Unknown special form:" (car expr))]))

(define (handle-begin expr env)
  "Evaluate a sequence of expressions in a begin block."
  (define (eval-sequence exprs)
    (cond
      [(null? exprs) (void)]                           ; No expressions to evaluate
      [(null? (cdr exprs)) (my-eval (car exprs) env)]   ; Last expression
      [else
       (my-eval (car exprs) env)                        ; Evaluate current expression
       (eval-sequence (cdr exprs))]))                   ; Continue with the rest
  (eval-sequence (cdr expr)))  ; Skip the `begin` symbol


(define (handle-define expr env)
  "Define a new variable in the current environment."
  (let ([name (cadr expr)]
        [value (my-eval (caddr expr) env)])
    (env-bind! env name value)))

(define (handle-if expr env)
  "Evaluate an if expression."
  (if (my-eval (cadr expr) env)
      (my-eval (caddr expr) env)
      (if (= (length expr) 4)
          (my-eval (cadddr expr) env)
          #f)))

(define (handle-cond expr env)
  "Evaluate a cond expression."
  (let loop ([clauses (cdr expr)])
    (cond
      [(null? clauses) (error "No matching clause in cond")]
      [(eq? (car (car clauses)) 'else)
       (if (null? (cdr clauses))
           (my-eval `(begin ,@(cdr (car clauses))) env)
           (error "Else clause must be last"))]
      [(my-eval (car (car clauses)) env)
       (my-eval `(begin ,@(cdr (car clauses))) env)]
      [else (loop (cdr clauses))])))

(define (handle-and expr env)
  "Evaluate an and expression."
  (define (eval-all clauses)
    (cond
      [(null? clauses) #t]                         ; All conditions are true
      [(not (my-eval (car clauses) env)) #f]       ; Short-circuit if false
      [else (eval-all (cdr clauses))]))            ; Continue evaluation
  (eval-all (cdr expr)))

(define (handle-or expr env)
  "Evaluate an or expression."
  (define (eval-any clauses)
    (cond
      [(null? clauses) #f]                         ; No conditions are true
      [(my-eval (car clauses) env) #t]             ; Short-circuit if true
      [else (eval-any (cdr clauses))]))            ; Continue evaluation
  (eval-any (cdr expr)))

(define (handle-not expr env)
  "Evaluate a not expression."
  (if (= (length expr) 2)
      (not (my-eval (cadr expr) env))
      (error "not requires exactly one argument")))

(define (handle-lambda expr env)
  "Create a lambda function with its own environment."
  (let ([params (cadr expr)]     ; List of parameters
        [body (cddr expr)])      ; Function body
    (λ args                      ; Lambda function takes a list of arguments
      (if (= (length params) (length args))
          (my-eval `(begin ,@body)
                   (extend-env params args env))   ; Extend environment properly
          (error "Parameter and argument count mismatch")))))

;; =====================
;; Environment Helpers
;; =====================
(define (extend-env params args parent-env)
  "Extend an environment with new parameter bindings."
  (let ([new-env (create-env parent-env)])
    (for ([param params] [arg args])  ; Map each parameter to its corresponding argument
      (env-bind! new-env param arg))
    new-env))



;; =====================
;; Tokenization and Parsing
;; =====================
(define LPAR (list 'lparen-token))   ;; Unique token for '('
(define RPAR (list 'rparen-token))   ;; Unique token for ')'

(define (next-token)
  (let ([ch (read-char)])
    (cond
      [(or (eq? ch #\space) (eq? ch #\newline)) (next-token)]
      [(eq? ch #\() LPAR]
      [(eq? ch #\)) RPAR]
      [(char-alphabetic? ch) (read-identifier ch)]
      [(char-numeric? ch) (read-number ch)]
      [else (error "Unexpected character:" ch)])))

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
;; Function Application
;; =====================
(define (apply-function func args)
  "Apply a function to a list of arguments."
  (if (procedure? func)
      (apply func args)  ; Apply function with unpacked arguments
      (error "Invalid function application:" func)))


;; =====================
;; REPL
;; =====================
(define (start-repl)
  "Start the REPL with the global environment."
  (let loop ()
    (display "repl> ")
    (let ([input (read)])
      (if (eq? input 'quit)
          (begin (display "Goodbye!") (newline))
          (begin
            (write (my-eval input global-env))
            (newline)
            (loop))))))




