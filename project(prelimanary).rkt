#lang racket

(define (rep-eval-print-loop evaluator)
  (display "repl> ")
  (let ((expr (read)))
    (cond
      ((eq? expr 'exit) ; Exit condition
       (display "Exiting the loop...")
       (newline))
      (else
       (write (evaluator expr)) ; Evaluate and print the result
       (newline)
       (rep-eval-print-loop evaluator))))) ; Tail-recursive call

(define (read-token)
  (let ((first-char (read-char))) ; Read the next character
    (cond
      ((char-whitespace? first-char) ; Ignore whitespace
       (read-token))
      ((eq? first-char #\() ; Detect '('
       left-parenthesis-token)
      ((eq? first-char #\)) ; Detect ')'
       right-parenthesis-token)
      ((char-alphabetic? first-char) ; Start of an identifier
       (read-identifier first-char))
      ((char-numeric? first-char) ; Start of a number
       (read-number first-char))
      (else
       (error "Illegal syntax"))))) ; Invalid character

(define (char-whitespace? char)
  (or (eq? char #\space) (eq? char #\newline))) ; Check for space or newline

(define left-parenthesis-token (list '*left-parenthesis-token*)) ; Unique token for '('
(define right-parenthesis-token (list '*right-parenthesis-token*)) ; Unique token for ')'

(define (token-leftpar? token)
  (eq? token left-parenthesis-token))

(define (token-rightpar? token)
  (eq? token right-parenthesis-token))

(define (read-identifier char1)
  (define (read-identifier-helper list-so-far)
    (let ((next-char (peek-char)))
      (if (or (char-alphabetic? next-char) (char-numeric? next-char))
          (read-identifier-helper (cons (read-char) list-so-far))
          (reverse list-so-far))))
  (string->symbol (list->string(read-identifier-helper (list char1)))))

(define (read-number char1)
  (define (read-number-helper list-so-far)
    (let ((next-char (peek-char)))
      (if (char-numeric? next-char)
          (read-number-helper (cons (read-char) list-so-far))
          (reverse list-so-far))))
  (string->number (list->string (read-number-helper (list char1)))))


(define (micro-read)
  (let ((next-token (read-token)))
    (cond ((token-leftpar? next-token)
           (read-list '()))
          (else
           next-token))))

(define (read-list list-so-far)
  (let ((token (read-token)))
    (cond ((token-rightpar? token)
           (reverse list-so-far)) ; Closing parenthesis ends the list
          ((token-leftpar? token)
           (read-list (cons (read-list '()) list-so-far))) ; Nested list
          (else
           (read-list (cons token list-so-far)))))) ; Add token to the list

(define (math-eval expr)
  (cond
    ((number? expr)
     expr)
    (else(math-eval-combo expr))))

(define (math-eval-combo expr)
  (let ((operator (eval-variable (car expr))) ; Look up operator
        (args (map math-eval (cdr expr)))) ; Evaluate all arguments
    (apply operator args))) ; Apply operator to all arguments



(define (eval expr)
  (cond
    ((symbol? expr)
     (eval-variable expr))
    ((pair? expr)
     (eval-list expr))
    ((self-evaluating? expr)
     expr)
    (else
     (error "Illegal expression:" expr))))

(define (self-evaluating? expr)
  (or (number? expr) (boolean? expr)))

(define (eval-list expr)
  (if (and (symbol? (car expr)) (special-form-name? expr))
      (eval-special-form expr)
      (math-eval-combo expr)))
(define (special-form-name? expr)
  (member (car expr) '(if define lambda cond)))


(define (eval-special-form expr)
  (let ((name (car expr)))
    (cond ((eq? name 'define)
           (eval-define expr))
          ((eq? name 'lambda)
           (eval-lambda expr))
          ((eq? name 'if)
           (eval-if expr))
          ((eq? name 'cond)
           (eval-cond expr)))))

(define (eval-if expr)
  (let ((expr-length (length expr)))
    (if (eval (cadr expr))
        (eval (caddr expr))
        (if (= expr-length 4)
            (eval (cadddr expr))
            #f))))

(define toplevel-envt '()) ; Use an immutable list

(define (toplevel-bind! name value)
  (let ((bdg (assoc name toplevel-envt)))
    (if bdg
        (set! toplevel-envt
              (map (lambda (pair)
                     (if (eq? (car pair) name)
                         (list name value)
                         pair))
                   toplevel-envt)) ; Replace the matching pair
        (set! toplevel-envt
              (cons (list name value) toplevel-envt))))) ; Add new binding

(define (toplevel-get name)
  (let ((binding (assoc name toplevel-envt)))
    (if binding
        (cadr binding)
        (error "Undefined variable:" name))))

(toplevel-bind! '+ +)
(toplevel-bind! '- -)
(toplevel-bind! '* *)
(toplevel-bind! '/ /)
(toplevel-bind! 'list (lambda args args)) ; Create a list of arguments
(toplevel-bind! 'cons cons)              ; Prepend an element to a list
(toplevel-bind! 'car car)                ; Get the first element of a list
(toplevel-bind! 'cdr cdr)                ; Get the rest of the list


(define (eval-variable symbol)
  (toplevel-get symbol))

(define (eval-define expr)
  (toplevel-bind! (cadr expr)
                  (eval (caddr expr))))

(define (eval-lambda expr)
  (let ((params (cadr expr))
        (body (cddr expr))) ; Handle multiple expressions
    (lambda args
      (eval `(begin ,@body) (extend-env params args toplevel-envt)))))


(define (extend-env params args env)
  (if (= (length params) (length args))
      (append (map cons params args) env) ; Combine params and args, then append to existing env
      (error "Parameter and argument count mismatch")))

(define (eval-cond expr)
  (define (evaluate-clauses clauses)
    (if (null? clauses)
        (error "No matching condition in cond expression")
        (let ((clause (car clauses)))
          (if (eq? (car clause) 'else)
              (if (null? (cdr clauses))
                  (eval `(begin ,@(cdr clause))) ; Evaluate all expressions in the body
                  (error "Else clause must be the last clause"))
              (if (eval (car clause))
                  (eval `(begin ,@(cdr clause))) ; Evaluate all expressions in the body
                  (evaluate-clauses (cdr clauses))))))) ; Otherwise, try the next clause
  (evaluate-clauses (cdr expr))) ; Skip the 'cond keyword

