(define get-operator 
  (lambda (op env)
    (cond
       ( (eq? op '+) +)
       ( (eq? op '*) *)
       ( (eq? op '/) /)
       ( (eq? op '-) -)
       ( else (display "cs305: ERROR\n\n") (repl env))
    )
  )
)

(define is-operator? 
  (lambda (op)
    (cond
      ((eq? op '+) #t)
      ((eq? op '-) #t)
      ((eq? op '/) #t)
      ((eq? op '*) #t)
      (else #f)
    )
  )
)

(define define-stmt? 
  (lambda (e)
     ( and (list? e) (= (length e) 3) (equal? (car e) 'define) (symbol? (cadr e)) )
  )
)

(define if-stmt? 
  (lambda (e)
    ( and (list? e) (= (length e) 4) (equal? (car e) 'if) )
  )
)

(define let-stmt? 
  (lambda (e)
    ( and (list? e) (= (length e) 3) (equal? (car e) 'let) (is-let-correct? (cadr e)) )
  )
)

(define is-let-correct? 
  (lambda (e)
    (cond
      ( (not (list? e))         	            #f )
      ( (null? e)		                    #t )
      ( (not (= (length (car e)) 2))	            #f )	
      ( (= (length (car e)) 2)	is-let-correct?(cdr e) )	
    )
  )
)

(define lambda-stmt?
  (lambda (e)
    ( and (list? e) (equal? (car e) 'lambda) (formal-list? (cadr e)) (not (define-stmt? (caddr e))) ) 
  )
)

(define formal-list? 
  (lambda (e)
    ( and (list? e) (symbol? (car e)) (or (null? (cdr e)) (formal-list? (cdr e))) )
  )
)

(define get-value 
  (lambda (var env)
    (cond
      ( (null? env) (display "cs305: ERROR\n\n") (repl env))
      ( (equal? (caar env) var) (cdar env) )
      ( else (get-value var (cdr env)) )
    )
  )
)

(define extend-env 
  (lambda (var val old-env)
    (cons (cons var val) old-env)
  )
)

(define s7-interpret
  (lambda (e env)
    (cond
      ((number? e) e)
      ((symbol? e) (get-value e env))
      ((not (list? e)) (display "cs305: ERROR\n\n") (repl env)) 
      ( (= (length e) 1) (display "cs305: ERROR\n\n") (repl env) )
      ((null? e) e)
     
      ; if statement:
      ((if-stmt? e) 
        (if (eq? (s7-interpret (cadr e) env) 0)
          ( s7-interpret (cadddr e) env)
          ( s7-interpret (caddr e) env)
        )
      )
	
      ; let statement: 
      ((let-stmt? e)
        (let 
          (
            (names (map car  (cadr e)))
            (exprs (map cadr (cadr e)))
          )
          (let 
            (
              (vals (map (lambda (expr) (s7-interpret expr env)) exprs))
            )
            (let 
              (
                (new-env (append (map cons names vals) env))
              )
              (s7-interpret (caddr e) new-env))
          )
        )
      )

      ; lambda statement:
      ((lambda-stmt? e) e) 
      ((lambda-stmt? (car e))  
        (if (= (length (cadar e)) (length (cdr e)))
          (let*
            (
              (par (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
              (new-env (append (map cons (cadar e) par) env))
            )
            (s7-interpret (caddar e) new-env)
          )

          ((display "cs305: ERROR\n\n") (repl env))
        )
      )
      
      ; operations:  
      ((is-operator? (car e))
        (let
          (
            (operator (get-operator (car e) env))
            (operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
          )
          (apply operator operands)
        )
      )
      (else 
        (let* 
          (
            (result (s7-interpret (list (get-value (car e) env) (cadr e)) env))
          )
          result
        )      
      )    
    )
  )
)

(define repl 
  (lambda (env)
    (let*
      (
        (dummy1 (display "cs305> "))
        (expr (read))
        (new-env (if (define-stmt? expr)
                     (extend-env (cadr expr) (s7-interpret (caddr expr) env) env)
                     env))
        (val (if (define-stmt? expr)
                 (cadr expr) 
                 (s7-interpret expr env)))
        (dummy2 (display "cs305: "))
        (dummy3 (display val))
        (dummy4 (newline)) 
        (dummy5 (newline))
      )
      (repl new-env)
    )
  )
)

(define cs305 (lambda () (repl '())))
