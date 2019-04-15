#lang racket

#||| Global Variables |||#
(define LAMBDA (string->symbol "\u03BB"))

#||| Helper functions |||#

#| Check the two types of lambda |#
(define (is-lambda x)
  (cond
    [
      (or (equal? x 'lambda) (equal? x LAMBDA))
      #t
    ]
    [
      else
      #f
    ]
  )
)

#| Check if keyword mismatch |#
(define (mismatch x y)
  (or
    (xor (equal? x 'if) (equal? y 'if)) 
    (xor (equal? x 'quote) (equal? y 'quote))
    (xor (equal? (is-lambda x) 'lambda) (equal? (is-lambda y) 'lambda))
  )
)

#| Check base cases (i.e. numeral equality, booleans, etc.) |#
(define (compare-base x y)
  (cond
    ;  Compare equal
    [
      (equal? x y)
      x
    ]
    ;  Compare boolean
    [
      (and (boolean? x) (boolean? y))
      (if x '% '(not %))
    ]
    ;  Compare not equal
    [
      else
      (list 'if '% x y)
    ]
  )
)

#| Use the shorter of the lambda symbols |#
(define (fix-lambda-symbol x y)
  (cond
    [
      (not (equal? x y))
      (list LAMBDA)
    ]
    [
      else
      (list x)
    ]
  )
)

#| Handle lambda case |#
(define (compare-lambda x y first)
  (cond
    [
      (and (list? x) (list? y))
        (cond
          [
            (and (equal? x empty) (equal? y empty))
            '()
          ]
          [
            (equal? (car x) (car y))
            (cons (car x) (compare-lambda (cdr x) (cdr y) #f))
          ]
          [
            else
              (cond
                [
                  (equal? first #t)
                  (cons (list 'if '% (car x) (car y)) (compare-lambda (cdr x) (cdr y) #f))
                ]
                [
                  else
                  (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))) (compare-lambda (cdr x) (cdr y) #f))
                ]
              )
          ]
        )
    ]
    [
      (list 'if '% x y)
    ]
  )
)

#| Lambda wrapper function |#
(define (compare-lambda-wrapper x y)
  (let
    (
      [lambda-x (list-ref x 0)]
      [lambda-y (list-ref y 0)]
      [args-x (cadr x)]
      [args-y (cadr y)]
      [expr-x (caddr x)]
      [expr-y (caddr y)]
    )
    (cond
      [
        (list? expr-x)
        (append (fix-lambda-symbol lambda-x lambda-y) (append (list (compare-lambda args-x args-y #t))
                                                          (list (compare-lambda expr-x expr-y #t))))
      ]
      [
        else
        (append (fix-lambda-symbol lambda-x lambda-y) (append (list (compare-lambda args-x args-y #t))
                                                          (compare-lambda expr-x expr-y #t)))
      ]
    )
  )
)

#| expr-compare |#
(define (expr-compare x y)
  (cond
    ;  Compare list
    [
      (and (list? x) (list? y))
        (cond
          ;  Compare equal
          [
            (equal? x y)
            x
          ]
          ;  Same length
          [
            (= (length x) (length y))
              (cond
                ;  Keyword mismatch (i.e. if, quote, lambda)
                [
                 (mismatch (car x) (car y))
                 (list 'if '% x y)
                ]
                ;  Leading quote
                [
                  (and (equal? (car x) 'quote) (equal? (car y) 'quote))
                  (compare-base x y)
                ]
                ;  Leading lambda
                [
                  (and (is-lambda (car x)) (is-lambda (car y)))
                  ;  Check lambda sign, args, and expr
                  (compare-lambda-wrapper x y)
                ]
                ;  Recurse
                [
                  else
                  (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))
                ]
              )
          ]
          ;  Different length
          [
            else
            (list 'if '% x y)
          ]
        )
    ]
    ;  Compare base
    [
      else 
      (compare-base x y)
    ]
  )
)

#| test-expr-compare |#
(define (test-expr-compare x y)
  (and
    (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
    (equal? (eval y) (eval (list 'let '((% #t)) (expr-compare x y))))
  )
)

#| test-expr-x |#
(define test-expr-x
  (list
    (+ 1 2)
    (quote (1 2))
    (lambda (x) (- x x) 10)
    (if #t 1 2)
  )
)

#| test-expr-y |#
(define test-expr-y
  (list
    (* 1 2)
    (quote (* 1 2))
    (lambda (x y) (+ x y) 10)
    (if #f 1 2)
    ''(1 2)
  )
)