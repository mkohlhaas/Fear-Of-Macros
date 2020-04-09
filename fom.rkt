#lang racket

(require (for-syntax racket/match racket/string racket/syntax))
(require racket/stxparam racket/splicing rackunit)

; -------------------------------------------------------------------------------
; 3.1 What is a syntax transformer?

; A "transformer" is a function that changes a syntax object. Syntax object in, (changed) syntax object out.
; Using define-syntax tells the Racket compiler, "Whenever you encounter a chunk of syntax starting with foo, please give
; it to my transformer function, and replace it with the syntax I give back to you."
; The compiler always deals with syntax objects.
(define-syntax foo
  (λ(stx)
    (syntax "I am foo")))

; repl
; (syntax foo)
; (foo)
; foo

(define-syntax (also-foo stx)
  (syntax "I am also foo"))

; repl
; (also-foo)

; Shorthand for syntax is #'.
(define-syntax (quoted-foo stx)
  #'"I am also foo, using #' instead of syntax")

; repl
; (quoted-foo)

(define-syntax (say-hi stx)
  #'(displayln "hi"))

; repl
; (say-hi)

; -------------------------------------------------------------------------------
; 3.2 What’s the input?

(define-syntax (show-me stx)
  (print stx)
  #'(void))

; repl
; (show-me '(+ 1 2))
; (void 1 2 3 "123" 'whatever)

(define stx #'(if x (list "true") #f))
(define stx1 #'(reverse-me "backwards" "am" "i" values))
(define stx2 #'(our-if-v2 #t "true" "false"))

; repl
; stx
; stx1
; stx2
; (syntax-source stx)
; (syntax-line stx)
; (syntax-column stx)
; Converts syntax object completely/recursively into S-expression.
; (syntax->datum stx)
; syntax-e and syntax-list go only one level deep. The content of the ordinary list are syntax objects.
; (syntax-e stx)
; (syntax->list #'(1 (+ 3 4) 5))
; (syntax-e #'(1 (+ 3 4) 5))
; Here is the difference between syntax-e and syntax->list.
; (syntax-e #'a)
; (syntax->list #'a)

; -------------------------------------------------------------------------------
; 3.3 Actually transforming the input

; Now let's actually change the input.
(define-syntax (reverse-me stx)
  (print stx)
  (printf "\n")
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
;                                               ^^^ = (reverse-me stx) !!!

; repl
; (reverse-me "backwards" "am" "i" values)
; (values "i" "am" "backwards")

; -------------------------------------------------------------------------------
; 3.4 Compile time vs. run time

(define-syntax (foo1 stx)
  (make-pipe) ;Ce n'est pas le temps d'exécution
  #'(void))

; repl
; make-pipe won't be called
; (foo1)

(define (our-if condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

; repl
#;
(our-if #t
        "true"
        "false")

(define (display-and-return x)
  (displayln x)
  x)

; repl
; Racket has strict argument evaluation. All expressions are evaluated.
#;
(our-if #t
        (display-and-return "true")
        (display-and-return "false"))

(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  ; Using quasiquotation.
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))

; repl
; The content of the quasiquotations can/will be syntax objects.
; (define xs (syntax->list stx))
; `(cond [,(cadr xs) ,(caddr xs)] [else ,(cadddr xs)])

; repl
#;#;
(our-if-v2 #t
           (display-and-return "true")
           (display-and-return "false"))

(our-if-v2 #f
           (display-and-return "true")
           (display-and-return "false"))

; Let's use pattern matching (instead of cadr, caddr, cadddr and the like).
(define-syntax (our-if-using-match stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

; repl
; (our-if-using-match #t "true" "false")

; -------------------------------------------------------------------------------
; 3.5 begin-for-syntax

; For creating helper functions at compile time use begin-for-syntax or define-for-syntax in simple cases.
#;#;
(begin-for-syntax
  (define (my-helper-function ....)
    ....))
(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)

#;#;
(define-for-syntax (my-helper-function ....)
  ....)
(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)

; -------------------------------------------------------------------------------
; 4 Pattern matching: syntax-case and syntax-rules

; Supply a "template", which uses variables from the pattern.
(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))

; define-syntax-rule is a shorthand for simple/single pattern-matching case.
; define-syntax-rule expands to syntax-case.
(define-syntax-rule (our-if-using-syntax-rule condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

; -------------------------------------------------------------------------------
; 4.1 Pattern variable vs. template—fight!

#;
(define-syntax (hyphen-define/wrong1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" a b))])
       #'(define (name args ...)
           body0 body ...))]))

; using syntax (#') on pattern variables
(define-syntax (hyphen-define/wrong1.1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" #'a #'b))])
       #'(define (name args ...)
           body0 body ...))]))

; repl
; (hyphen-define/wrong1.1 foo bar () #t)
; (foo-bar)

(define-syntax (hyphen-define/wrong1.2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a" #'a #'b)))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))
; repl
; (hyphen-define/wrong1.2 foo bar () #t)
; (foo-bar)

(define-syntax (hyphen-define/ok1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'a)
                                                         (syntax->datum #'b))))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))
; repl
; (hyphen-define/ok1 foo bar () #t)
; (foo-bar)

(define-syntax (hyphen-define/wrong2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a (string->symbol (format "~a-~a" 'a 'b)))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))

; repl
; (hyphen-define/wrong2 foo bar () #t)
; (foo-bar)

; -------------------------------------------------------------------------------
; 4.1.1 with-syntax

; with-syntax is simply syntax-case rearranged:
; (syntax-case <syntax> () [<pattern> <body>])
; (with-syntax ([<pattern> <syntax>]) <body>)

(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax #'a
                                        (string->symbol (format "~a-~a"
                                                                (syntax->datum #'a)
                                                                (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))
; repl
; (hyphen-define/ok2 foo bar () #t)
; (foo-bar)

; -------------------------------------------------------------------------------
; 4.1.2 with-syntax*

(define-syntax (foo2 stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'c)]))

; -------------------------------------------------------------------------------
; 4.1.3 format-id

; using format-id to produce an identifier
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))

; -------------------------------------------------------------------------------
; 4.1.4 Another example

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let ([name-stxs (syntax->list #'(names ...))])
       (with-syntax ([name (datum->syntax (car name-stxs)
                                          (string->symbol
                                           (string-join (for/list ([name-stx name-stxs])
                                                          (symbol->string
                                                           (syntax-e name-stx)))
                                                        "-")))])
         #'(define (name args ...)
             body0 body ...)))]))

; repl
; (hyphen-define* (foo bar baz) (v) (* 2 v))
; (foo-bar-baz 50)

; -------------------------------------------------------------------------------
; 4.2 Making our own struct

(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ; Define a constructor.
           (define (id fields ...)
             (apply vector (cons 'id  (list fields ...))))
           ; Define a predicate.
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field.
           ; Note: #,@ (!!!)
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

; repl
(our-struct foo3 (a b))
(define s (foo3 1 2))
(check-true (foo3? s))
(check-false (foo3? 1))
(check-equal? (foo3-a s) 1)
(check-equal? (foo3-b s) 2)
(check-exn exn:fail? (lambda () (foo3-a "furble")))
; (our-struct "blah" ("blah" "blah"))

; add a guard/fender expression to our clause
(define-syntax (our-struct1 stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     ; Guard or "fender" expression:
     (for-each (λ(x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'id (syntax->list #'(fields ...))))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ; Define a constructor.
           (define (id fields ...)
             (apply vector (cons 'id  (list fields ...))))
           ; Define a predicate.
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field.
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))
; Now the same misuse gives a better error message:
; (our-struct1 "blah" ("blah" "blah"))

; -------------------------------------------------------------------------------
; 4.3 Using dot notation for nested hash lookups

; repl
(define js (hasheq 'a (hasheq 'b (hasheq 'c "value"))))
; (hash-ref (hash-ref (hash-ref js 'a) 'b) 'c)

(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                          [else def]))])
    (for/fold ([h h])
              ([k (in-list ks)])
      (hash-ref h k))))

; repl
; (hash-refs js '(a b c))

(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ; If the optional ‘default' is missing, use #f.
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))
; repl
; (hash.refs js.a.b.c)
; (hash.refs js.blah)
; (hash.refs js.blah 'does-not-exist)

; give helpful messages
(define-syntax (hash.refs1 stx)
  (syntax-case stx ()
    ; Check for no args at all
    [(_)
     (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx)]
    ; If the optional ‘default' is missing, use #f.
    [(_ chain)
     #'(hash.refs1 chain #f)]
    [(_ chain default)
     (unless (identifier? #'chain)
       (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain))
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
       ; Check that we have at least hash.key
       (unless (and (>= (length ids) 2)
                    (not (eq? (syntax-e (cadr ids)) '||)))
         (raise-syntax-error #f "Expected hash.key" stx #'chain))
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))

; repl
; (hash.refs1)
; (hash.refs1 0)
; (hash.refs1 js)
; (hash.refs1 js.)

; -------------------------------------------------------------------------------
; 5 Syntax parameters
; See www.schemeworkshop.org/2011/papers/Barzilay2011.pdf

(define-syntax-rule (aif condition true-expr false-expr)
  (let ([it condition])
    (if it
        true-expr
        false-expr)))
; repl
; (aif #t (displayln it) (void))

; Using syntax parameter to circumvent macro hygiene.
; Use syntax parameters using define-syntax-parameter and syntax-parameterize
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))

(define-syntax-rule (aif1 condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
          true-expr)
        false-expr)))

; repl
; (aif1 10 (displayln it) (void))
; (aif1 #f (displayln it) (void))

; -------------------------------------------------------------------------------
; 6 What’s the point of splicing-let?

; => let over lambda
(splicing-let ([x 0])
  (define (get-x)
    x))

; repl
; (get-x)
; x

(splicing-let ([x 0])
  (define (inc)
    (set! x (+ x 1)))
  (define (dec)
    (set! x (- 1 x)))
  (define (get)
    x))

; repl
; (get)
; (inc)
; (get)
; (dec)
; (get)

; The macro system you will mostly want to use for production-quality macros is called syntax-parse

; -------------------------------------------------------------------------------
; List of used syntax functions:

; define-syntax
; syntax
; #'
; syntax-source
; syntax-line
; syntax-column
; syntax->datum
; syntax-e
; syntax->list
; datum->syntax
; quasiquotation (https://bit.ly/2UXu1o2): `(... ,thingy ... ,@listy ...), #`(... #,thingy ... #,@listy)
; begin-for-syntax
; define-for-syntax
; syntax-case
; define-syntax-rule
; with-syntax
; with-syntax*
; define-syntax-parameter
; syntax-parameterize
