#lang racket

(require (for-syntax racket/match racket/string racket/syntax))

(define-syntax foo
  (λ(stx)
    (syntax "I am foo")))

(define-syntax (also-foo stx)
  (syntax "I am also foo"))

(define-syntax (quoted-foo stx)
  #'"I am also foo, using #' instead of syntax")

(define-syntax (say-hi stx)
  #'(displayln "hi"))

(define-syntax (show-me stx)
  (print stx)
  #'(void))

(define stx #'(if x (list "true") #f))
(define stx1 #'(reverse-me "backwards" "am" "i" values))
(define stx2 #'(our-if-v2 #t "true" "false"))

(define-syntax (reverse-me stx)
  ;(print stx)
  ;(printf "\n")
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(define-syntax (foo1 stx)
  (make-pipe) ;Ce n'est pas le temps d'exécution
  #'(void))

(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))

(define (display-and-return x)
  (displayln x)
  x)

(define-syntax (our-if-using-match stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

; supply a "template", which uses variables from the pattern.
(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))

; define-syntax-rule is a shorthand for simple pattern-matching cases.
; define-syntax-rule expands to syntax-case.
(define-syntax-rule (our-if-using-syntax-rule condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

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

(define-syntax (hyphen-define/wrong1.2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax #'a
                                 (string->symbol (format "~a-~a" #'a #'b)))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))

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

; with-syntax is simply syntax-case rearranged:
;  (syntax-case <syntax> () [<pattern> <body>])
;  (with-syntax ([<pattern> <syntax>]) <body>)
(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (datum->syntax #'a
                                        (string->symbol (format "~a-~a"
                                                                (syntax->datum #'a)
                                                                (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))

(define-syntax (foo2 stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'c)]))

; using format-id to produce an identifier
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))

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
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

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

(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                          [else def]))])
    (for/fold ([h h])
              ([k (in-list ks)])
      (hash-ref h k))))

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