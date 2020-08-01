#lang racket/base

(provide (all-defined-out))

(require "literacy.rkt")

(require racket/format)
(require racket/string)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-group-style (make-style #false (list (make-color-property "Crimson") (make-background-color-property "MistyRose"))))
(define ipa-doublebar-style (make-style "ipadoublebar" (list (make-color-property "DeepSkyBlue"))))
(define ipa-weak-style (make-style #false (list (make-color-property "DarkKhaki"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-ruby
  (lambda [base ruy #:options [options #false] #:style [styles null]]
    (define bases (ipa-ruby-content base))
    (define bimax (sub1 (length bases)))
    (define rubies (ipa-ruby-content ruy))
    (define rsize (length rubies))
    (define-values (ssize slast)
      (cond [(null? styles) (values 0 "iparuby")]
            [(list? styles) (values (length styles) (last styles))]
            [else (values 0 styles)]))

    (make-traverse-element
     (λ [get set!]
       (define latex? (handbook-latex-renderer? get))

       (for/list ([b0 (in-list bases)]
                  [i (in-naturals 0)])
         (define-values (b terminate?) (ipa-word b0))
         (define r (and (< i rsize) (list-ref rubies i)))
         (define s (if (< i ssize) (list-ref styles i) slast))
         (define-values (xheight? small-asc?) (ipa-word-xheight? (content->string b)))
         (define intergap (if (not xheight?) (if (not small-asc?) 0.2 0.3) 0.5))

         (define token-element
           (cond [(or (not r) (eq? r '-) (eq? r '||) (equal? r "") (equal? r "-")) b]
                 [(not latex?) (list b (superscript (ipa-symbol r)))]
                 [else (make-multiarg-element (if (pair? options) (make-style s (list (make-command-optional (map ~a options)))) s)
                                              (list b (ipa-symbol r) (number->string intergap)))]))
         
         (cond [(>= i bimax) token-element]
               [else (list token-element " ")]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-phonetics
  (lambda [word]
    (elem #:style ipa-group-style word)))

(define ipa-diacritic-element
  (lambda [word]
    (elem #:style "super" word)))

(define ipa-puncture-element
  (lambda [word]
    (elem #:style ipa-doublebar-style word)))

(define ipa-weak-element
  (lambda [word]
    (elem #:style ipa-weak-style word)))

(define ipa-symbol
  (lambda [sym]
    (racketcommentfont (ipa-/sym/ sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-word
  (lambda [word]
    (ipa-word-tokenize (~a word))))

(define ipa-sym
  (lambda [symbols]
    (define-values (s ?) (ipa-symbol-tokenize (~a symbols)))
    (IPA s)))

(define ipa-/sym/
  (lambda [symbols]
    (define-values (s ?) (ipa-symbol-tokenize (~a symbols)))
    (IPA (list "/" s "/"))))

(define ipa-ruby-content
  (lambda [v]
    (cond [(list? v) v]
          [(string? v) (string-split v)]
          [else (list v)])))

(define ipa-word-xheight?
  (lambda [word]
    (for/fold ([xheight? #true]
               [small? #true])
              ([ch (in-string (~a word))])
      (cond [(not (char-lower-case? ch)) (values (and (memq ch '(#\. #\, #\- #\space)) xheight?) small?)]
            [(memq ch '(#\b #\d #\f #\h #\k #\l)) (values #false #false)]
            [(memq ch '(#\i #\j #\t)) (values #false small?)]
            [else (values xheight? small?)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-symbol-tokenize
  (lambda [sym]
    (let tokenize ([chars (string->list sym)]
                   [nekot null]
                   [snekot null])
      (if (null? chars)
          (let ([last-token (list->string (reverse nekot))])
            (values (cond [(null? snekot) last-token]
                          [(string=? last-token "") (reverse snekot)]
                          [else (reverse (cons last-token snekot))])
                    #true))
          (let ([self (car chars)])
            (cond [(eq? self #\^) (tokenize (cdr chars) (cons #\" nekot) snekot)]
                  [(eq? self #\*) (tokenize (cdr chars) (cons #\@ nekot) snekot)]
                  [(eq? self #\.) (tokenize (cdr chars) (cons #\" (cons #\" nekot)) snekot)]
                  [(eq? self #\&) (let-values ([(token++ rest _) (ipa-chars-token++ chars #\$ handbook-latex-command0 nekot snekot)]) (tokenize rest null token++))]
                  [else (tokenize (cdr chars) (cons self nekot) snekot)]))))))

(define ipa-word-tokenize
  (lambda [word]
    (let tokenize ([chars (string->list word)]
                   [nekot null]
                   [snekot null]
                   [terminated? #true])
      (if (null? chars)
          (let ([last-token (list->string (reverse nekot))])
            (values (cond [(null? snekot) last-token]
                          [(string=? last-token "") (reverse snekot)]
                          [else (reverse (cons last-token snekot))])
                    terminated?))
          (let ([self (car chars)])
            (case self
              [(#\^) (tokenize (cdr chars) (cons #\' nekot) snekot terminated?)]
              [(#\=) (let-values ([(token++ rest ?) (ipa-chars-token++ chars ipa-puncture-element nekot snekot)]) (tokenize rest null token++ ?))]
              [(#\/) (let-values ([(token++ rest ?) (ipa-chars-token++ chars #\/ ipa-phonetics nekot snekot ipa-word-tokenize)]) (tokenize rest null token++ ?))]
              [(#\+) (let-values ([(token++ rest ?) (ipa-chars-token++ chars ipa-diacritic-element nekot snekot)]) (tokenize rest null token++ ?))]
              [(#\_) (let-values ([(token++ rest ?) (ipa-chars-token++ chars #\_ ipa-weak-element nekot snekot ipa-word-tokenize)]) (tokenize rest null token++ ?))]
              [else (tokenize (cdr chars) (cons self nekot) snekot terminated?)]))))))

(define ipa-chars-take
  (lambda [chars $ [make-element values] [subtake #false]]
    (define term-idx (index-of (cdr chars) $ eq?))
    (define ipa-size (or term-idx (sub1 (length chars))))
    (cond [(< ipa-size 1) (values #false null)]
          [else (let* ([word (list->string (take (cdr chars) ipa-size))])
                  (values (make-element (if (not subtake) word (let-values ([(e ?) (subtake word)]) e)))
                          (take-right chars (max (- (length chars) ipa-size 2) 0))
                          (and term-idx #true)))])))

(define ipa-chars-token++
  (case-lambda
    [(chars make-element nekot snekot)
     (define-values (maybe-token rest terminated?)
       (cond [(null? (cdr chars)) (values #false null #false)]
             [else (values (make-element (string (cadr chars))) (cddr chars) #true)]))
     (values (ipa-chars-token++ maybe-token nekot snekot) rest terminated?)]
    [(chars $ make-element nekot snekot)
     (ipa-chars-token++ chars $ make-element nekot snekot #false)]
    [(chars $ make-element nekot snekot subtake)
     (define-values (maybe-token rest terminated?) (ipa-chars-take chars $ make-element subtake))
     (values (ipa-chars-token++ maybe-token nekot snekot) rest terminated?)]
    [(maybe-token nekot snekot)
     (define prev-tokens
       (let ([prev-token (list->string (reverse nekot))])
         (cond [(string=? prev-token "") snekot]
               [else (cons prev-token snekot)])))
     
     (cond [(not maybe-token) prev-tokens]
           [else (cons maybe-token prev-tokens)])]))