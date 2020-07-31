#lang racket/base

(provide (all-defined-out))

(require "literacy.rkt")

(require racket/format)
(require racket/string)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-word
  (lambda [word]
    (let tokenize ([chars (string->list (~a word))]
                   [nekot null]
                   [snekot null])
      (if (null? chars)
          (let ([last-token (list->string (reverse nekot))])
            (cond [(null? snekot) last-token]
                  [(string=? last-token "") (reverse snekot)]
                  [else (reverse (cons last-token snekot))]))
          (let ([self (car chars)])
            (cond [(eq? self #\^) (tokenize (cdr chars) (cons #\' nekot) snekot)]
                  [(eq? self #\+) (let-values ([(token++ rest) (ipa-chars-token++ chars ipa-diacritic-element nekot snekot)]) (tokenize rest null token++))]
                  [(eq? self #\-) (let-values ([(token++ rest) (ipa-chars-token++ chars ipa-puncture-element nekot snekot)]) (tokenize rest null token++))]
                  [(eq? self #\/) (let-values ([(token++ rest) (ipa-chars-token++ chars #\/ ipa-phonetics nekot snekot)]) (tokenize rest null token++))]
                  [else (tokenize (cdr chars) (cons self nekot) snekot)]))))))

(define ipa-ruby
  (lambda [base ruy #:options [options #false] #:style [styles null]]
    (define bases (ipa-ruby-content base))
    (define rubies (ipa-ruby-content ruy))
    (define rsize (length rubies))
    (define-values (ssize slast)
      (cond [(null? styles) (values 0 "iparuby")]
            [(list? styles) (values (length styles) (last styles))]
            [else (values 0 styles)]))

    (make-traverse-element
     (λ [get set!]
       (define latex? (handbook-latex-renderer? get))

       (add-between
        (for/list ([b0 (in-list bases)]
                   [i (in-naturals 0)])
          (define b (ipa-word b0))
          (define r (and (< i rsize) (list-ref rubies i)))
          (define s (if (< i ssize) (list-ref styles i) slast))
          (define intergap (if (ipa-word-no-ascenders? b0) 0.5 0.2))
          
          (cond [(or (not r) (eq? r '-) (eq? r '||) (equal? r "") (equal? r "-")) b]
                [(not latex?) (list b (superscript (ipa-symbol r)))]
                [else (make-multiarg-element (if (pair? options) (make-style s (list (make-command-optional (map ~a options)))) s)
                                             (list b (ipa-symbol r) (number->string intergap)))]))
        " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-phonetics
  (lambda [word]
    (litchar word)))

(define ipa-diacritic-element
  (lambda [word]
    (elem #:style "textsuperscript" (litchar word))))

(define ipa-puncture-element
  (lambda [word]
    (superscript word)))

(define ipa-symbol
  (lambda [sym]
    (racketcommentfont (ipa-/sym/ sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-sym
  (lambda [symbols]
    (IPA (ipa-symbol-tokenize symbols))))

(define ipa-/sym/
  (lambda [symbols]
    (IPA (list "/" (ipa-symbol-tokenize symbols) "/"))))

(define ipa-ruby-content
  (lambda [v]
    (cond [(list? v) v]
          [(string? v) (string-split v)]
          [else (list v)])))

(define ipa-word-no-ascenders?
  (lambda [word]
    (for/and ([ch (in-string (~a word))])
      (if (char-lower-case? ch)
          (not (memq ch '(#\b #\d #\f #\h #\i #\j #\k #\l #\t)))
          (memq ch '(#\. #\,))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-symbol-tokenize
  (lambda [sym]
    (let tokenize ([chars (string->list (~a sym))]
                   [nekot null]
                   [snekot null])
      (if (null? chars)
          (let ([last-token (list->string (reverse nekot))])
            (cond [(null? snekot) last-token]
                  [(string=? last-token "") (reverse snekot)]
                  [else (reverse (cons last-token snekot))]))
          (let ([self (car chars)])
            (cond [(eq? self #\^) (tokenize (cdr chars) (cons #\" nekot) snekot)]
                  [(eq? self #\.) (tokenize (cdr chars) (cons #\" (cons #\" nekot)) snekot)]
                  [(eq? self #\&) (let-values ([(token++ rest) (ipa-chars-token++ chars #\$ handbook-latex-command0 nekot snekot)]) (tokenize rest null token++))]
                  [else (tokenize (cdr chars) (cons self nekot) snekot)]))))))

(define ipa-chars-take
  (lambda [chars $ [string->element values]]
    (define ipa-size (or (index-of (cdr chars) $ eq?) (sub1 (length chars))))
    (cond [(< ipa-size 1) (values #false null)]
          [else (values (string->element (list->string (take (cdr chars) ipa-size)))
                        (take-right chars (max (- (length chars) ipa-size 2) 0)))])))

(define ipa-chars-token++
  (case-lambda
    [(chars string->element nekot snekot)
     (define-values (maybe-token rest)
       (cond [(null? (cdr chars)) (values #false null)]
             [else (values (string->element (string (cadr chars))) (cddr chars))]))
     (values (ipa-chars-token++ maybe-token nekot snekot) rest)]
    [(chars $ string->element nekot snekot)
     (define-values (maybe-token rest) (ipa-chars-take chars $ string->element))
     (values (ipa-chars-token++ maybe-token nekot snekot) rest)]
    [(maybe-token nekot snekot)
     (define prev-tokens
       (let ([prev-token (list->string (reverse nekot))])
         (cond [(string=? prev-token "") snekot]
               [else (cons prev-token snekot)])))
     
     (cond [(not maybe-token) prev-tokens]
           [else (cons maybe-token prev-tokens)])]))
