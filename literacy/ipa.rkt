#lang racket/base

(provide (all-defined-out))

(require "literacy.rkt")

(require racket/format)
(require racket/string)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define en-word
  (lambda [word0]
    (define word (~a word0))
    (define tokens (string-split word "/"))
    (define /? (and (> (string-length word) 0) (eq? (string-ref word 0) #\/)))
    (case (length tokens)
      [(3) (if (and /?) (list (en-emph (car tokens)) (cadr tokens) (en-emph (caddr tokens))) (list (car tokens) (en-emph (cadr tokens)) (caddr tokens)))]
      [(2) (if (and /?) (list (en-emph (car tokens)) (cadr tokens)) (list (car tokens) (en-emph (cadr tokens))))]
      [(1) (if (and /?) (en-emph word) word)]
      [else word])))

(define en-ruby
  (lambda [base ruy #:options [options #false] #:style [styles null]]
    (define bases (en-ruby-content base))
    (define rubies (en-ruby-content ruy))
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
          (define b (en-word b0))
          (define r (and (< i rsize) (list-ref rubies i)))
          (define s (if (< i ssize) (list-ref styles i) slast))
          
          (cond [(or (not r) (eq? r '-) (eq? r '||) (equal? r "") (equal? r "-")) b]
                [(not latex?) (list b (superscript (en-symbol r)))]
                [else (make-multiarg-element (if (pair? options) (make-style s (list (make-command-optional (map ~a options)))) s)
                                             (list b (en-symbol r)))]))
        " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define en-token
  (lambda [t]
    (cond [(box? t) (handbook-latex-command0 (~a (unbox t)))]
          [(list? t) (map en-token t)]
          [else (~a t)])))

(define en-sym
  (lambda [symbols]
    (IPA (en-token symbols))))

(define en-/sym/
  (lambda [symbols]
    (IPA (list "/" (en-token symbols) "/"))))

(define en-ruby-content
  (lambda [v]
    (cond [(list? v) v]
          [(string? v) (string-split v)]
          [else (list v)])))

(define en-emph
  (lambda [word]
    (litchar word)))

(define en-symbol
  (lambda [sym]
    (racketcommentfont (en-/sym/ sym))))
