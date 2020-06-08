#lang racket

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (ja-thing stx)
  (syntax-case stx []
    [(_ ja-struct ja-type pre-flow ...)
     #'(defthing ja-struct ja-type
         pre-flow ...)]))

(define-syntax (ja-things stx)
  (syntax-case stx []
    [(_ [[ja-struct ja-type] ...] pre-flow ...)
     #'(deftogether
         [(ja-thing ja-struct ja-type) ...]
         pre-flow ...)]))

(define-syntax (ja-example stx)
  (syntax-case stx []
    [(_ [ja ...] [ruy ...] english)
     #'(nested
        (tabular #:style 'block
                 #:column-properties '(left)
                 #:row-properties '(bottom-border () 'bottom-border)
                 (list (list (commandline "Example"))
                       (list (list (let-values ([(token style) (ja-example->ruby-token 'ruy)])
                                         (ruby (list (symbol->string 'ja)) (list token) #:style style)) ...))
                       (list (hspace 1))
                       (list english))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ruby
  (lambda [base ruy #:options [options #false] #:style [style "ruby"]]
    (map (λ [b r]
           (make-traverse-element
            (λ [get set]
              (cond [(not (handbook-latex-renderer? get)) (elem b)]
                    [(list? options) (make-multiarg-element (make-style style (list (make-command-optional (map ~a options)))) (list b r))]
                    [else (make-multiarg-element style (list b r))]))))
         (ja-ruby-content base)
         (ja-ruby-content ruy))))
    
(define chinese
  (lambda [#:font [font "FandolSong"] . contents]
    (make-traverse-element
     (λ [get set]
       (if (handbook-latex-renderer? get)
           (make-multiarg-element "Chinese" (list font contents))
           contents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-title
  (lambda [en kenji hiragana]
    (list en ~ "|" ~
          (ruby kenji hiragana))))

(define ja-deftech
  (lambda [en kenji hiragana #:key [key #false]]
    (list (deftech en #:key key) ~
          "「" (ruby kenji hiragana) "」")))

(define ja-tech
  (lambda [en kenji hiragana #:key [key #false]]
    (list (tech #:key (or key en)
                en ~
                "「" (ruby kenji hiragana) "」"))))

(define ja-form
  (lambda contents
    (apply math contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ja-example->ruby-token
  (lambda [token]
    (define content (symbol->string token))

    (cond [(regexp-match? #px"[A-Z]+" content) (values (tech (tt content)) "exmptag")]
          [(eq? token '-) (values "" "ruby")]
          [else (values content "exmpruby")])))

(define ja-ruby-content
  (lambda [v]
    (cond [(string? v) (string-split v "|")]
          [(list? v) v]
          [else (list v)])))
