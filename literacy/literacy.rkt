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
     #'(tabular #:style 'block
                #:column-properties '(left)
                #:row-properties '(() top-border () bottom-border)
                (list (list "Example")
                      (list (ruby #:options (list "size=0.80")
                                  (list (symbol->string 'ja) ...)
                                  (list (ja-example-token->ruby 'ruy)
                                        ...)))
                      (list " ")
                      (list english)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ruby
  (lambda [base ruy #:options [options #false]]
    (map (λ [b r]
           (make-traverse-element
            (λ [get set]
              (cond [(not (handbook-latex-renderer? get)) (elem b)]
                    [(list? options) (make-multiarg-element (make-style "ruby" (list (make-command-optional (map ~a options)))) (list b r))]
                    [else (make-multiarg-element "ruby" (list b r))]))))
         (if (list? base) base (string-split base "|"))
         (if (list? ruy)  ruy  (string-split ruy  "|")))))

(define en-ruby
  (lambda [base ruy #:options [options #false]]
    (map (λ [b r]
           (make-traverse-element
            (λ [get set]
              (cond [(not (handbook-latex-renderer? get)) (elem b)]
                    [(list? options) (make-multiarg-element (make-style "enruby" (list (make-command-optional (map ~a options)))) (list b r))]
                    [else (make-multiarg-element "enruby" (list b r))]))))
         (if (list? base) base (string-split base "|"))
         (if (list? ruy)  ruy  (string-split ruy  "|")))))
    
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
(define ja-example-token->ruby
  (lambda [token]
    (define content (symbol->string token))

    (cond [(regexp-match? #px"\\w+" content) (tech (tt content))]
          [(eq? token '-) ""]
          [else content])))
