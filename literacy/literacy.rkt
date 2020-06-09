#lang racket

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)

(require scribble/core)
(require scribble/manual)
(require scribble/latex-properties)

(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

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

(define-syntax (ja-example-parse stx)
  (syntax-case stx []
    [(_ [[ja ...] [ruy ...] [en ...]])
     #'(list (list (list (let-values ([(token style) (ja-example->ruby-token 'ruy)])
                           (ruby (list (symbol->string 'ja)) (list token) #:style style)) ...))
             ;;; NOTE:
             ;; Rubies that displayed under kenjis do not contribute to the height of whole,
             ;;  an empty line is required to avoid overlapping.
             (list "")
             (list (string-join (map symbol->string '(|| en ...)))))]))

(define-syntax (ja-example stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~optional (~seq #:tag maybe-tag:id) #:defaults ([maybe-tag #'#false]))) ...
        [[ja ...] [ruy ...] [en ...]] ...)
     #'(make-tamer-indexed-traverse-block
        (λ [type chapter-index current-index]
          (define extag (symbol->string (or 'maybe-tag (gensym 'ex))))
          (define empty-row (list (list "")))
          (define example (format "Example ~a.~a" chapter-index current-index))

          (define example-rows
            (for/list ([example-row (in-list (list (ja-example-parse [[ja ...] [ruy ...] [en ...]]) ...))]
                       [sub (in-naturals 0)])
              (define subexample (string (integer->char (+ 97 sub))))
              (list (tabular #:sep (hspace 1)
                             #:style 'block
                             #:column-properties '(left center left)
                             (list (list ""
                                         (elemtag (string-append extag "." subexample) (tt subexample))
                                         (tabular #:style 'block example-row)))))))
          
          (values (string->symbol extag)
                  (tabular #:style 'block
                           #:column-properties '(left)
                           #:row-properties (append (list '(())) (make-list (add1 (length example-rows)) (list 'bottom-border)) (list '(())))
                           (append empty-row
                                   (list (list (elemtag extag (tt example))))
                                   example-rows
                                   empty-row))))
        'ex)]
    [(_ (~alt (~optional (~seq #:tag maybe-tag:id) #:defaults ([maybe-tag #'#false]))) ...
        [ja ...] [ruy ...] [en ...])
     #'(make-tamer-indexed-traverse-block
        (λ [type chapter-index current-index]
          (define extag (or 'maybe-tag (gensym 'ex)))
          (define empty-row (list (list "")))
          (define example (format "Example ~a.~a" chapter-index current-index))
          (define example-row (ja-example-parse [[ja ...] [ruy ...] [en ...]]))
          
          (values extag
                  (tabular #:style 'block
                           #:column-properties '(left)
                           #:row-properties '(() bottom-border () () 'bottom-border ())
                           (append empty-row
                                   (list (list (elemtag (symbol->string extag) (tt example))))
                                   example-row
                                   empty-row))))
        'ex)]))

(define-syntax (ja-exref stx)
  (syntax-parse stx #:datum-literals []
    [(_ extag:id)
     #'(make-tamer-indexed-elemref
        (λ [type chapter-index maybe-index]
          (elemref (symbol->string 'extag)
                   (subscript (format "~a~a.~a" type chapter-index (or maybe-index '?)))))
        'ex 'extag)]
    [(_ extag:id subtag:id)
     #'(make-tamer-indexed-elemref
        (λ [type chapter-index maybe-index]
          (elemref (format "~a.~a" 'extag 'subtag)
                   (subscript (format "~a~a.~a~a" type chapter-index (or maybe-index '?) 'subtag))))
        'ex 'extag)]))

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
